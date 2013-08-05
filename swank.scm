;;;;
;; swank.scm
;;
;; gauche:
;; (load "./swank.scm")
;; (swank-server 5040)

;; emacs:
;; (defun swank-test ()
;;   (interactive)
;;   (slime-connect "127.0.0.1" 5040))
;; M-x (swank-test)

;; TODO define-module
(use gauche.net)
(use gauche.selector)
(use text.parse)
(use util.match)
(use srfi-13)
(use gauche.uvector)
(use gauche.vport)
(use gauche.parameter)

(define log-port (current-error-port))
(define nil '())
(define t 't)


;;;;; Conditions
(define-condition-type <swank-error> <error>
  swank-error?
  (debug-info swank-error-debug-info)
  (reason swank-error-reason))

(define (make-return-event ok result id)
  (if ok
      `(:return (:ok ,result) ,id)
      `(:return (:abort "abort!") ,id)))


;;;;; Utility

(define (write-to-string/ss s) (write-to-string s write/ss))


;;;;; Input

;; Read an S-expression from STREAM using the SLIME protocol.
(define (decode-message stream)
  (read-message stream "dummy"))

(define (encode-message message stream)
  (let ((string (write-to-string/ss message)))
    (format stream "~6,'0x" (string-length string))
    (display string stream)
    (flush stream)))

;; read and return s-expression
(define (read-message stream package)
  (let ((packet (read-packet stream)))
    (read-form packet package)))

;; read a form
(define (read-form string package)
  (guard (exc
          [(condition-has-type? exc <read-error>)
           (format log-port "read-form error")
           'read-error]
          [else 'other-error])
         (read-from-string string)))

;; parse header of packet and return packet length.
;; return #f if invalid format.
(define (parse-header stream)
  (string->number (read-chunk stream 6) 16))

;; read chunk from stream
(define (read-chunk stream len)
  (let1 buffer (read-block len stream)
    (cond
     ((eof-object? buffer)
      (error "read-chunk eof!"))
     ((= (string-length buffer) len)
      buffer)
     (else
      (error "read-chunk error!")))))

;; read packet from stream and return string.
(define (read-packet stream)
  (let ((len (parse-header stream)))
    (if (number? len)
        (read-chunk stream len)
        (errorf "Invalid packet header"))))

;;
(define (make-swank-socket port)
  (make-server-socket 'inet port :reuse-addr? #t))

(define (accept-handler server)
  (let* ((client (socket-accept server))
         (output (socket-output-port client))
         (input  (socket-input-port client :buffering #f)))
    (format log-port "Client Start~%")
    (swank-receive client input output)
    (format log-port "Client Closed~%")))

(define (swank-receive client input output)
  (let loop ((event (decode-message input)))
    (format log-port ";; swank-receive event ~a~%" event)
    ;; TODO use event queue
    (dispatch-event event input output)
    (loop (decode-message input))))

(define (dispatch-event event input output)
  (format log-port ";; dispatch-event: ~s~%" event)
  (match event
    ((:emacs-rex form package thread-id id) 
     ;; TODO introduce thread
     (eval-for-emacs form package id input output))
    (_
     (format log-port ";; Unknown event: ~s~%" event))))

(define (make-return-event ok result id)
  (if ok
      `(:return (:ok ,result) ,id)
      `(:return (:abort "abort!") ,id)))

;; emacs output stream
(define (make-emacs-output-stream output)
  (make <buffered-output-port>
    :flush (lambda (u8v flag)
             (write-string (u8vector->string u8v) output)
             (u8vector-length u8v))))

(define (write-string message output)
  (send-to-emacs `(:write-string ,message) output))

(define (eval-for-emacs form package id input output)
  ;; FIXME introduce user environment
  (let ((emacs-output (make-emacs-output-stream output)))
    (dynamic-wind
        (lambda () #f)
        (lambda ()
          (with-output-to-port emacs-output
            (lambda ()
              (let1 result (eval form (interaction-environment))
                  (format log-port ";; eval-for-emacs form: ~a ;; result: ~a~%"
                          form result)
                  (send-to-emacs (make-return-event #t result id) output)))))
        (lambda ()
          (flush emacs-output)
          (close-output-port emacs-output)))))

;; (define (current-socket-io)
;;   (socket-output-port (ref 'socket *connection*)

;; "Send EVENT to Emacs."
(define (send-to-emacs event output)
  ;; TODO event driven
  (encode-message event output))

;;; The `DEFSLIMEFUN' macro defines a function that Emacs can call via RPC.
;; A DEFINE for functions that Emacs can call by RPC.
(define-macro (defslimefun name params :rest bodys)
  `(define (,name ,@params) ,@bodys))

(defslimefun swank:connection-info ()
  `(:pid ,(sys-getpid)
	 :package (:name "user" :prompt "user")
	 :encoding (:coding-systems ("utf-8-unix"))
	 :features '()
	 :modules ,(all-modules->string-list)
	 :lisp-implementation (:type "Gauche" :version ,(gauche-version))
	 :version "2013-05-26" ;; slime version.
         ))

(define (all-modules->string-list)
  (map (lambda (m) (symbol->string (module-name m))) (all-modules)))

(defslimefun swank:swank-require (modules :optional filename)
  ;; FIXME what modules?
  (all-modules->string-list))

;; FIXME
(defslimefun swank:create-repl (target . opts)
  (list "user" "user"))

(define (make-values-event result)
  )

;; FIXME
(defslimefun swank:listener-eval (string)
  ;; FIXME use eval-repl and multiple-value
  (format log-port "listener-eval: ~a~%" string)
  (let ((sexp (read-from-string string)))
    (format log-port "listener-eval: sexp ~a~%" sexp)
    (let ((result (eval sexp  (interaction-environment))))
      `(:values ,(write-to-string/ss result)))))

;; FIXME
(defslimefun swank:autodoc (raw-form :key 
                                     (print-right-margin #f)
                                     (print-lines #f))
  '("" nil))

;; <slime-connection>
;; socket 
;; (define-class <slime-connection> ()
;;   ((socket :init-keyword :socket)))

;;(define *connection* #f)

(define (swank-server port)
  (let* ((server (make-swank-socket port))
         ;;(connection (make <slime-connection> :socket server))
         )
    (dynamic-wind
        (lambda () #f)
        (lambda () (accept-handler server))
        (lambda ()
          (format log-port ";; Close Listening on port ~a" port)
          (socket-close server)))))


      
