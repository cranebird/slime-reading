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
(use srfi-19)
(use gauche.uvector)
(use gauche.vport)
(use gauche.parameter)
(use gauche.logger)

;; (define log-port (current-error-port))

(define nil '())
(define t 't)


;;;;; Conditions
;; TODO not used
(define-condition-type <swank-error> <error>
  swank-error?
  (debug-info)
  (reason))


;;;;; Utility

(define write-to-string/ss
  (lambda (s)
    (write-to-string s write/ss)))


;;;;; Input

;; Read an S-expression from STREAM using the SLIME protocol.
(define decode-message
  (lambda (stream)
    (read-message stream "dummy")))

(define encode-message
  (lambda (message stream)
    (let1 string (write-to-string/ss message)
      (format stream "~6,'0x" (string-length string))
      (display string stream)
      (flush stream))))

;; read and return s-expression
(define read-message
  (lambda (stream package)
    (let1 packet (read-packet stream)
      (read-form packet package))))

;; read a form
(define read-form
  (lambda (string package)
    (guard (exc
            ((<read-error> exc)
             (log-format "read-form error: ~a~%" string)
             (raise exc)))
           (read-from-string string))))

;; parse header of packet and return packet length.
;; return #f if invalid format.
(define packet-header-length 6)
(define parse-header
  (lambda (stream)
    (string->number (read-chunk stream packet-header-length) 16)))

;; read chunk from stream
(define read-chunk
  (lambda (stream len)
    (let1 buffer (read-block len stream)
      (cond
       ((eof-object? buffer) (error "read-chunk eof!"))
       ((= (string-length buffer) len) buffer)
       (else (error "read-chunk error!"))))))

;; read packet from stream and return string.
(define read-packet
  (lambda (stream)
    (let1 len (parse-header stream)
      (if (number? len)
          (read-chunk stream len)
          (errorf "Invalid packet header: ~a" len)))))

(define make-swank-socket
  (lambda (port)
    (make-server-socket 'inet port :reuse-addr? #t)))

(define call-with-swank-socket
  (lambda (port proc)
    (let1 sock #f
      (unwind-protect
       (begin
         (set! sock (make-swank-socket port))
         (log-format "start swank on port ~a~%" port)
         (proc sock))
       (if sock
           (begin
             (log-format "closing port ~a...~%" port)
             (socket-close sock))
           (log-format "sock is #f~%"))))))

(define-class <emacs-connection> ()
  ((server      :init-keyword :server :accessor server-of)
   (client      :init-keyword :client :accessor client-of)
   (user-input  :init-keyword :input :accessor input-of)
   (user-output :init-keyword :output :accessor output-of)))

(define make-emacs-connection
  (lambda (sock)
    (let* ((client (socket-accept sock))
           (input  (socket-input-port client :buffering #f))
           (output (socket-output-port client)))
      (make <emacs-connection>
        :server sock :client client :output output :input input))))

;;; elisp
;; (put 'call-with-emacs-connection 'scheme-indent-function 1)
(define call-with-emacs-connection
  (lambda (port proc)
    (let1 conn #f
      (unwind-protect
       (begin
         (call-with-swank-socket port
           (lambda (sock)
             (log-format "open emacs-connection...~%")
             (set! conn (make-emacs-connection sock))
             (proc conn))))
       (begin
         (if conn
             (begin
               (log-format "close emacs-connection...~%")
               ;; do clean up here
               )
             (log-format "emacs-connection is #f~%")))))))

(define dispatch-event
  (lambda (conn event)
    (log-format "dispatch-event: ~s~%" event)
    (match event
      ((:emacs-rex form package thread-id id)
       (let1 result (eval-for-emacs conn form package id)
         (send-to-emacs `(:return (:ok ,result) ,id) (output-of conn))))
      (_
       (log-format "Unknown event: ~s~%" event)
       (errorf <swank-error> "dispatch-evnet: Unknown event ~a" event)))))

;; emacs output stream
(define make-emacs-output-stream
  (lambda (output)
    (make <buffered-output-port>
      :flush (lambda (u8v flag)
               (write-string (u8vector->string u8v) output)
               (u8vector-length u8v)))))

;; FIXME bad name
(define write-string
  (lambda (message output)
    (send-to-emacs `(:write-string ,message) output)))

;;(put 'call-with-emacs-output-stream 'scheme-indent-function 1)
(define call-with-emacs-output-stream
  (lambda (output proc)
    (let1 emacs-output #f
      (unwind-protect
       (begin
         (set! emacs-output (make-emacs-output-stream output))
         (proc emacs-output))
       (when emacs-output
         (flush emacs-output)
         (close-output-port emacs-output))))))

;; (define call-with-emacs-output-stream
;;   (lambda (output proc)
;;     (let1 emacs-output (make-emacs-output-stream output)
;;       (dynamic-wind
;;         (lambda () #f)
;;         (lambda () (proc emacs-output))
;;         (lambda ()
;;           (flush emacs-output)
;;           (close-output-port emacs-output))))))

;; make evaluator
(define evaluator
  (lambda (conn)
    (lambda (expr env)
      (let1 output (output-of conn)
        (call-with-emacs-output-stream output
          (lambda (emacs-output)
            (with-output-to-port emacs-output
              (lambda () (eval expr env)))))))))

(define eval-for-emacs
  (lambda (conn form package id)
    ;; FIXME introduce user environment
    ;; FIXME user input
    (let1 result ((evaluator conn) form (interaction-environment))
      (log-format "eval-for-emacs form: ~a~%" form)
      (log-format "eval-for-emacs result: ~a~%" result)
      result)))

;; Send EVENT to Emacs.
(define send-to-emacs
  (lambda (event output)
    (encode-message event output)))

;;; The `DEFSLIMEFUN' macro defines a function that Emacs can call via RPC.
;; A DEFINE for functions that Emacs can call by RPC.
(define-macro (defslimefun name params :rest bodys)
  `(define (,name ,@params) ,@bodys))

;; TODO
(defslimefun swank:connection-info ()
  `(:pid ,(sys-getpid)
	 :package (:name "user" :prompt "user")
	 :encoding (:coding-systems ("utf-8-unix"))
	 :features '()
	 :modules ,(all-modules->string-list)
	 :lisp-implementation (:type "Gauche" :version ,(gauche-version))
         ;; check slime version.
	 :version "2013-05-26"))

(define all-modules->string-list
  (lambda ()
    (map (lambda (m) (symbol->string (module-name m))) (all-modules))))

(defslimefun swank:swank-require (modules :optional filename)
  ;; FIXME what is MODULES?
  (all-modules->string-list))

;; TODO
(defslimefun swank:create-repl (target . opts)
  (list "user" "user"))

(defslimefun swank:listener-eval (string)
  ;; FIXME use eval-repl and multiple-value
  (log-format "listener-eval: ~a~%" string)
  (let1 sexp (read-from-string string)
    ;; FIXME read error check
    (log-format "listener-eval: sexp ~a~%" sexp)
    (call-with-values (lambda () (eval sexp (interaction-environment)))
      (lambda result
        `(:values . ,(map write-to-string/ss result))))))

;; TODO
(defslimefun swank:autodoc (raw-form
                            :key 
                            (print-right-margin #f)
                            (print-lines #f))
  '("" nil))

(defslimefun quit-lisp () (exit))

(define make-swank-drain
  (lambda ()
    (make <log-drain> :program-name "swank"
          :path #t
          :prefix (lambda (x)
                    (let ((d (current-date)))
                      (format #f "~a:~a:~a "
                              (date-hour d)
                              (date-minute d)
                              (date-second d)))))))

;;;; main
(define swank-server
  (lambda (port)
    (log-default-drain (make-swank-drain))
    (log-format "start!~%")
    (unwind-protect
     (call-with-emacs-connection port
       (lambda (conn)
         (let loop ()
           (let1 event (decode-message (input-of conn))
             (log-format "server: ~a~%" (socket-status (server-of conn)))
             (log-format "event: ~a~%" event)
             ;; TODO use event queue
             (dispatch-event conn event)
             (loop)))))
     (log-format "exit loop~%"))))

;; (define (swank-server port)
;;   (log-default-drain (make-swank-drain))
;;   (with-emacs-connection port conn
;;     (let loop ()
;;       (let1 event (decode-message (input-of conn))
;;         (log-format "event: ~a~%" event)
;;          ;; TODO use event queue
;;         (dispatch-event conn event)
;;         (loop)))))

;; see lib/gauche/interactive.scm
;; TODO fix call with-emacs-connection
;; (define (repl port)
;;   (log-default-drain (make-swank-drain))
;;   (with-emacs-connection port conn
;;     (let ((reader (lambda () (decode-message (input-of conn))))
;;           (evaluator (lambda (exp module)
;;                        (dispatch-event conn exp)))
;;           (printer (lambda vals (for-each
;;                                  (^e (write/ss e)
;;                                      (newline)) vals)))
;;           (prompter (lambda () (display "swank> ") (flush))))
;;       ((with-module gauche read-eval-print-loop)
;;        reader evaluator printer prompter))))

;;;; see src/libeval.scm
;; (define (repl port)
;;   (log-default-drain (make-swank-drain))
;;   (with-emacs-connection port conn
;;     (let ((reader (lambda () (decode-message (input-of conn))))
;;           (evaluator (lambda (exp module)
;;                        (dispatch-event conn exp)))
;;           (printer (lambda vals (for-each
;;                                  (^e (write/ss e)
;;                                      (newline)) vals)))
;;           (prompter (lambda () (display "repl> ") (flush))))
;;       (let loop1 ()
;;         (guard
;;          (e [else
;;              (report-error e)
;;              (log-format "error: ~a~%" e)
;;              #f])
;;          (let loop2 ()
;;            (prompter)
;;            (let1 exp (reader)
;;              (if (not (eof-object? exp))
;;                  (receive results (evaluator exp (current-module))
;;                    (apply printer results)
;;                    (loop2))))))
;;         (loop1)))))
