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
          ((<read-error> exc)
           (log-format "read-form error: ~a~%" string)
           (raise exc)))
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

(define (make-swank-socket port)
  (make-server-socket 'inet port :reuse-addr? #t))

;; (define-macro (with-swank-socket sock port thunk)
;;   `(let ((,sock (make-swank-socket ,port)))
;;      (log-format "start swank on port ~a~%" port)
;;      (dynamic-wind
;;          (lambda () #f)
;;          ,thunk
;;          (lambda ()
;;            (log-format "close port ~a~%" ,port)
;;            (socket-close ,sock)))))

(define (with-swank-socket port fn)
  (let ((sock (make-swank-socket port)))
    (log-format "start swank on port ~a~%" port)
    (dynamic-wind
        (lambda () #f)
        (fn sock)
        (lambda ()
          (log-format "close port ~a~%" port)
          (socket-close sock)))))

(define-class <emacs-connection> ()
  ((server :init-keyword :server :accessor server-of)
   (client :init-keyword :client :accessor client-of)
   (user-input :init-keyword :input :accessor input-of)
   (user-output :init-keyword :output :accessor output-of)))

;; (define emacs-connection (make-parameter #f))

(define (with-emacs-connection port fn)
  (let ((client (gensym))
        (output (gensym))
        (input (gensym)))
    (with-swank-socket port
                       (lambda (sock)
                         (let* ((client (socket-accept sock))
                                (input (socket-input-port client :buffering #f))
                                (output (socket-output-port client)))
                           (let ((conn
                                  (make <emacs-connection>
                                    :server sock :client client
                                    :output output :input input)))
                             (log-format "connection Opend~%")
                             (fn conn)
                             (log-format "connection Closed~%")))))))

(define (dispatch-event conn event)
  (log-format "dispatch-event: ~s~%" event)
  (match event
    ((:emacs-rex form package thread-id id)
     (let1 result (eval-for-emacs conn form package id)
       (send-to-emacs `(:return (:ok ,result) ,id) (output-of conn))))
    (_
     (log-format "Unknown event: ~s~%" event)
     (errorf <swank-error> "dispatch-evnet: Unknown event ~a" event))))

;; emacs output stream
(define (make-emacs-output-stream output)
  (make <buffered-output-port>
    :flush (lambda (u8v flag)
             (write-string (u8vector->string u8v) output)
             (u8vector-length u8v))))

;; FIXME bad name
(define (write-string message output)
  (send-to-emacs `(:write-string ,message) output))

(define (with-emacs-output-stream output fn)
  (let ((emacs-output (make-emacs-output-stream output)))
    (dynamic-wind
        (lambda () #f)
        (lambda () (fn emacs-output))
        (lambda ()
          (flush emacs-output)
          (close-output-port emacs-output)))))

;; make evaluator
(define (evaluator conn)
  (lambda (expr env)
    (let ((output (output-of conn)))
      (with-emacs-output-stream output
          (lambda (emacs-output)
            (with-output-to-port emacs-output
              (lambda ()
                (eval expr env))))))))

(define (eval-for-emacs conn form package id)
  ;; FIXME introduce user environment
  ;; FIXME user input
  (let1 result ((evaluator conn) form (interaction-environment))
    (log-format "eval-for-emacs form: ~a~%" form)
    (log-format "eval-for-emacs result: ~a~%" result)
    result))

;; Send EVENT to Emacs.
(define (send-to-emacs event output)
  (encode-message event output))

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

(define (all-modules->string-list)
  (map (lambda (m) (symbol->string (module-name m))) (all-modules)))

(defslimefun swank:swank-require (modules :optional filename)
  ;; FIXME what is MODULES?
  (all-modules->string-list))

;; TODO
(defslimefun swank:create-repl (target . opts)
  (list "user" "user"))

(defslimefun swank:listener-eval (string)
  ;; FIXME use eval-repl and multiple-value
  (log-format "listener-eval: ~a~%" string)
  (let ((sexp (read-from-string string)))
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

(define (make-swank-drain)
  (make <log-drain> :program-name "swank"
        :path #t :prefix (lambda (x)
                           (let ((d (current-date)))
                             (format #f "~a:~a:~a "
                                     (date-hour d)
                                     (date-minute d)
                                     (date-second d))))))

;;;; main
(define (swank-server port)
  (log-default-drain (make-swank-drain))
  (log-format "start!~%")
  (with-emacs-connection port
      (lambda (conn)
        (let loop ()
          (let1 event (decode-message (input-of conn))
            (log-format "event: ~a~%" event)
            ;; TODO use event queue
            (dispatch-event conn event)
            (loop))))))

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
