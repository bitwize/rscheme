#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/iolib/output.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.8
 | File mod date:    2003-11-04 15:55:26
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  iolib
 |
 | Purpose:          general <output-port> interface
 `------------------------------------------------------------------------|#

;;
;;  generic output stuff
;;


;; idea for a trick:
;;   have a class <closed-output-port> which we
;;   reset the class of a port to when it gets closed
;;   that way, subclasses don't have to continually be
;;   checking for whether or not they're closed
;;   (may fail in the case where we cache, either explicitly
;;   with find-method, or implicitly in the compiler's decision
;;   tree, the class of an instance.  may not be worth
;;   violating the maxim that the class of an object is constant)
;;

(define-generic-function output-port-write-char)
(define-generic-function close-output-port)
(define-generic-function flush-output-port)

(define-generic-function print)

;; suitable defaults..

(define-generic-function write-string)
(define-generic-function write-int)
(define-generic-function write-markup)
(define-generic-function call-with-markup)

(define-method close-output-port ((self <output-port>))
  (values))

(define-method flush-output-port ((self <output-port>))
  (values))

(define-method call-with-markup ((self <output-port>) markup thunk)
  (dynamic-wind
      (lambda ()
        (write-markup self (cons 'start markup)))
      thunk
      (lambda ()
        (write-markup self (cons 'end markup)))))

(define-method write-markup ((self <output-port>) markup)
  (values))     ; discard markup

(define-method write-string ((self <output-port>) (str <string>))
  (let (((n <fixnum>) (string-length str)))
    (let loop (((i <fixnum>) 0))
      (if (fixnum<? i n)
	  (begin
	    (output-port-write-char self (string-ref str i))
	    (loop (add1 i)))))))

(define-method write-int ((self <output-port>) (int <fixnum>))
  (write-string self (number->string int)))

;;
;; optional-arg'd functions
;;

(define-macro (define-with-implicit-output-port decl . body)
  (let ((fn (car decl))
	(args (cdr decl)))
    `(define-inline ,fn
       (nlambda
	((,@args port)
	 ,@body)
	((,@args)
	 (let-syntax ((port (else (current-output-port))))
	   ,@body))))))

;;;



(define-with-implicit-output-port (write-char ch)
  (output-port-write-char port ch))

(define-with-implicit-output-port (newline)
  (output-port-write-char port #\newline))

(define-with-implicit-output-port (write item)
  (write-object item port))

(define-with-implicit-output-port (display item)
  (display-object item port))
