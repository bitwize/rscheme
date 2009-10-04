#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/iolib/input.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.13
 | File mod date:    2004-07-02 07:50:33
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  iolib
 |
 | Purpose:          Generic <input-port> interface
 `------------------------------------------------------------------------|#

;;
;;  general input stuff
;;

(define-syntax (increment-line self)
  (set-input-port-line-number! self (add1 (input-port-line-number self))))

;;

(define-class <eof> (<object>))

(%early-once-only
(define $eof-object (make <eof>))
)

(define (eof-object? thing)
  (eq? thing $eof-object))

(define (eof-object)
  $eof-object)

;; these generic functions define the basic
;; functionality.
;;
;; there is no default for the first two, but
;; the higher-level functions
;; have defaults which are written in terms of read-char

(define-generic-function input-port-read-char)
(define-generic-function input-port-peek-char)
(define-generic-function close-input-port)

(define-generic-function input-port-scan-token)
(define-generic-function input-port-read)
(define-generic-function input-port-read-line)
(define-generic-function input-port-read-len)  ;; these two underly the
(define-generic-function input-port-read-rest) ;; `read-string' API
(define-generic-function input-port-read-max)

(define-generic-function set-input-port-prompt!)
(define-generic-function set-input-port-completions!)

(define-macro (define-input-proc fn)
  (let ((using (symbol-append "input-port-" fn)))
    `(define-inline ,fn
       (nlambda
	(() (,using (current-input-port)))
	((port) (,using port))))))

(define-method input-port-read-line ((self <input-port>))
  (let (((first <pair>) (cons 0 '())))
    (let loop (((prev <pair>) first))
      (let ((ch (input-port-read-char self)))
	(if (eq? ch $eof-object)
	    (if (eq? prev first)
		$eof-object
		(list->string (cdr first)))
	    (if (eq? ch #\newline)
		(list->string (cdr first))
		(let (((cell <pair>) (cons ch '())))
		  (set-cdr! prev cell)
		  (loop cell))))))))
;; it's worth noting that being at EOF means a char is ALWAYS
;; ready (because read-char will never block, because it will
;; immediately return $eof-object)

(define-method input-port-char-ready? ((self <input-port>))
  #t)

;;;

(define-input-proc read-char)
(define-input-proc char-ready?)
(define-input-proc peek-char)
(define-input-proc scan-token)
(define-input-proc read-line)
(define-input-proc read)

(define (skip-whitespace port)
  (let flush-whitespace-loop ()
    (if (and (char-ready? port)
             (char-whitespace? (peek-char port)))
        (begin
          (read-char port)
          (flush-whitespace-loop))
        (values))))

;;;

(define-method parse-using-grammar ((self <string>) (grammar <grammar>))
  (parse-using-grammar (open-input-string self) grammar))

(define-method parse-using-grammar ((self <input-port>) (grammar <grammar>))
  (parse-using-grammar
   (lambda ()
     (let ((ch (read-char self)))
       (if (eof-object? ch)
           #f
           ch)))
   grammar))

