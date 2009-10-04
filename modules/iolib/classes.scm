#|------------------------------------------------------------*-Scheme-*--|
 | File:	    modules/iolib/classes.scm
 |
 |          Copyright (C)1998 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |	    See <http://www.rscheme.org/> for the latest info.
 |
 | File version:     1.9
 | File mod date:    2005-10-12 20:01:33
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  iolib
 |
 | Purpose:          Class definitions for I/O ports that are needed early 
 |		     on in the compilation process
 `------------------------------------------------------------------------|#

(define-class <io-port> (<object>) :abstract)

(define-method name ((self <io-port>))
  #f)

(define-method write-object ((self <io-port>) port)
  (let ((n (name self))
	(c (class-name (object-class self))))
    (if n
	(format port "#[~a ~a]" c n)
	(format port "#[~a]" c))))

;;;

(define-class <io-error> (<condition>) :abstract
  (io-error-msg type: <string>)
  (io-error-args type: <list> init-value: '()))

(define-class <io-open-error> (<io-error>))
  
(define-class <io-active-error> (<io-error>)
  (on-port type: <io-port>))

(define-class <io-port-is-closed-error> (<io-active-error>))
(define-class <io-parse-error> (<io-active-error>))
(define-class <io-scan-error> (<io-active-error>))

;;;

(define-class <input-port> (<io-port>)
  (input-port-line-number type: <fixnum> init-value: -1)) ;; ie, nonsense

(define (input-port? x)
  (instance? x <input-port>))

(define-class <std-input-port> (<input-port>)
  (name type: <string>)
  file-stream)

;;;

(define-class <output-port> (<io-port>))

(define (output-port? x)
  (instance? x <output-port>))

(define-class <std-output-port> (<output-port>)
  (name type: <string>)
  file-stream)

;;;

;;;

(define (signal-port-is-closed (port <io-port>) in)
  (error
   (make <io-port-is-closed-error>
         on-port: port
         io-error-msg: (if (name port)
                           (if (input-port? port)
                               "~a: input port is closed in ~a"
                               "~a: output port is closed in ~a")
                           (if (input-port? port)
                               "input port is closed in ~a"
                               "output port is closed in ~a"))
         io-error-args: (let ((in (cons in '())))
                          (if (name port) 
                              (cons (name port) in)
                              in)))))

;;;

(define-generic-function underlying-input-port)  ; work around bug per CR 713
(define-generic-function underlying-output-port)      ; ''
(define-generic-function set-underlying-input-port!)  ; ''
(define-generic-function set-underlying-output-port!) ; ''

;;;

(define-generic-function port-position)
(define-generic-function set-port-position!)

