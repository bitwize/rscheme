#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/iolib/conven.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.5
 | File mod date:    1998-01-03 19:07:28
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  iolib
 |
 | Purpose:          I/O convenience functions
 `------------------------------------------------------------------------|#

(define (with-output-to-port port thunk)
  (thread-let ((*output-port* port))
    (thunk)))

(define (with-input-from-port port thunk)
  (thread-let ((*input-port* port))
    (thunk)))

; variants for accessing files...

(define (call-with-output-file string proc)
  (let ((file (open-output-file string)))
    (let ((result (values->list (proc file))))
      (close-output-port file)
      (list->values result))))

(define (with-output-to-file string thunk)
  (let ((file (open-output-file string)))
    (let ((result (values->list (with-output-to-port file thunk))))
      (close-output-port file)
      (list->values result))))


(define (call-with-input-file string proc)
  (let ((file (open-input-file string)))
    (let ((result (values->list (proc file))))
      (close-input-port file)
      (list->values result))))

(define (with-input-from-file string thunk)
  (let ((file (open-input-file string)))
    (let ((result (values->list (with-input-from-port file thunk))))
      (close-input-port file)
      (list->values result))))

;;
;; variants for accessing strings...
;;

(define (with-input-from-string string thunk)
  (let ((port (open-input-string string)))
    (let ((result (values->list (with-input-from-port port thunk))))
      (close-input-port port)
      (list->values result))))

;; these are a little different than the usual, which return
;; the return value of the given procedure, in that it
;; returns the resulting string instead

(define (call-with-output-string proc)
  (let ((port (open-output-string)))
    (proc port)
    (close-output-port port)))

(define (with-output-to-string thunk)
  (let ((port (open-output-string)))
    (with-output-to-port port thunk)
    (close-output-port port)))

; for-each for ports

(define (with-objects-from-port port proc)
  (let loop ((n 0))
    (let ((item (read port)))
      (if (eof-object? item)
	  n
	  (begin
	    (proc item)
	    (loop (+ n 1)))))))

(define (with-objects-from-file file proc)
  (let ((port (open-input-file file)))
    (let ((n (with-objects-from-port port proc)))
      (close-input-port port)
      n)))
