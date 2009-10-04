#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/iolib/strin.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.10
 | File mod date:    2007-01-28 10:04:24
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  iolib
 |
 | Purpose:          Implement string-input-ports
 `------------------------------------------------------------------------|#

(define-class <buffered-input-port> (<input-port>) :abstract
  (buffered-input-buffer type: <string> init-value: "")
  (buffered-input-posn type: <fixnum> init-value: 0))

;;;------------------------------------------------------------------------
;;;
;;;  a concrete subclass which provides no more input than what
;;;  it starts out with
;;;

(define-class <string-input-port> (<buffered-input-port>))

(define (open-input-string (source <string>))
  (make <string-input-port>
	input-port-line-number: 1
	buffered-input-buffer: source))

;;; these methods can be overridden in subclasses to allow more
;;; data to be supplied to the string as it runs out
;;; `provide-more-input' should return either a <string> or #f

(define-method provide-more-input ((self <string-input-port>))
  #f)

(define-method more-input-ready? ((self <string-input-port>))
  #f)

;;;

(define-method input-port-char-ready? ((self <buffered-input-port>))
  (or (fixnum<? (buffered-input-posn self)
		(string-length (buffered-input-buffer self)))
      (more-input-ready? self)))

(define-method input-port-read-char ((self <buffered-input-port>))
  (let (((contents <string>) (buffered-input-buffer self))
	((i <fixnum>) (buffered-input-posn self)))
    (if (fixnum<? i (string-length contents))
	(begin
	  (set-buffered-input-posn! self (add1 i))
	  (let ((ch (integer->ascii-char (bvec-ref contents i))))
            (if (eq? ch #\newline)
                (increment-line self))
            ch))
	(let ((more (provide-more-input self)))
	  (if (string? more)
	      (begin
		(set-buffered-input-buffer! self more)
		(set-buffered-input-posn! self 0)
		(input-port-read-char self))
	      $eof-object)))))

(define-method input-port-peek-char ((self <buffered-input-port>))
  (let (((contents <string>) (buffered-input-buffer self))
	((i <fixnum>) (buffered-input-posn self)))
    (if (fixnum<? i (string-length contents))
	(integer->ascii-char (bvec-ref contents i))
	(let ((more (provide-more-input self)))
	  (if (string? more)
	      (begin
		(set-buffered-input-buffer! self more)
		(set-buffered-input-posn! self 0)
		(input-port-peek-char self))
	      $eof-object)))))


(define-method input-port-read-line ((self <buffered-input-port>))
  (let* ((o (buffered-input-posn self))
	 (x (string-search (buffered-input-buffer self) #\newline o)))
    (if x
	(begin
	  (set-buffered-input-posn! self (+ x 1))
	  (substring (buffered-input-buffer self) o x))
	(next-method))))


(define-method collect ((self <buffered-input-port>) (more? <function>))
  (let loop (((i <fixnum>) (buffered-input-posn self))
	     ((n <fixnum>) (string-length (buffered-input-buffer self)))
	     ((str <string>) (buffered-input-buffer self))
	     (r '()))
    (if (fixnum<? i n)
	(let ((ch (integer->ascii-char (bvec-ref str i))))
	  (if (more? ch)
	      (if (eq? ch #\newline)
		  (begin
		    (increment-line self)
		    (loop (add1 i) n str (cons ch r)))
		  (loop (add1 i) n str (cons ch r)))
	      (begin
		(set-buffered-input-posn! self i)
		(reverse! r))))
	(let ((more (provide-more-input self)))
	  (if (string? more)
	      (begin
		(set-buffered-input-buffer! self more)
		(loop 0 (string-length more) more r))
	      (begin
		(set-buffered-input-posn! self i)
		(reverse! r)))))))
  
(define-method close-input-port ((self <buffered-input-port>))
  (set-buffered-input-buffer! self "")
  (set-buffered-input-posn! self 0))

;;;

(define-method port-position ((self <string-input-port>))
  (buffered-input-posn self))

(define-method set-port-position! ((self <string-input-port>) posn)
  (if (or (< posn 0) (> posn (string-length (buffered-input-buffer self))))
      (error "set-port-position!: Position `~s' is out of range" posn))
  (set-buffered-input-posn! self posn))
