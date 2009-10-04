#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/iolib/stdout.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.13
 | File mod date:    2006-01-28 16:50:06
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  iolib
 |
 | Purpose:          output through stdio, <std-output-port>
 `------------------------------------------------------------------------|#

;;
;;  an <output-port> using stdio
;;

(define-class <output-pipe-port> (<std-output-port>))

(define (open-output-file path)
  (let ((f (fopen (relative-file path) "w")))
    (if (not f)
	(error "open-output-file: open of `~a' failed" path))
    (make <std-output-port>
	  name: path
	  file-stream: f)))

(define (open-output-append-file path)
  (let ((f (fopen (relative-file path) "a")))
    (if (not f)
	(error "open-output-file: open of `~a' failed" path))
    (make <std-output-port>
	  name: path
	  file-stream: f)))

;;; normally you should not call this directly -- use
;;; `open-output-process' instead, because it may be redirected
;;; to a thread-aware implementation

(define (open-output-process/popen (str <string>))
  (let ((f (popen str "w")))
    (if (not f)
	(error "open-output-process: open of `~a' failed" str))
    (make <output-pipe-port>
	  name: str
	  file-stream: f)))

(define-syntax (an-open-stream method self)
  (let ((strm (file-stream self)))
    (if (eq? strm 0)
	(signal-port-is-closed self (mquote method))
	strm)))

(define-method output-port-write-char ((self <std-output-port>)
				       (ch <ascii-char>))
  (fputc (an-open-stream output-port-write-char self) ch)
  (values))

(define-method write-string ((self <std-output-port>)
			     (str <string>))
  (let ((n (fwrite/str (an-open-stream write-string self) str)))
    (if (eq? n (string-length str))
	(values)
	(error "write-string: wrote only ~d out of ~d bytes"
	       n (string-length str)))))

;;

(define (flush-stdio-out self fs)
  (if (not (eq? (fflush fs) 0))
      (error "flush-output-port ~s: failed" self)))

(define-method close-output-port ((self <std-output-port>))
  (let ((fs (an-open-stream close-output-port self)))
    (flush-stdio-out self fs)
    (let ((rc (fclose fs)))
      (set-file-stream! self 0)
      (if (eq? rc 0)
	  (values)
	  (error "close-output-port ~s: failed" self)))))

(define-method close-output-port ((self <output-pipe-port>))
  (let ((fs (an-open-stream close-output-port self)))
    (flush-stdio-out self fs)
    (let ((rc (pclose fs)))
      (set-file-stream! self 0)
      (if (eq? rc 0)
	  (values)
	  (error "close-output-port ~s: failed" self)))))

(define-method flush-output-port ((self <std-output-port>))
  (flush-stdio-out self (an-open-stream flush-output-port self))
  (values))

(define-method port-position ((self <std-output-port>))
  (ftell (an-open-stream port-position self)))

(define-method set-port-position! ((self <std-output-port>) posn)
  (fseek (an-open-stream set-port-position! self) posn 0)
  (values))


