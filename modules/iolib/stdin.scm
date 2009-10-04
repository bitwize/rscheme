#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/iolib/stdin.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.19
 | File mod date:    2006-01-28 16:50:06
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  iolib
 |
 | Purpose:          input through stdio, <std-input-port>
 `------------------------------------------------------------------------|#

(define-method input-port-char-ready? ((self <std-input-port>))
  (not (eq? (fcanget (an-open-stream input-port-char-ready? self)) 0)))

;;;

(define-class <input-pipe-port> (<std-input-port>))

(define (open-input-file path)
  (let* ((rpath (relative-file path))
         (f (fopen rpath "r")))
    (if f
	(make <std-input-port>
	      name: rpath
	      input-port-line-number: 1
	      file-stream: f)
	(error "open-input-file: open of `~a' failed" rpath))))


;;; normally you should not call this directly -- use
;;; `open-input-process' instead, because it may be redirected
;;; to a thread-aware implementation

(define (open-input-process/popen (str <string>))
  (let ((f (popen str "r")))
    (if (not f)
	(error "open-input-process: open of `~a' failed" str))
    (make <input-pipe-port>
	  name: str
	  input-port-line-number: 1
	  file-stream: f)))

(define-method input-port-read-char ((self <std-input-port>))
  (let ((c (fgetc (an-open-stream input-port-read-char self))))
    (if c
	(begin
	  (if (eq? c #\newline)
	      (increment-line self))
	  c)
	$eof-object)))

(define-syntax (stdin-peek self)
  (fpeekc (an-open-stream stdin-peek self)))

(define-method input-port-peek-char ((self <std-input-port>))
  (or (stdin-peek self) $eof-object))

(define-method collect ((self <std-input-port>) (more? <function>))
  (let ((strm (an-open-stream collect self)))
    (let loop ((r '()))
      (let ((ch (fpeekc strm)))
	(if (and ch (more? ch))
	    (begin
	      (if (eq? ch #\newline)
		  (increment-line self))
	      (fgetc strm)
	      (loop (cons ch r)))
	    (reverse! r))))))

(define-method input-port-read-line ((self <std-input-port>))
  (let ((line (fgetln (an-open-stream input-port-read-line self))))
    (increment-line self)
    (or line $eof-object)))
  
(define-method close-input-port ((self <std-input-port>))
  (if (eq? (fclose (an-open-stream close-input-port self)) 0)
      (set-file-stream! self 0)
      (error "close-input-port ~s: failed" self)))

(define-method close-input-port ((self <input-pipe-port>))
  (if (eq? (pclose (an-open-stream close-input-port self)) 0)
      (set-file-stream! self 0)
      (error "close-input-port ~s: failed" self)))

(define (file->string file)
  (let ((f (fopen (relative-file file) "r")))
    (if f
	(begin
	  (fseek f 0 2)
	  (let* ((size (ftell f))
		 (str (bvec-alloc <string> (+ size 1))))
	    (fseek f 0 0)
 	    (do ((i 0 (+ i 65536)))
                ((>= i size))
              (fread-fill f str i (min 65536 (- size i))))
	    (fread-fill f str 0 size)
	    (fclose f)
	    str))
	(error "file->string: couldn't open `~a'" file))))

;;;

(define-method port-position ((self <std-input-port>))
  (ftell (an-open-stream port-position self)))

(define-method set-port-position! ((self <std-input-port>) posn)
  (fseek (an-open-stream set-port-position self) posn 0)
  (values))


