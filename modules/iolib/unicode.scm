#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/iolib/unicode.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.11
 | File mod date:    2003-10-22 18:03:18
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  iolib
 |
 | Purpose:          Support UNICODE and UTF encoding
 `------------------------------------------------------------------------|#

(define (unicodify utf-str)
  (let loop ((s 0) (d '()))
    (if (< s (string-length utf-str))
	(bind ((ch i (utf-encoding-to-unicode utf-str s)))
	  (loop (+ s i) (cons ch d)))
	(reverse d))))

(define (utf-encoding-to-unicode utf-str offset)
  (let ((a (char->integer (string-ref utf-str offset))))
    (cond
     ((< a #x80)
      (values (integer->ascii-char a) 1))
     ((< a #b11100000)
      (let ((b (char->integer (string-ref utf-str (+ offset 1)))))
	(values (integer->char
		 (+ (logical-shift-left (bitwise-and a #b11111) 6)
		    (bitwise-and b #b111111)))
		2)))
     ((< a #b11110000)
      (let ((b (char->integer (string-ref utf-str (+ offset 1))))
	    (c (char->integer (string-ref utf-str (+ offset 2)))))
	(values (integer->unicode-char
		 (+ (logical-shift-left (bitwise-and a #b1111) 12)
		    (logical-shift-left (bitwise-and b #b111111) 6)
		    (bitwise-and c #b111111)))
		3)))
     (else
      (values (integer->unicode-char #x0080) 1)))))

(define *unicode-locale* 
  (let ((lang (getenv "LANG")))
    (and lang (string=? lang "UNIVERSAL"))))

(define-method unicode-char->utf-string ((ch <ascii-char>))
  (let (((i <fixnum>) (get-immob-value ch)))
    (if (< i #x0080)
	(string (integer->char i))
        (string (integer->char (+ #b11000000
                                  (logical-shift-right i 6)))
                (integer->char (+ #b10000000
                                  (bitwise-and i #b111111)))))))

(define-method unicode-char->utf-string ((ch <unicode-char>))
  (let ((i (get-immob-value ch)))
    (if (< i #x0080)
	(string (integer->char i))
	(if (< i #x0400)
	    (string (integer->char (+ #b11000000
				      (logical-shift-right i 6)))
		    (integer->char (+ #b10000000
				      (bitwise-and i #b111111))))
	    (string (integer->char (+ #b11100000
				      (logical-shift-right i 12)))
		    (integer->char (+ #b10000000
				      (bitwise-and (logical-shift-right i 6)
						   #b111111)))
		    (integer->char (+ #b10000000
				      (bitwise-and i #b111111))))))))

(define-method write-object ((self <unicode-char>) port)
  (let ((i (get-immob-value self)))
    (if (< i 128)
	(write (integer->char i) port)
	(if *unicode-locale*
	    (begin
	      (display "#\\" port)
	      (display (unicode-char->utf-string self) port))
	    (format port "#\\x~x" i)))))


(define (unicode-string-length (self <unicode-string>))
  (sub1 (div2 (bvec-length self))))

(define (unicode-string-ref (self <unicode-string>) 
			    (index <fixnum>))
  (integer->char (bvec-read-unsigned-16 self (mul2 index))))

(define (unicode-string-set! (self <unicode-string>) 
			     (index <fixnum>) 
			     (ch <char>))
  (bvec-write-unsigned-16 self (mul2 index) (char->integer ch))
  (values))

(define-method display-object ((self <unicode-string>) port)
  ;; write out unicode strings in UTF8 encoding
  (for-each
   (lambda (i)
     (write-string
      port
      (unicode-char->utf-string (unicode-string-ref self i))))
   (range (unicode-string-length self))))

(define-method write-object ((self <unicode-string>) port)
  (output-port-write-char port #\")
  (for-each
   (lambda (i)
     (let* ((ch (unicode-string-ref self i))
	    (cc (char->integer ch)))
       (if (or (< cc 32)
	       (eq? cc 34)
	       (eq? cc 92))
	   (let ((a (assq cc '((34 . #\")
			       (92 . #\\)
			       (10 . #\n)
			       (9 . #\t)
			       (7 . #\a)
			       (8 . #\b)
			       (11 . #\v)
			       (12 . #\f)
			       (13 . #\r)))))
	     (if a
		 (begin
		   (output-port-write-char port #\\)
		   (output-port-write-char port (cdr a)))
		 (format port "\\~03o" cc)))
	   (if (> cc 127)
	       (write-string port (unicode-char->utf-string ch))
	       (output-port-write-char port ch)))))
   (range (unicode-string-length self)))
  (output-port-write-char port #\"))
