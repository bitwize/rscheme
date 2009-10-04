#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/iolib/scanchar.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.6
 | File mod date:    1998-08-29 22:18:04
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  iolib
 |
 `------------------------------------------------------------------------|#

(define $special-chars '(("nul" . 0) ("soh" . 1) ("stx" . 2) ("etx" . 3) 
			("eot" . 4) ("enq" . 5) ("ack" . 6) ("bel" . 7) 
			("bs" . 8) ("tab" . 9) ("newline" . 10) ("vt" . 11) 
			("np" . 12) ("cr" . 13) ("so" . 14) ("si" . 15) 
			("dle" . 16) ("dc1" . 17) ("dc2" . 18) ("dc3" . 19) 
			("dc4" . 20) ("nak" . 21) ("syn" . 22) ("etb" . 23) 
			("can" . 24) ("em" . 25) ("sub" . 26) ("esc" . 27) 
			("fs" . 28) ("gs" . 29) ("rs" . 30) ("us" . 31) 
			;;
			("space" . 32) ("del" . 127)
			("ff" . 12) ("lf" . 10) ("nl" . 10)
			;;
			;; from [Dylan 92] p.24
			;;
			("rubout" . 127)
			("page" . 11)
			("backspace" . 8)
			("return" . 13)
			("linefeed" . 10)))

;; grok-char-literal* returns the character code
;; of the character grokked.

(define (grok-char-literal* (char-list <list>))
  (let ((key (list->string (map char-downcase char-list))))
    (let ((a (assoc key $special-chars)))
      (if a
	  (cdr a)
	  #f))))

;;
;; this is not called for "normal" character constants... 
;; those are special-cased in scan-special
;;

(define-syntax (match-char-prefix whole-char-list pattern proc)
  (letrec-syntax ((matcher (syntax-form (char-list)
			     (proc char-list))
			   ;;
			   (syntax-form (char-list '&more)
			     (if (pair? char-list)
				 (proc char-list)
				 #f))
			   ;;
			   (syntax-form (char-list '&char p)
			     (if (pair? char-list)
				 (let ((rc (grok-char-literal p
							     (car char-list)
							     (cdr char-list))))
				   (if rc
				       (proc rc)
				       #f))
				 #f))
			   ;;
			   (syntax-form (char-list next-pat . more-pat)
			     (if (pair? char-list)
				 (if (char-ci=? (car char-list) next-pat)
				     (let ((temp (cdr char-list)))
				       (matcher temp . more-pat))
				     #f)
				 #f))))
    (matcher whole-char-list . pattern)))
  

(define (grok-char-literal p (ch <ascii-char>) (rest <list>))
  (if (null? rest)
      (char->integer ch)
      (let (((lst <pair>) (cons ch rest)))
	(or (match-char-prefix 
	     lst
	     (#\m #\- &char p)
	     (lambda ((cc <fixnum>))
	       (if (< cc 128)
		   (+ cc 128)
		   (lexical-error 
		    p
		    "`M-' prefix invalid on character code ~d"
		    cc))))
	    (match-char-prefix
	     lst
	     (#\c #\- &char p)
	     (lambda ((c <fixnum>))
	       (cond
		((and (>= c 64) (< c (+ 64 32)))    ;; #\C-@ .. #\C-_
		 (- c 64))
		((and (>= c 192) (< c (+ 192 32)))  ;; #\C-M-@ .. #\C-M-_
		 (- c 64))
		; note that the lower-case versions ONLY permit
		; letters (ie, #\C-`, #\C-{, ..., #\C-DEL are not valid)
		((and (>= c 97) (< c (+ 97 26)))    ;; #\C-a .. #\C-z
		 (- c 96))
		((and (>= c 225) (< c (+ 225 26)))  ;; #\C-M-a .. #\C-M-z
		 (- c 96))
		(else
		 (lexical-error
		  p
		  "`C-' prefix invalid on character code ~d"
		  c)))))
	    (match-char-prefix
	     lst
	     (#\x &more)
	     (lambda ((rest <pair>))
	       (string->number (list->string rest) 16)))
	    (match-char-prefix
	     lst
	     (#\d &more)
	     (lambda ((rest <pair>))
	       (string->number (list->string rest) 10)))
	    (match-char-prefix
	     lst
	     (#\o &more)
	     (lambda ((rest <pair>))
	       (string->number (list->string rest) 8)))
	    (grok-char-literal* lst)))))

(define (scan-char-literal p ch)
  (let* ((rest-of-lexeme (collect p not-delimiter?))
	 (char-code (grok-char-literal p ch rest-of-lexeme)))
    (if char-code
	(if (>= char-code 0)
	    (if (< char-code 256)
		(integer->ascii-char char-code)
		(if (< char-code 65536)
		    (integer->unicode-char char-code)
		    (lexical-error 
		     p
		     "character code value ~d is out of range (>65535)"
		     char-code)))
	    (lexical-error 
	     p
	     "character code value ~d is invalid (<0)"
	     char-code))
	(lexical-error p
		       "`~a' is not a valid character literal"
		       (list->string (cons #\# (cons ch rest-of-lexeme)))))))
