#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/iolib/scan.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.36
 | File mod date:    2005-02-21 16:09:14
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  iolib
 |
 | Purpose:          Token scanner -- used by scheme reader
 `------------------------------------------------------------------------|#

(let-syntax
    ((peek-char (syntax-form (p) (input-port-peek-char p)))
     (read-char (syntax-form (p) (input-port-read-char p)))
     (line-number (syntax-form (p) (gvec-ref p 0)))
     (scan-token (syntax-form (p) (input-port-scan-token p))))

;; collect characters from a port `self'
;; as long as characters are read and the expression `more?' is true
;; for them

(define-method collect ((self <input-port>) (more? <function>))
  (let loop ((r '()))
    (let ((ch (peek-char self)))
      (if (and (not (eof-object? ch))
	       (more? ch))
	  (loop (cons (read-char self) r))
	  (reverse! r)))))

;;
;; some shorthand for making tokens...
;;
(define-syntax (make-token type data line-num)
  (values type data line-num))
    
(define-syntax (make-triv-token type line-num)
  (values type #f line-num))

;; some general facilities...

(define (lexical-error port message . args)
  (apply error 
	 (string-append "scan-token:~d: " message)
	 (line-number port)
	 args))

(define (bad-follow port char follow)
  (lexical-error port
		 "~s cannot be followed by ~s" 
		 char 
		 (if (eof-object? follow)
		     '<eof>
		     follow)))

;; the defining properties for identifiers and numbers
;;
;; (these are syntax for two reasons related to speed
;;   1. when the compiler inlines them, it can see they
;;      are bvecs and can omit the check
;;   2. they will be literals instead of TLREF's, saving
;;      an indirection
;;  and one other reason:
;;   3. the id-initial-switch is used only for initialization,
;;      so if init-scanners gets dropped, the string can get
;;      recovered

;;
;;  everything >=0x80 is now accepted, to support UTF encodings
;;  for unicode

(define-syntax ($id-contd-switch)
"0000000000000000000000000000000001001110001101111111111111101111111111111111111111111111111000110111111111111111111111111110001011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111")

(define-syntax ($id-initial-switch)
"0000000000000000000000000000000001001110001000011111111111101111111111111111111111111111111000110111111111111111111111111110001011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111")

;; this numswitch includes slash(/)'s to support rationals

(define-syntax ($num-switch)
"0000000000000000000000000000000000000000000101111111111111000000011111100100000100000000100000000111111001000001000000001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")

(define-syntax ($delimiter-switch)
"0000000001111100000000000000000010100000110000000000000000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")

;; is the character a valid starter?

(define-syntax (valid-id-initial? ch)
  (and (ascii-char? ch)
       (eq? (bvec-ref ($id-initial-switch) (char->integer ch)) 49)))

(define (valid-id-continued? ch)
  (and (ascii-char? ch)
       (eq? (bvec-ref ($id-contd-switch) (char->integer ch)) 49)))

(define (valid-num-continued? ch)
  (and (ascii-char? ch)
       (eq? (bvec-ref ($num-switch) (char->integer ch)) 49)))

;; this procedure returns #t if character is a valid delimeter
;; a delimiter is defined as <whitespace> | ( | ) | " | ;

(define-syntax (delimiter? ch)
  (if (eq? ch $eof-object)
      #t
      (eq? (bvec-ref ($delimiter-switch) (char->integer ch)) 49)))

(define (not-delimiter? ch)
  (if (eq? ch $eof-object)
      #f
      (not (eq? (bvec-ref ($delimiter-switch) (char->integer ch)) 49))))

;;
;; scan the characters of a number
;;

(define-syntax (numeric-lexeme p . pre)
  (letrec-syntax ((getit (syntax-form ()
			   (collect p valid-num-continued?))
			 (syntax-form (one . more)
			   (cons one (getit . more)))))
    (list->string (getit . pre))))

;; This procedure scans comments.  It reads everything until EOL or EOF is 
;; reached.  Once it reaches EOL or EOF, it calls scan-token again to parse the
;; next token.
;; (it uses `read-line' to read up to and including the NL

(define (scan-comment p c)
  (read-line p)
  (scan-token p))

;; this is used to scan the "." character.  

(define (scan-dot p c)
  (let ((ch (peek-char p)))
    (cond
     ;;
     ;; ".5" or something
     ;;
     ((char-numeric? ch)
      (interp-number p (numeric-lexeme p c)))
     ;;
     ;; ". " or something
     ;;
     ((delimiter? ch)
      (make-triv-token '<dot> (line-number p)))
     ;;
     ;; "..." 
     ;;
     ((eq? ch #\.)
      (read-char p)  ;; that's the second #\. already in ch
      (let ((ch (read-char p)))  ;; this had better be the third #\.
	(if (eq? ch #\.)
	    (if (delimiter? (peek-char p))
		(make-token '<symbol> '... (line-number p))
		(lexical-error p "'...' not followed by a delimiter"))
	    (lexical-error p 
			   "'..~a' is not a valid token"
			   (if (char? ch)
			       ch
			       "")))))
     (else
      (bad-follow p c ch)))))

;; This is used to skip multi-line LISP style comments.
;; This should be called after the first "#|" has been read

(define (skip-extended-comment p)
  (let ((ln (line-number p)))
    (let loop ((ch (read-char p)))
      (if (eof-object? ch)
	  (lexical-error p "line ~d: unterminated extended comment" ln)
	  ;; check for nested comment
	  (if (eq? ch #\#)
	      (let ((nxt (read-char p)))
		(if (eq? nxt #\|)
		    (begin
		      (skip-extended-comment p)
		      (loop (read-char p)))
		    ;; note that we don't really eat the character after #\#
		    ;; (unless it's a #\|)
		    ;; because it might be a #\# or #\|, in which case it we
		    ;; wouldn't want to miss it.  then #|##|...|#|# would fail
		    (loop nxt)))
	      (if (eq? ch #\|)
		  (let ((nxt (read-char p)))
		    (if (eq? nxt #\#)
			;; finally, return a value
			#t
			;; otherwise, process the succeeding char
			(loop nxt)))
		  (loop (read-char p))))))))

;;
;; having collected a numeric-looking token, try to interpret it
;;

(define (interp-number p number)
  (let ((n (or (string->number number)
	       ;;
	       ;; try a hook function to see if it's something
	       ;; similar to a number, but different
	       ;;
	       (alternate-number-parser number))))
    (if n
	;;
	;; it's a valid number or number-like object
	;; understood by alternate-number-parser
	;;
	(make-token '<literal> n (line-number p))
        (if (and (char>=? (string-ref number 0) #\0)
                 (char<=? (string-ref number 0) #\9))
            (lexical-error p "Badly formatted number: ~s" number)
            (make-token '<symbol> (string->symbol number) (line-number p))))))

(define *alternate-number-parsers* '())

(define (alternate-number-parser str)
  (let loop ((lst *alternate-number-parsers*))
    (if (null? lst)
	#f
	(or ((car lst) str) 
	    (loop (cdr lst))))))

;;;  Note that "alternate number parsers" are not really for *numeric*
;;;  items.  They are for things that the scanner initially rolls up
;;;  as a number, but aren't.  For example "2005-02-18" will get picked
;;;  up as a single token and handed to interp-number.
;;;
;;;  Things that *are* numbers should be added to *number-parsers* so
;;;  that string->number will handle it.  Doing this is important partly
;;;  because some number parsers will recursively call string->number
;;;  to parse substrings.

(define (add-alternate-number-parser! (fn <function>))
  (set! *alternate-number-parsers* 
	(cons fn (delq fn *alternate-number-parsers*))))

;; This procedure is used to scan the special math operators + and -.
;; There are two possible cases of the + and - signs:
;; 1. It is the mathmatical operator addition or subtraction, in which case it
;;    must be followed by white space.  In this case, we return the symbol + or
;;    -. 
;; 2. It can prefix a number, indicating the sign of the number.  In this case
;;    we collect a numeric lexeme and try to interp it as a number

(define (scan-math-operator p c)
    (let ((ch (peek-char p)))
      (if (delimiter? ch)
          (list->symbol-token (list c) (line-number p))
          (interp-number p (numeric-lexeme p c)))))



(define (scan-str-get-esc-number p init-lst radix valid-digit?)
  (let digit-loop ((digits init-lst))
    (let ((c (peek-char p)))
      (if (eof-object? c)
	  #\?
	  (if (valid-digit? c)
	      (digit-loop (cons (read-char p) digits))
	      (let* ((num-str (list->string (reverse digits)))
		     (num (string->number num-str radix)))
		(if (and num (< num 256))
		    (integer->ascii-char num)
		    (lexical-error
		     p 
		     "string escape `\\~a' does not form a valid char number"
		     num-str))))))))

(define (scan-str-escape p ch add!)
  (case (char-downcase ch)
    ((#\n) (add! #\newline))
    ((#\t) (add! #\tab))
    ((#\") (add! #\"))
    ((#\\) (add! #\\))
    ((#\x) (add! (scan-str-get-esc-number p 
					  '() 
					  16 
					  (lambda (ch)
					    (or (and (char>=? ch #\0)
						     (char<=? ch #\9))
						(and (char>=? ch #\a)
						     (char<=? ch #\f))
						(and (char>=? ch #\A)
						     (char<=? ch #\F)))))))
    ((#\') (add! #\'))
    ((#\.)) ;; a character escape NOP, useful for terminating numeric escapes
    ((#\v) (add! (integer->ascii-char 11)))
    ((#\b) (add! (integer->ascii-char 8)))
    ((#\r) (add! (integer->ascii-char 13)))
    ((#\f) (add! (integer->ascii-char 12)))
    ((#\a) (add! (integer->ascii-char 7)))
    ((#\?) (add! #\?))
    ((#\c) (let ((nxt (read-char p)))
	     (if (char? nxt)
		 (let ((code (char->integer (char-upcase nxt))))
		   (if (and (>= code 64) (< code 96))
		       (add! (integer->char (- code 64)))
		       (lexical-error 
			p
			"~s cannot follow `\\c' as a string escape"
			nxt)))
		 #f)))
    (else 
     (if (char-numeric? ch)
	 (add! (scan-str-get-esc-number p 
					(cons ch '()) 
					8
					(lambda (ch)
					  (and (char>=? ch #\0)
					       (char<=? ch #\9)))))
	 (lexical-error p "~s cannot follow `\\' as a string escape" ch)))))

(define (scan-string* add! p)
  (let loop ()
    (let ((c (read-char p)))
      (if (char? c)
	  (if (eq? c #\\)
	      ;; an escape character.. process it
	      (let ((c (read-char p)))
		(if (char? c)
		    (begin
		      (scan-str-escape p c add!)
		      (loop))
		    ;; return #f, indicating an unterminated string
		    #f))
	      ;; not an escape character...
	      (if (eq? c #\")
		  ;; but the end of the string, so return
		  ;; a success story
		  #t
		  ;; not an escape or the EOS, so a regular char
		  (begin
		    (add! c)
		    (loop))))
	  ;; not a character at all; must be EOF
	  ;; return #f to indicate an error
	  #f))))
	 
(define (scan-string p c)
  (bind ((ln (line-number p))
	 (lst result (call-with-list-extending
		      (lambda (add!) 
			(scan-string* add! p)))))
    (if result
	(make-token '<literal> (list->string lst) ln)
	(lexical-error p "line ~d: unterminated string ~#*20s" 
		       ln
		       (list->string lst)))))

(define (string-ci-assoc str lst)
    (let loop ((l lst))
	(if (null? l)
	    #f
	    (let ((entry (car l)))
		(if (string-ci=? str (car entry))
		    entry
		    (loop (cdr l)))))))

(define (sharp-num-handler p str)
  (if (vmemq (string-ref str 1) '#(#\x #\o #\e #\i #\b #\d))
      (interp-number p str)
      #f))

(define (sharp-uniqobj-handler p str)
  (let ((u (string->unique-object str)))
    (if u
	(make-token '<literal> u (input-port-line-number p))
	#f)))

(define (scan-sharp-comma p c)             ; SRFI-10
  (make-token '<hash-comma-token> 0 (input-port-line-number p)))

(define *sharp-handlers*
  (list sharp-uniqobj-handler 
	sharp-num-handler))

(define (add-sharp-handler! proc)
  (set! *sharp-handlers* (cons proc *sharp-handlers*)))

;; This scans all tokens that begin w/ a #<keyword>

(define (special-sharp p first-ch)
  ;;
  ;; #t and #f case have already been checked
  ;;
  (let ((str (list->string (cons* #\#
				  first-ch
				  (collect p not-delimiter?)))))
    (let loop ((h *sharp-handlers*))
      (if (null? h)
	  (lexical-error p "Bad sharp: ~s" str)
	  (bind ((type data line ((car h) p str)))
	    (if type
		(values type data line)
		(loop (cdr h))))))))

(define (scan-sharp-f p c)
  (if (delimiter? (peek-char p))
      (make-token '<literal> #f (line-number p))
      (special-sharp p c)))

(define (scan-sharp-t p c)
  (if (delimiter? (peek-char p))
      (make-token '<literal> #t (line-number p))
      (special-sharp p c)))

(define (add-sharp-scanner! ch proc)
  (vector-set! *sharp-scanners* (char->integer ch) proc))

(define (init-sharp-scanners)
  (set! *sharp-scanners*
	(make-vector 256
		     (lambda (p c)
		       (bad-follow p #\# c))))
  ;;
  (for-each
   (lambda (ch)
     (if (char-alphabetic? ch)
	 (add-sharp-scanner! ch special-sharp)))
   (map integer->char (range 256)))
  ;;
  (add-sharp-scanner! #\f scan-sharp-f)
  (add-sharp-scanner! #\F scan-sharp-f)
  (add-sharp-scanner! #\t scan-sharp-t)
  (add-sharp-scanner! #\T scan-sharp-t)
  (add-sharp-scanner! #\, scan-sharp-comma)
  ;;
  (add-sharp-scanner! #\\ scan-char-const)
  ;;
  ;;  vectors #(...)
  ;;
  (add-sharp-scanner! 
   #\(
   (lambda (p c)
     (make-triv-token '<open-vector> (line-number p))))
  ;;
  ;; special hack for unix scripts.. "#!" acts like ";"
  ;;
  (add-sharp-scanner! #\! scan-comment)
  ;;
  ;; multi-line (extended) comments  #|...|#
  ;;
  (add-sharp-scanner! 
   #\|
   (lambda (p c)
     (skip-extended-comment p)
     (scan-token p))))

(define (scan-char-const p c)
  (let ((ch (read-char p)))
    (if (eof-object? ch)
	(lexical-error p "Unexpected <eof> after #\\")
	;; special common case
	(if (delimiter? (peek-char p))
	    (make-token '<literal> ch (line-number p))
	    (make-token '<literal>
			(scan-char-literal p ch)
			(line-number p))))))

(define (scan-special p c)
  (let ((ch (read-char p)))
    (if (eof-object? ch)
	(lexical-error p "Unexpected <eof> after `#'")
	((vector-ref *sharp-scanners* (char->integer ch)) p ch))))

;; convert a list of characters to a symbol token object
;; note that we downcase the characters (YUCK; should have
;; a flag to say if we are being case sensitive or not)

(define (list->symbol-token chs line)
  (make-token '<symbol>
	      (string->symbol
	       (list->string
		chs
		;(map char-downcase chs)
		))
	      line))

(define (scan-ident p (c <ascii-char>))
  (list->symbol-token (cons c (collect p valid-id-continued?))
		      (line-number p)))

(define (scan-braced-text p c)
  (let ((ln (input-port-line-number p))
	(str (list->string 
	      (collect p
		       (let ((depth 0)
			     (in-string? #f)
			     (escaped? #f))
			 (lambda (ch)
			   (cond
			    ((< depth 0)
			     #f)
			    (escaped?
			     (set! escaped? #f)
			     #t)
			    ((eq? ch #\\)
			     (set! escaped? #t)
			     #t)
			    (in-string?
			     (if (eq? ch #\")
				 (set! in-string? #f))
			     #t)
			    ((eq? ch #\{)
			     (set! depth (+ depth 1))
			     #t)
			    ((eq? ch #\})
			     (set! depth (- depth 1))
			     #t)
			    ((eq? ch #\")
			     (set! in-string? #t)
			     #t)
			    (else
			     (char? ch)))))))))
    (make-token '<curly-braced> 
		(substring str 0 (sub1 (string-length str)))
		ln)))

(define *scanners* #f)
(define *sharp-scanners* #f)

#| initialize the *scanner-table* top-level-variable
   to be a 256-element vector containing all ASCII characters from ASCII  0 to
   ASCII 255.  Each element in the vector contains pointers to the appropriate 
   procedure to scan that character.  This vector essentially maps a
   character to the function that correctly parse that character.

   Mathmatically this is stated: 
	f(character)=function to correctly parse that character.

	e.g. f(a)=function to parse identifier
	     f(3)=function to parse number

   Scanner algorithm:
   We read in a character.  Depending on what character is read in, we then 
   call the procedure to correctly parse that character.  
|#

(define (init-scanners)
    (set! *scanners* 
	  (make-vector 256 
		       (lambda (p c) 
			 (lexical-error p "~s cannot start a lexeme" c))))
    (let loop (((i <fixnum>) 0))
      (if (fixnum<? i 256)
	  (begin
	    (if (valid-id-initial? (integer->char i))
		(vector-set! *scanners* i scan-ident))
	    (loop (add1 i)))))
    (init #\0 #\9 (lambda (p c)
		    (interp-number p (numeric-lexeme p c))))
    (init1 #\. scan-dot)
    (init1 #\" scan-string)
    (init1 #\# scan-special)
    (init1 #\; scan-comment)
    (inits '(#\+ #\-) scan-math-operator)
    (init1 #\{ scan-braced-text)
    (init1 #\( (lambda (p c) (make-triv-token '<open-paren> (line-number p))))
    (init1 #\) (lambda (p c) (make-triv-token '<close-paren> (line-number p))))
    (init1 #\' (lambda (p c) (make-triv-token 'quote (line-number p))))
    (init1 #\` (lambda (p c) (make-triv-token 'quasiquote (line-number p))))
    (init1 #\, (lambda (p c) 
		(if (eq? (peek-char p) #\@)
		    (begin
			(read-char p)
			(make-triv-token 'unquote-splicing (line-number p)))
		    (make-triv-token 'unquote (line-number p)))))

    (let loop (((i <fixnum>) 0))
      (if (fixnum<=? i 32)
	  (begin
	    (if (char-whitespace? (integer->char i))
		(vector-set! *scanners* i scan-past-whitespace))
	    (loop (+ i 1)))))

    (define (init1 ch s)
	(vector-set! *scanners* (char->integer ch) s))
	
    (define (inits l s)
	(for-each
	    (lambda ((ch <ascii-char>))
		(vector-set! *scanners* (char->integer ch) s))
	    l))

    (define (init from to s)
	(let ((limit (char->integer to)))
	    (do ((i (char->integer from) (add1 i)))
		((> i limit))
	      (vector-set! *scanners* i s)))))

;; keeps skipping whitespace until no more to skip,
;; then calls the next guy
;;
;; skips ';' comments, too

(define (scan-past-whitespace p c)
  (let loop ()
    (let ((ch (read-char p)))
      (if (eof-object? ch)
	  ch
	  (if (char-whitespace? ch)
	      (loop)
	      (if (eq? ch #\;)
		  (begin
		    ;; skip rest of line
		    (read-line p)
		    (loop))
		  ((vector-ref *scanners* (char->integer ch)) p ch)))))))
)

;; scan a single token from an input port p

(define (scanner-dispatch port ch)
  ((vector-ref *scanners* (char->integer ch)) port ch))

(define-method input-port-scan-token ((p <input-port>))
  (let ((c (input-port-read-char p)))
    (if (eof-object? c)
	c
	(scanner-dispatch p c))))

(%early-once-only
 (init-scanners)
 (init-sharp-scanners))
