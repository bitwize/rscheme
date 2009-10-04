#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/imageio/refsmgr.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.7
 | File mod date:    2007-01-28 10:02:16
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  imageio
 |
 | Purpose:          manage references
 `------------------------------------------------------------------------|#

;;
;;  encoding & decoding schemes for the REF section
;;

;; take your pick

  (define-syntax (debug . args))
; (define-syntax (debug . args) (format #t . args))

(define-constant $nul #\NUL)

(define (string-length-rec str-or-list)
  (if (string? str-or-list)
      (string-length str-or-list)
      (if (char? str-or-list)
	  1
	  (let loop ((len 0)
		     (lst str-or-list))
	    (if (pair? lst)
		(loop (+ len (string-length-rec (car lst))) (cdr lst))
		len)))))

(define (string-copy-rec! (dest <string>) 
			  (offset <fixnum>) 
			  str-or-list)
  (if (string? str-or-list)
      (let (((len <fixnum>) (string-length str-or-list)))
	(bvec-copy dest offset
		   str-or-list 0
		   len)
	(+ offset len))
      (if (char? str-or-list)
	  (begin
	    (string-set! dest offset str-or-list)
	    (+ offset 1))
	  (let loop ((x offset)
		     (lst str-or-list))
	    (if (pair? lst)
		(loop (string-copy-rec! dest x (car lst)) (cdr lst))
		x)))))

(define (string-append-rec str-or-list)
  (let ((s (make-string (string-length-rec str-or-list))))
    (string-copy-rec! s 0 str-or-list)
    s))

(define (optimize-refs ref-vec symbol-dict class-dict make-other-refs inv)
  (let ((fn-descrs '())
	(classes '())
	(symbols '())
	(other '()))
    ;; collate the various kinds of references
    (vector-for-each (lambda (ref)
		       (cond
			((symbol? ref)
			   (set! symbols (cons ref symbols)))
			((class? ref)
			 (set! classes (cons ref classes)))
			((instance? ref <code-ptr-anchor>)
			 (if (not (memq (fn-descr ref) fn-descrs))
			     (begin
			       (set! fn-descrs (cons (fn-descr ref) fn-descrs))
			       (set-code-ptrs! (fn-descr ref) '())))
			 (set-code-ptrs! (fn-descr ref)
					 (cons ref 
					       (code-ptrs (fn-descr ref)))))
			((instance? ref <fn-descr-anchor>)
			 (if (not (memq ref fn-descrs))
			     (begin
			       (set! fn-descrs (cons ref fn-descrs))
			       (set-code-ptrs! ref '()))))
			(else
			 (set! other (cons ref other)))))
		     ref-vec)
    ;;
    ;;  construct the parts
    ;;
    (bind ((code-ptr-refstr code-ptr-order (make-code-ptr-refs fn-descrs))
	   (class-refstr class-order (make-class-refs classes class-dict inv))
	   (symbol-refstr symbol-order (make-symbol-refs symbols symbol-dict))
	   (other-refstr other-order (make-other-refs other))
	   (all-order (list->vector (append code-ptr-order 
					    class-order
					    symbol-order
					    other-order))))
      (values (string-append-rec (list (number->binary-2
					(vector-length all-order))
				       code-ptr-refstr
				       class-refstr
				       symbol-refstr
				       other-refstr
				       "!"))
	      all-order))))

(define (make-other-refs others)
  (if (null? others)
      (values '() '())
      (error "pickle: don't know how to pickle ~d references: ~s"
	     (length others)
	     others)))

(define (make-code-ptr-refs cps)
  (if (null? cps)
      (values '() '())
      (gen-code-ptr-refs cps)))

(define (gen-code-ptr-refs cps)
  (let ((by-module '()))
    (for-each (lambda (cp)
		(let ((x (assoc (module-name cp) by-module)))
		  (if x
		      (set-cdr! x (cons cp (cdr x)))
		      (set! by-module
			    (cons (list (module-name cp) cp)
				  by-module)))
		  (values)))
	      cps)
    ;;
    ;; sort the code ptrs by monotone #
    ;;
    (for-each (lambda (a)
		(for-each (lambda (fn-d)
			    (set-code-ptrs! fn-d
					    (sort (code-ptrs fn-d)
						  (lambda (a b)
						    (< (monotone-number a)
						       (monotone-number b))))))
			  (cdr a)))
	      by-module)
    ;;
    ;; partition & sort by part
    ;;
    (for-each (lambda (a)
		(let ((parts '()))
		  (debug "partitioning module parts: ~a\n" (car a))
		  (for-each (lambda (fn-d)
			      (debug "  fn-d: ~s\n" fn-d)
			      (let ((x (assq (part-number fn-d) parts)))
				(if x
				    (set-cdr! x (cons fn-d (cdr x)))
				    (set! parts (cons (list (part-number fn-d)
							    fn-d)
						      parts)))
				(values)))
			    (cdr a))
		  ;; sort the functions in the part by fn#
		  (for-each (lambda (part)
			      (debug "  sorting part ~d\n" (car part))
			      (set-cdr! part
					(sort (cdr part)
					      (lambda (a b)
						(< (function-number a)
						   (function-number b)))))
			      (values))
			    parts)
		  ;;
		  (set-cdr! a
			    (sort parts 
				  (lambda (a b)
				    (< (bitwise-and (car a) #x3ff)
				       (bitwise-and (car b) #x3ff)))))
		  (values)))
	      by-module)
    ;;
    ;; print it out
    ;;
    (for-each (lambda (a)
		(debug "MODULE: ~a\n" (car a))
		(for-each 
		 (lambda (part)
		   (debug "  PART ~d\n" (car part))
		   (for-each 
		    (lambda (fn-d)
		      (debug "   FUNCTION DESCRIPTOR: ~s\n" fn-d)
		      (for-each (lambda (cp)
				  (debug "\t~s\n" cp))
				(code-ptrs fn-d)))
		    (cdr part)))
		 (cdr a)))
	      by-module)
    ;;
    ;; return two values -- the encoding strings (a list of strings)
    ;; and the actual order (which, note, may even include objects
    ;; not originally in the vector -- in particular, if a <fn-d>
    ;; is not actually in the input vector, it will still get
    ;; output & take up an id)
    ;;
    (values 
     ;;
     ;; generate the string
     ;;
     (list #\m
	   (map (lambda (a)
		  (gen-for-module (car a) (cdr a)))
		by-module)
	   $nul)
     ;;
     ;; compute the order upstairs
     ;;
     (let ((first #f)
	   ((last <pair>) (cons 0 '())))
       (set! first last)
       (let-syntax ((in-order (syntax-form (item)
				(let (((p <pair>) (cons item '())))
				  (set-cdr! last p)
				  (set! last p)))))
	 (for-each 
	  (lambda (a)
	    (for-each
	     (lambda (part)
	       (for-each (lambda (fn-d)
			   (in-order fn-d)
			   (for-each (lambda (p) (in-order p))
				     (code-ptrs fn-d)))
			 (cdr part)))
	     (cdr a)))
	  by-module))
       (cdr first)))))

(define (gen-for-module module-name parts)
  (debug "REFS TO CODE MODULE ~a (~d parts)\n" 
	  module-name
	  (length parts))
  (list (str-literal module-name "module name")
	$nul  ;; make a NUL available after the module name
	(number->binary-2 (length parts))
	(map (lambda (part)
	       (debug "  PART ~d\n" (car part))
	       (cons (number->binary-4 (car part))
		     (let ((c (compact-code-ptrs-in-part (cdr part))))
		       (if c
			   (cons (number->binary-1 c) '())
			   (if (not (every? exactly-entry? (cdr part)))
			       ;; scattered usage
			       (cons
				(integer->char 255)
				(gen-non-compact-part (cdr part)))
			       ;; semi-compact (no non-entry points)
			       (cons
				(integer->char 254)
				(gen-semi-compact-part (cdr part))))))))
	     parts)))

(define (compact-code-ptrs-in-part fn-descrs)
  (if (and (equal? (map function-number fn-descrs)
		   (range (length fn-descrs)))
	   (every? exactly-entry? fn-descrs))
      (length fn-descrs)
      #f))

(define (exactly-entry? (fn-descr <fn-descr-anchor>))
  (let ((cps (code-ptrs fn-descr)))
    (and (pair? cps)
	 (null? (cdr cps))
	 (eq? (monotone-number (car cps)) 0))))

;; a semi-compact representation, usable when the functions are
;; just a subset of the part, but they are all entry points

(define (gen-semi-compact-part fn-descrs)
  (cons (number->binary-1 (length fn-descrs))
	(map (lambda (fn-d)
	       (debug "    ENTRY FOR FN ~s\n" fn-d)
	       (number->binary-1 (function-number fn-d)))
	     fn-descrs)))

(define (gen-non-compact-part fn-descrs)
  (cons (number->binary-1 (length fn-descrs))
	(map (lambda (fn-d)
	       (debug "    MONOTONES OF FN ~s\n" fn-d)
	       (cons* (number->binary-1 (function-number fn-d))
		      (number->binary-1 (length (code-ptrs fn-d)))
		      (map (lambda (cp)
			     (number->binary-2 (monotone-number cp)))
			   (code-ptrs fn-d))))
	     fn-descrs)))

(define (number->binary-4 num)
  (string (integer->char (logical-shift-right num 24))
	  (integer->char (bitwise-and (logical-shift-right num 16) #xFF))
	  (integer->char (bitwise-and (logical-shift-right num 8) #xFF))
	  (integer->char (bitwise-and (logical-shift-right num 0) #xFF))))

(define (number->binary-2 num)
  (if (or (< num 0) (> num 65535))
      (error "pickle: number `~d' is too big for unsigned 2-byte repr"
	     num)
      (string (integer->char (bitwise-and (logical-shift-right num 8) #xFF))
	      (integer->char (bitwise-and (logical-shift-right num 0) #xFF)))))

(define (number->binary-1 num)
  (if (or (< num 0) (> num 255))
      (error "pickle: number `~d' is too big for unsigned 1-byte repr"
	     num)
      (integer->char num)))

(define (str-literal str what)
  ;; reserve 251...255 for special tokens
  (if (> (string-length str) 250)
      (error "pickle: ~a too long: ~a" what str))
  (list (integer->char (string-length str)) str))

(define (gen-symbol-list symbols)
  (list (map (lambda (sym)
	       (str-literal (symbol->string sym) "symbol"))
	     symbols)
	$nul))

(define (make-symbol-refs symbols symbol-dict)
  (split-on-dict symbol-dict symbols symbols #\S #\s))

(define (make-class-refs classes class-dict inv)
  (split-on-dict class-dict (map (lambda (c)
                                   (or (table-lookup inv c)
                                       (name c)))
                                 classes) 
                 classes #\C #\c))

(define (split-on-dict dict symbols data class-for-in class-for-not-in)
  (if (null? symbols)
      (values '() '())
      (if (not dict)
	  (values (cons class-for-not-in (gen-symbol-list symbols))
		  data)
	  (let ((in '())
		(in-d '())
		(not-in '())
		(not-in-d '())
		(tbl (symbol-dict-id-table dict)))
	    (for-each (lambda (s d)
			(let ((id (table-lookup tbl s)))
			  (debug "split ~a/~a: ~s (~s) => ~s\n"
				  class-for-in class-for-not-in
				  s d id)
			  (if id
			      (begin
				(set! in (cons (integer->char id) in))
				(set! in-d (cons d in-d)))
			      (begin
				(set! not-in (cons s not-in))
				(set! not-in-d (cons d not-in-d))))))
		      symbols
		      data)
	    (if (null? in)
		(values (cons class-for-not-in 
			      (gen-symbol-list not-in))
			not-in-d)
		(if (null? not-in)
		    (values (cons* class-for-in
				   (integer->char (length in))
				   in)
			    in-d)
		    (values (list class-for-in 
				  (integer->char (length in))
				  in
				  class-for-not-in
				  (gen-symbol-list not-in))
			    (append in-d not-in-d))))))))
