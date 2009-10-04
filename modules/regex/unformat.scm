#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/regex/unformat.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.8
 | File mod date:    1999-02-12 09:26:17
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  regex
 |
 `------------------------------------------------------------------------|#

(define (str-unfmt-pat v)
  (case (pre-dot-number v)
    ((#f) '(* any))
    ((1) '(seq any))
    ((2) '(seq any any))
    ((3) '(seq any any any))
    ((4) '(seq any any any any))
    ((5) '(seq any any any any any))
    (else
     (cons 'seq (map (lambda (i)
		       'any)
		     (range (pre-dot-number v)))))))

(define (unformat/a (info <vector>))
  (values 
   (str-unfmt-pat info)
   (lambda ((str <string>))
     str)))

(define (unformat/s (info <vector>))
  (values
   (str-unfmt-pat info)
   (lambda ((str <string>))
     (let ((item (read (open-input-string str))))
       (if (eof-object? item)
	   (error "unformat/s: ~s has no readable content" str)
	   item)))))
  
(define (num-unfmt-pat v)
  '(seq (? #\-)
	(+ (or digit #\. 
	       #\/)) ;; `/' for rationals,
	(? (seq (or #\E #\e)
		(? (or #\+ #\-))
		(+ digit)))))

(define (int-unfmt-pat v space-ok?)
  (let ((n (pre-dot-number v)))
    (if n
	(let ((ch (if space-ok?
		      '(or digit space)
		      'digit)))
	  (cons 'seq (map (lambda (i) ch) (range n))))
	(if space-ok?
	    '(seq (* space) (+ digit) (* space))
	    '(+ digit)))))

(define (unformat/d (info <vector>))
  (if (pre-dot-number info)
      (values
       (if (sharp-flag info)
	   (int-unfmt-pat info #t)
	   (str-unfmt-pat info)) ;; get an exact substring
       (lambda ((str <string>))
	 ;; but then only try to interpret the trimmed part
	 (let ((t (trim-whitespace str)))
	   (or (string->number t)
	       (error "unformat/d: ~s is not a number" t)))))
      (values
       (if (sharp-flag info)
	   (int-unfmt-pat info #f)
	   (num-unfmt-pat info))
       (lambda ((str <string>))
	 (or (string->number str)
	     (error "unformat/d: ~s is not a number" str))))))

(%early-once-only
(define *global-unformatters*
  (list (cons #\a unformat/a)
	(cons #\s unformat/s)
	(cons #\d unformat/d)))
)

(define (parse-unformat-opts opts)
  (let ((anywhere? #f)
	(unfs *global-unformatters*))
    ;;
    (if (and (pair? opts)
	     (boolean? (car opts)))
      (begin
	(set! anywhere? (car opts))
	(set! opts (cdr opts))))
    ;;
    (if (and (pair? opts)
	     (eq? (car opts) 'using:))
	(begin
	  (set! unfs (cadr opts))
	  (set! opts (cddr opts))))
    ;;
    (if (not (null? opts))
	(error "unformat->proc: unknown options: ~j" opts)
	(values anywhere? unfs))))

(define (unformat->proc str . opts)
  (bind ((anywhere? unfs (parse-unformat-opts opts))
	 (structure (parse-format-string str))
	 (handler '())
	 (pat (cons 'seq
		    (map (lambda (elem)
			   (if (string? elem)
			       elem
			       (bind ((pat unf (unfmtch->pat elem unfs)))
				 (set! handler
				       (cons unf handler))
				 (list 'save pat))))
			 structure)))
	 (matcher (reg-expr->proc
		   (if anywhere?
		       pat
		       (list 'entire pat)))))
    (if (null? handler)
	(error "unformat->proc: no format specifiers in ~s" str))
    (if anywhere?
	(mk-unformatter matcher (reverse handler))
	(mk-unformatter-exact matcher (reverse handler)))))

(define (mk-unformatter matcher unformatters)
  (lambda (str)
    (bind ((s e #rest ps (matcher str)))
      (if s
	  (list->values
	   (cons* s e (map (lambda (proc arg)
			     (proc arg))
			   unformatters
			   ps)))
	  (values)))))

(define (mk-unformatter-exact matcher unformatters)
  (lambda (str)
    (bind ((s e #rest ps (matcher str)))
      (if s
	  (list->values
	   (map (lambda (proc arg)
		  (proc arg))
		unformatters
		ps))
	  (values)))))

(define (add-global-unformatter! unf)
  (set! *global-unformatters* 
	(cons unf (delq (assq (car unf) *global-unformatters*)))))

;;; `ch' should be a character,
;;; `proc' a procedure of one argument which will be called
;;; with the info from parsing the format string (ie, an info vector)
;;; it should return two values -- first, a regex pattern term,
;;; and second, a procedure to consume the result and generate
;;; the parsed answer

(define (make-unformatter ch proc)
  (cons ch proc))

(define (unfmtch->pat info unfs)
  (let* ((ch (if (char? info)
		 info
		 (vector-ref info 0)))
	 (x (assq ch unfs)))
    (if x
	((cdr x) (if (char? info) $default-info info))
	(error "unknown unformat char: ~s" ch))))
