#| test stuff...

(define (tg)
  (let* ((g (make-symbol-table)))
    (table-insert! g 'e (list
			 (make <rule>
			       name: 'e
			       action: (lambda (a b c)
					 (list '+ a c))
			       rhs: (vector 't
					    (match '+)
					    'e))
			 (make <rule>
			       name: 'e
			       action: identity
			       rhs: (vector 't))))
    (table-insert! g 't (list
			 (make <rule>
			       name: 't
			       action: identity
			       rhs: (vector match-num))
			 (make <rule>
			       name: 't
			       action: identity
			       rhs: (vector match-id))))
    g))

(define (t #optional (inp default: '#(x + 1 + y)))
  (parse inp (tg) 'e))

(define (match-num tok)
  (values tok (number? tok)))

(define (match-id tok)
  (values tok (symbol? tok)))

(define (match kwd)
  (let ((p (lambda (tok)
	     (values '+ (eq? tok '+)))))
    (set-property! p 'matches (format #f "[~a]" kwd))
    p))
(set-property! match-num 'matches "NUM")
(set-property! match-id 'matches "ID")
|#

;;;----------------------------------------------------------------------

(define-thread-var *radix* 10)

(define (with-radix R thunk)
  (thread-let ((*radix* R))
   (thunk)))

(define (match-digit-R digit)
  (let ((v (cond
            ((and (char>=? digit #\0) (char<=? digit #\9))
             (- (char->integer digit) (char->integer #\0)))
            ((and (char>=? digit #\a) (char<=? digit #\z))
             (+ 10 (- (char->integer digit) (char->integer #\a))))
            ((and (char>=? digit #\A) (char<=? digit #\Z))
             (+ 10 (- (char->integer digit) (char->integer #\A))))
            (else #f))))
    (if (and v (< v *radix*)) 
	(values v v)
	#f)))

(define (match-digit-10 tok)
  (values (- (char->integer tok)
	     (char->integer #\0))
	  (and (char>=? tok #\0)
	       (char<=? tok #\9))))

(define (match-char ch)
  (let ((p (lambda (tok)
	     (values tok (char-ci=? tok ch)))))
    ;(set-property! p 'matches (format #f "[~a]" ch))
    p))
;(set-property! match-digit-10 'matches 'DIGIT-10)
;(set-property! match-digit-R 'matches 'DIGIT-R)

;;; construct the built-in number grammar

(define (reduce-small-10 lst)
  (reduce (lambda ((l <fixnum>) (r <fixnum>))
	    (fixnum+ (fixnum* l 10) r))
	  0
	  lst))

(define (reduce-R lst)
  (if (< (length lst) 9)
      (reduce (let ((R *radix*))
		(lambda ((l <fixnum>) (r <fixnum>))
		  (fixnum+ (fixnum* l R) r)))
	      0
	      lst)
      (raw-int-64->integer
       (reduce (let ((R (fixnum->raw-int-64 *radix*)))
		 (lambda ((l <long-int>) (r <fixnum>))
		   (raw-int-64+ 
		    (raw-int-64* l R)
		    (fixnum->raw-int-64 r))))
	       (fixnum->raw-int-64 0)
	       lst))))

(define (make-number-grammar)
  (let ((g (make-symbol-table)))
    ;
    ;  --- non-empty sequences of digits ---
    ;
    (add-rule! g 'digit-10 identity match-digit-10)
    (add-rule! g 'digit-10-list list 'digit-R)
    (add-rule! g 'digit-10-list cons 'digit-R 'digit-R-list)
    ;
    (add-rule! g 'digit-R identity match-digit-R)
    (add-rule! g 'digit-R-list list 'digit-R)
    (add-rule! g 'digit-R-list cons 'digit-R 'digit-R-list)
    ;
    ;  --- numbers in arabic notation ---
    ;
    (add-rule! g 'uinteger-10 reduce-small-10 'digit-10-list)
    (add-rule! g 'uinteger-R reduce-R 'digit-R-list)
    ;
    ;  --- modifiers ---
    ;
    (add-rule! g 'exponent-marker identity (match-char #\e))
    (add-rule! g 'exponent-marker identity (match-char #\s))
    (add-rule! g 'exponent-marker identity (match-char #\f))
    (add-rule! g 'exponent-marker identity (match-char #\d))
    (add-rule! g 'exponent-marker identity (match-char #\l))
    ;
    (add-rule! g 'sign (lambda (ch) '#\-) (match-char #\-))
    (add-rule! g 'sign (lambda (ch) '#\+) (match-char #\+))
    (add-rule! g 'sign (lambda () '#\+))
    ;
    (add-rule! g 'exactness (lambda () 'exact))
    (add-rule! g 'exactness (lambda () 'exact)
	       (match-char #\#) (match-char #\e))
    (add-rule! g 'exactness (lambda () 'inexact)
	       (match-char #\#) (match-char #\i))
    ;
    ;  --- decimal and exponential notation ---
    ;
    (add-rule! g 'decimal-R (lambda (pre dp post suffix)
			      (let ((d (list 'dp pre post)))
				(if (eq? suffix 0)
				    d
				    (list 'e d suffix))))
	       'digit-R-list
	       (match-char #\.)
	       'digit-R-list
	       'suffix)
    ;
    (add-rule! g 'decimal-R (lambda (pre suffix)
			      (if suffix
				  (exact->inexact
				   (* pre (expt 10 suffix)))
				  pre))
	       'uinteger-R
	       'suffix)
    ;
    (add-rule! g 'suffix (lambda () #f))
    (add-rule! g 'suffix (lambda (expm sign exp)
			   (if (eq? sign '#\-)
			       (- exp)
			       exp))
	       'exponent-marker
	       'sign
	       'uinteger-10)
    ;
    ;  --- the rest of the numeric tower ---
    ;
    (add-rule! g 'ureal-R identity 'uinteger-R)
    (add-rule! g 'ureal-R identity 'decimal-R)
    ;
    (add-rule! g 'real-R (lambda (sign num)
			   (if (eq? sign #\-)
			       (- num)
			       num))
	       'sign
	       'ureal-R)
    ;
    ;  ... rational numbers ...
    ;
    (add-rule! g 'ureal-R (lambda (n sl d)
			    (/ n d))
	       'uinteger-R
	       (match-char #\/)
	       'uinteger-R)
    ;
    ;  ... complex numbers ...
    ;
    (add-rule! g 'complex-R identity 'real-R)
    (add-rule! g 'complex-R (lambda (magnitude at angle)
			      (make-polar magnitude angle))
	       'real-R
	       (match-char #\@)
	       'real-R)
    (add-rule! g 'complex-R (lambda (re plus im i)
			      (make-rectangular re im))
	       'real-R
	       (match-char #\+)
	       'ureal-R
	       (match-char #\i))
    (add-rule! g 'complex-R (lambda (re minus im i)
			      (make-rectangular re (- im)))
	       'real-R
	       (match-char #\-)
	       'ureal-R
	       (match-char #\i))
    (add-rule! g 'complex-R (lambda (re plus i)
			      (make-rectangular re 1))
	       'real-R
	       (match-char #\+)
	       (match-char #\i))
    (add-rule! g 'complex-R (lambda (re minus i)
			      (make-rectangular re -1))
	       'real-R
	       (match-char #\-)
	       (match-char #\i))
    (add-rule! g 'complex-R (lambda (plus i)
			      (make-rectangular 0 1))
	       (match-char #\+)
	       (match-char #\i))
    (add-rule! g 'complex-R (lambda (minus i)
			      (make-rectangular 0 -1))
	       (match-char #\-)
	       (match-char #\i))
    ;
    ;  --- entry point ---
    ;
    (add-rule! g 'number (lambda (p c) c) 'prefix-R 'complex-R)
    ;
    (add-rule! g 'prefix-R (lambda ()))
    (add-rule! g 'prefix-R (lambda (sh o) (set! *radix* 8))
	       (match-char #\#)
	       (match-char #\o))
    (add-rule! g 'prefix-R (lambda (sh o) (set! *radix* 16))
	       (match-char #\#)
	       (match-char #\x))
    (add-rule! g 'prefix-R (lambda (sh o) (set! *radix* 10))
	       (match-char #\#)
	       (match-char #\d))
    (add-rule! g 'prefix-R (lambda (sh o) (set! *radix* 2))
	       (match-char #\#)
	       (match-char #\b))
    ;
	       
    ; need to support having the prefix in here, but putting in 
    ; the prefix should change the radix
    ; (really need to make radix part an inherited attribute)
    g))

(define *number-grammar* (make-number-grammar))

(define (parse-num str)
  (parse (list->vector (string->list str))
	 *number-grammar*
	 'number))

