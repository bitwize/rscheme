#|------------------------------------------------------------*-Scheme-*--|
 | File:	    modules/lowscm/keywords.scm
 |
 |          Copyright (C)1998 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |	    See <http://www.rscheme.org/> for the latest info.
 |
 | File version:     1.2
 | File mod date:    2003-10-13 13:02:52
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  low-scheme
 |
 | Purpose:          keyword list processing support procedures
 `------------------------------------------------------------------------|#

(define-class <keyword-list-malformed> (<condition>)
  kle-keyword-list)

(define-method display-object ((self <keyword-list-malformed>) port)
  (__format port "120-279F Keyword list is not properly formed\n")
  (__format port "         given: ~#@*60s\n" (kle-keyword-list self))
  (__format port "         at position ")
  (let loop ((l (kle-keyword-list self))
             (i 0))
    (cond
     ((null? l)
      (__format port "?\n"))                ;; strange... no error now!
     ((not (pair? l))
      (__format port "~d, ~#@*50s is not a pair\n" i l))
     ((not (keyword? (car l)))
      (__format port "~d, ~#@*50s is not a keyword\n" i (car l)))
     ((not (pair? (cdr l)))
      (__format port "~d, after ~s, ~#@*50s is not a list\n" 
                i (car l) (cdr l)))
     (else
      (loop (cddr l) (add1 (add1 i)))))))

;;;
;;;  parses a list representation alternating keywords and values
;;;  into a vector suitable for vassq operations (ie, also alternating
;;;  keywords and values).
;;;
;;;  signals the error <keyword-list-malformed> if the input is not a 
;;;  proper list, the keys are not all keywords, or there is a missing
;;;  value for a keyword (ie, the list has an odd length)
;;;
;;;  doesn't terminate on a cyclic structure

(define (keyword-value-list->vector kv-list)
  (let ((q (make-dequeue)))
    (let loop ((p kv-list))
      (if (pair? p)
	  (begin
	    (if (keyword? (car p))
		(if (pair? (cdr p))
		    (begin
		      (dequeue-push-back! q (car p))
		      (dequeue-push-back! q (cadr p))
		      (loop (cddr p)))
		    (signal
		     (make <keyword-list-malformed>
			   kle-keyword-list: kv-list)))
		(signal
		 (make <keyword-list-malformed>
		       kle-keyword-list: kv-list))))
	  (if (null? p)
	      (dequeue-state q)
	      (signal
	       (make <keyword-list-malformed>
		     kle-keyword-list: kv-list)))))))


;;;
;;;  construct a list of "remainder" keyword/values, where the "used"
;;;  entries have a key of #f
;;;

(define (remainder->list (v <vector>))
  (let loop ((r '())
	     ((k <fixnum>) (gvec-length v)))
    (if (eq? k 0)
	r
	(let (((i <fixnum>) (fixnum- k 2)))
	  (loop (if (vector-ref v i)
		    (cons* (vector-ref v i)
			   (vector-ref v (add1 i))
			   r)
		    r)
		i)))))

;;;  look up the given keyword (`kwd') in the given 
;;;  keyword/value vector (`v') and invoke the `found-proc'
;;;  with the associated value if present (after clobbering
;;;  all occurrences of the key in the vector with #f).  If
;;;  not present, invoke the `notfound-proc' with no arguments.

(define (using-keyword-value kwd (v <vector>) found-proc notfound-proc)
  (let ((i (vassq kwd v)))
    (if (fixnum? i)
	(let ((a (gvec-ref v i)))
	  (let loop ((i i))
	    (if (fixnum? i)
		(begin
		  (gvec-set! v (sub1 i) #f)
		  (loop (vassq kwd v)))
		(found-proc a))))
	(notfound-proc))))

(define (get-keyword-value (kvv <vector>) keyword default)
  (using-keyword-value
   keyword
   kvv
   (lambda (item)
     item)
   (lambda ()
     default)))

;;;  check to make sure all the keywords in the given 
;;;  keyword-value vector have been consumed by the
;;;  keyword processor

(define (check-all-keywords-used (v <vector>) fn-name)
  (let loop (((i <fixnum>) 0))
    (if (eq? i (vector-length v))
	(values)
	(if (vector-ref v i)
	    (some-keywords-not-used v fn-name)
	    (loop (fixnum+ i 2))))))

(define (some-keywords-not-used (v <vector>) fn-name)
  (let loop (((i <fixnum>) 0)
	     (r '()))
    (if (eq? i (vector-length v))
	(error "~s: excess keywords supplied: ~j" fn-name (reverse r))
	(if (vector-ref v i)
	    (loop (fixnum+ i 2) (cons (vector-ref v i) r))
	    (loop (fixnum+ i 2))))))

;;;


;;;
;;;  Parse a keyword list into an association list
;;;
;;;  (keyword-list->assoc-list '(a: 3 b: 4)) ==> '((a: 3) (b: 4))
;;;  (keyword-list->symbol-assoc-list '(a: 3 b: 4)) ==> '((a 3) (b 4))
;;;

(define (keyword-list->assoc-list lst)
  (map-keyword-list lst (lambda (k v) (cons k v))))

(define (keyword-list->symbol-assoc-list lst)
  (map-keyword-list lst (lambda (k v) (cons (keyword->symbol k) v))))

(define (map-keyword-list lst proc)
  (let loop ((l lst)
             (r '()))
    (if (null? l)
        (reverse! r)
        (if (and (pair? l)
                 (keyword? (car l))
                 (pair? (cdr l)))
            (loop (cddr l)
                  (cons (proc (car l) (cadr l)) r))
            (error (make <keyword-list-malformed>
                         kle-keyword-list: lst))))))

