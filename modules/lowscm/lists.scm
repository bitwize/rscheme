#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/lowscm/lists.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.8
 | File mod date:    1999-01-23 15:35:39
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  low-scheme
 |
 | Purpose:          general higher-level list operations
 `------------------------------------------------------------------------|#

;;; Dylan's `size' may return #f for collections of unbounded size
;;; The behavior of Scheme's `length' is unspecified for a cyclic list

;;; a 200MHz PPro can zip through 2300 pairs in about 1ms;
;;; hence, we'll do the work in chunks of 1000 pairs
;;; the first two chunks are uninterruptable because we do
;;; a forward jump

(define-glue (length lst)
  literals: ((& <improper-list>))
{
  obj n = ZERO;
  obj l = lst;

  while (PAIR_P( l ) && FX_LT( n, int2fx( 1000 )))
    {
      l = pair_cdr( l );
      n = ADD1( n );
    }
  if (NULL_P( l ))
    {
      REG0 = n;
      RETURN1();
    }
  else
    {
      /* need to keep looking, or it was an error */
      REG0 = lst;
      REG1 = n;
      REG2 = l;
      JUMP( 3, list_length_cont );
    }
}
("list_length_cont" 
{
  obj n = REG1;
  obj l = REG2;
  int k = 0;

  if (!PAIR_P( l ))
    {
      raise_error( make3( TLREFB(0), NIL_OBJ, REG0, l ) );
    }
  for (k=0; PAIR_P( l ) && (k < 1000); k++)
    {
      l = pair_cdr( l );
      n = ADD1( n );
    }
  if (NULL_P( l ))
    {
      REG0 = n;
      RETURN1();
    }
  else
    {
      /* need to keep looking, or it was an error */
      REG0 = lst;
      REG1 = n;
      REG2 = l;
      BJUMP( 3, list_length_cont );
    }
}))

(define (call-with-list-extending (proc <function>))
  (let ((first #f)
	((last <pair>) (cons 0 '())))
    (set! first last)
    (let ((result (proc (lambda (item)
			  (let (((cell <pair>) (cons item '())))
			    (set-cdr! last cell)
			    (set! last cell)
			    item)))))
      (values (cdr first) result))))

(define (select pred lst)
  (if (pair? lst)
      (let ((first #f)
	    ((last <pair>) (cons 0 '())))
	(set! first last)
	(let-syntax ((add! (syntax-form (item)
			     (let (((cell <pair>) (cons item '())))
			       (set-cdr! last cell)
			       (set! last cell)))))
	  (let loop (((l <pair>) lst))
	    (let ((elem (car l)))
	      (if (pred elem)
		  (add! elem))
	      (if (pair? (cdr l))
		  (loop (cdr l))
		  (cdr first))))))
      '()))

(define (range (n <fixnum>))
  (let loop (((i <fixnum>) n) (r '()))
    (if (eq? i 0)
	r
	(let (((j <fixnum>) (sub1 i)))
	  (loop j (cons j r))))))

(define (list . items)
  items)

(define (reverse lst)
  (if (pair? lst)
      (let loop (((l <pair>) lst) (r '()))
	(if (pair? (cdr l))
	    (loop (cdr l) (cons (car l) r))
	    (if (null? (cdr l))
		(cons (car l) r)
		(error "reverse: not a list: ~s" lst))))
      '()))

(define (reverse! lst)
  (if (pair? lst)
      (let loop ((next lst)
		 (prev '()))
	(if (pair? next)
	    (let ((n (cdr next)))
	      (set-cdr! next prev)
	      (loop n next))
	    prev))
      '()))

(define (delq item list)
  (if (pair? list)
      (let ((r (delq item (cdr list))))
	(if (eq? (car list) item)
	    r
	    (if (eq? r (cdr list))
		list
		(cons (car list) r))))
      list))

(define (delq! item list)
  (let loop ((prev #f) (l list))
    (if (pair? l)
	(let (((l <pair>) l))
	  (if (eq? (car l) item)
	      (if prev
		  (begin
		    (set-cdr! prev (cdr l))
		    (loop prev (cdr l)))
		  (begin
		    (set! list (cdr l))
		    (loop #f (cdr l))))
	      (loop l (cdr l))))
	(if prev
	    list
	    '()))))
