#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/lowscm/finding.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.4
 | File mod date:    1997-11-29 23:10:37
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  low-scheme
 |
 | Purpose:          functions to find elements in lists
 `------------------------------------------------------------------------|#

(define (list-error proc lst)
  (type-error proc 1 lst "not a proper list"))

(define (assoc-error proc lst)
  (type-error proc 1 lst "not an association list"))


(define-syntax (%assoc proc test search list)
    (let loop ((l list))
	(if (pair? l)
	    (let (((l <pair>) l))
	      (let ((x (car l)))
		(if (pair? x)
		    (let (((x <pair>) x))
		      (if (test (car x) search)
			  x
			  (loop (cdr l))))
		    (assoc-error proc list))))
	    (if (null? l)
		#f
		(list-error proc list)))))

(define-syntax (%member proc test search list)
    (let loop ((l list))
	(if (pair? l)
	    (if (test (car l) search)
		l
		(loop (cdr l)))
	    (if (null? l)
		#f
		(list-error proc list)))))

(define-glue (assq item list)
 literals: ((& assq) (& assoc-error) (& list-error))
{
  COUNT_ARGS(2);
  REG2 = REG1;  /* copy base list into REG2 */
  JUMP(3,assq_2);
}
("assq_2" {
unsigned i;
obj entry;

  for (i=0; i<20; i++)
    {
      if (!PAIR_P(REG1))
	{
	  if (EQ(REG1,NIL_OBJ))
	    {
	      REG0 = FALSE_OBJ;
	      RETURN1();
	    }
	  else
	    {
	      REG0 = TLREF(0);
	      APPLY(2,TLREF(2));
	    }
	}
      entry = pair_car(REG1);
      if (!PAIR_P(entry))
	{
	  REG0 = TLREF(0);
	  REG1 = entry;
	  APPLY(2,TLREF(1));
	}
      else if (EQ(pair_car(entry),REG0))
	{
	  REG0 = entry;
	  RETURN1();
	}
      REG1 = pair_cdr(REG1);
    }
/* loop for some more */
    BJUMP(2,assq_2);
}))

#|(define (assq item list)
    (%assoc assq eq? item list))
|#	
(define (assv item list)
    (%assoc assoc eqv? item list))
    
(define (assoc item list)
    (%assoc assoc equal? item list))
    
(define (memq item list)
    (%member memq eq? item list))

(define (memv item list)
    (%member memv eqv? item list))

(define (member item list)
    (%member member equal? item list))
