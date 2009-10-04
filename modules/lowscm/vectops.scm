#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/lowscm/vectops.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.12
 | File mod date:    1998-12-19 22:53:17
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  low-scheme
 |
 | Purpose:          vector applicative and associative operations
 `------------------------------------------------------------------------|#

;;;
;;;  applicative operations (vector-for-each and vector-map)
;;;

(define-syntax (vfe proc i count . args)
  (let (((n <fixnum>) (sub1 count)))
    (if (eq? n -1)
	(values)
	(let loop (((i <fixnum>) 0))
	  (if (eq? i n)
	      (proc . args)
	      (begin
		(proc . args)
		(loop (add1 i))))))))

(define (min2 (a <fixnum>) (b <fixnum>))
  (if (fixnum<? a b)
      a
      b))

(define (vectors-min-len (vs <pair>))
  (let loop (((l <pair>) vs)
	     ((min <fixnum>) (gvec-length (car vs))))
    (let* (((len <fixnum>) (gvec-length (car l)))
	   ((new-min <fixnum>) (if (fixnum<? len min)
				   len
				   min)))
      (if (pair? (cdr l))
	  (loop (cdr l) new-min)
	  new-min))))


(define (map-vector-ref vector-list (index <fixnum>))
  (if (null? vector-list)
      '()
      (let (((v <vector> :trust-me) (car vector-list)))
	(cons (gvec-ref v index) (map-vector-ref (cdr vector-list) index)))))

(define (vector-for-each/i proc (v1 <vector>) . more-vs)
  (assert (every? vector? more-vs))
  (if (null? more-vs)
      (vfe proc i (gvec-length v1) i (vector-ref v1 i))
      (if (null? (cdr more-vs))
	  (let (((v2 <vector>) (car more-vs)))
	    (vfe proc i (min2 (gvec-length v1) (gvec-length v2))
		 i (vector-ref v1 i) (vector-ref v2 i)))
	  (vfe apply* i
	       (vectors-min-len (cons v1 more-vs))
	       i v1
	         (map-vector-ref more-vs i)
		 proc))))

(define (vector-for-each proc (v1 <vector>) . more-vs)
  (assert (every? vector? more-vs))
  (if (null? more-vs)
      (vfe proc i (gvec-length v1) (vector-ref v1 i))
      (if (null? (cdr more-vs))
	  (let (((v2 <vector>) (car more-vs)))
	    (vfe proc i (min2 (gvec-length v1) (gvec-length v2))
		 (vector-ref v1 i) (vector-ref v2 i)))
	  (vfe apply* i
	       (vectors-min-len (cons v1 more-vs))
	       (gvec-ref v1 i)
	       (map-vector-ref more-vs i)
	       proc))))

;;;
;;;

(define-syntax (vm proc i count . args)
  (let (((n <fixnum>) count))
    (if (eq? n 0)
	'#()
	(let (((r <vector>) (gvec-alloc <vector> n #f))
	      ((n <fixnum>) n))
	  (let loop (((i <fixnum>) 0))
	    (if (fixnum<? i n)
		(begin
		  (vector-set! r i (proc . args))
		  (loop (add1 i)))
		r))))))

(define (vector-map proc (v1 <vector>) . more-vs)
  (assert (every? vector? more-vs))
  (if (null? more-vs)
      (vm proc i (gvec-length v1) (vector-ref v1 i))
      (if (null? (cdr more-vs))
	  (let (((v2 <vector>) (car more-vs))
		((v1 <vector>) v1))
	    (vm proc i (min2 (gvec-length v1) (gvec-length v2))
		(vector-ref v1 i) (vector-ref v2 i)))
	  (vm apply* i (vectors-min-len (cons v1 more-vs))
	      (vector-ref v1 i)
	      (map-vector-ref more-vs i)
	      proc))))

;;;
;;;  boolean vector arithmetic
;;;

(define-glue (vector-bool-op vec1 vec2 op newv)
{
char c;
int b, opcode = fx2int(op);
UINT_32 i, len;
obj dest;

    COUNT_ARGS(4);
    if(!(VECTOR_P(vec1) && VECTOR_P(vec2) 
    	 && OBJ_ISA_FIXNUM(op) && OBJ_ISA_BOOLEAN(newv)))
    {
	scheme_error( "vector-bool-op: bad arg", 0 );
    }
    len = SIZEOF_PTR(vec1);
    if (SIZEOF_PTR(vec2) < len)
	len = SIZEOF_PTR(vec2);
	
    if (truish(newv))
	dest = alloc( len, vector_class );
    else
	dest = vec1;
    for (i=0; i<len; i+=SLOT(1))
    {
	if (EQ(gvec_read(vec1,i),FALSE_OBJ))
	{
	    if (EQ(gvec_read(vec2,i),FALSE_OBJ))
		b = opcode & 1;
	    else
		b = opcode & 2;
	}
	else
	{
	    if (EQ(gvec_read(vec2,i),FALSE_OBJ))
		b = opcode & 4;
	    else
		b = opcode & 8;
	}
	if (truish(newv))
  	    gvec_write_init_non_ptr( dest, i, b ? TRUE_OBJ : FALSE_OBJ );
        else
	    gvec_write_non_ptr( dest, i, b ? TRUE_OBJ : FALSE_OBJ );
    }
    REG0 = dest;
    RETURN(1);
})

(define (vector-or vec1 vec2)     (vector-bool-op vec1 vec2 #b1110 #t))
(define (vector-or! vec1 vec2)    (vector-bool-op vec1 vec2 #b1110 #f))
(define (vector-and vec1 vec2)    (vector-bool-op vec1 vec2 #b1000 #t))
(define (vector-and! vec1 vec2)   (vector-bool-op vec1 vec2 #b1000 #f))
(define (vector-andn  vec1 vec2)  (vector-bool-op vec1 vec2 #b0001 #t))
(define (vector-andn! vec1 vec2)  (vector-bool-op vec1 vec2 #b0001 #f))
(define (vector-xor vec1 vec2)    (vector-bool-op vec1 vec2 #b0110 #t))
(define (vector-xor! vec1 vec2)   (vector-bool-op vec1 vec2 #b0110 #f))

;;;
;;;  associative operations (vassq and vmemq)
;;;

(%strategy ccode
(define (vassq key (v <vector>))
  (let (((n <fixnum>) (sub1 (gvec-length v))))
    (let loop (((i <fixnum>) 0))
      (if (fixnum<? i n)
	  (if (eq? (gvec-ref v i) key)
	      (add1 i)
	      (loop (fixnum+ i 2)))
	  #f))))

(define (vmemq key (v <vector>))
  (let (((n <fixnum>) (gvec-length v)))
    (let loop (((i <fixnum>) 0))
      (if (eq? i n)
	  #f
	  (if (eq? (vector-ref v i) key)
	      i
	      (loop (add1 i)))))))
)

(define (vinsert2 (vec <vector>) k v)
  (vector-append (vector k v) vec))

(define (vinsert (vec <vector>) ent)
  (vector-append (vector ent) vec))

;;; this is pretty darn inefficient, but that's one
;;; of the costs of using vectors (though it could be
;;; made slightly better)

(define (vdelq item (vec <vector>))
  (let loop ((v vec))
    (let ((i (vmemq item v)))
      (if i
	  (loop (vector-append 
		 (subvector v 0 i)
		 (subvector v (add1 i))))
	  v))))

(define (vlast (vec <vector>))
  (let ((n (gvec-length vec)))
    (if (eq? n 0)
	(signal (make <no-last-element>
		      collection: vec))
	(vector-ref vec (sub1 n)))))
	
(define (vector->string (vec <vector>))
  (list->string (vector->list vec)))
