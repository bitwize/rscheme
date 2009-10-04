
(define-class <bezier-curve> (<geometric>) :bvec
  #|
      0      4      8
      +------+------+
      | p0.x | p0.y |
      +------+------+
      | p1.x | p1.y |
      +------+------+
      | p2.x | p2.y |
      +------+------+
      | p3.x | p3.y |
      +------+------+

     32     36     40
      +------+------+
      | a.x  | a.y  |
      +------+------+
      | b.x  | b.y  |
      +------+------+
      | c.x  | c.y  |
      +------+------+
      | d.x  | d.y  |
      +------+------+

     64 bytes total
  |#)

(define-method control-points ((self <bezier-curve>))
  (values (make-point (bvec-read-float-32 self 0) 
		      (bvec-read-float-32 self 4))
	  (make-point (bvec-read-float-32 self 8)
		      (bvec-read-float-32 self 12))
	  (make-point (bvec-read-float-32 self 16)
		      (bvec-read-float-32 self 20))
	  (make-point (bvec-read-float-32 self 24)
		      (bvec-read-float-32 self 28))))

(define-method flatten ((self <bezier-curve>) (flatness <real>))
  ...)

(define-glue (flatten (curv <bezier-curve>) (flatness <double-float>))
{

})
