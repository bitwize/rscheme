#|------------------------------------------------------------*-Scheme-*--|
 | File:	    modules/corelib/complex.scm
 |
 |          Contributed by HIROSHI OOTA <oota@POBoxes.com>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest info.
 |
 | File version:     1.3
 | File mod date:    2003-10-22 18:05:29
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  corelib
 |
 | Purpose:          complex number handling
 `------------------------------------------------------------------------|#

(define-safe-glue (make-rectangular (re <number>) (im <number>))
{
#if !FULL_NUMERIC_TOWER
    scheme_error( "complex.scm:make-rectanglar:~s: function stubbed out", 1, int2fx(__LINE__) ); 
    REG0 = ZERO;
    RETURN1();
#else
  REG0 = make_complex_obj(re, im);
  RETURN1();
#endif
})


(define-generic-function real-part)
(define-generic-function imag-part)

(define-method real-part ((self <rect-complex>)) (re self))
(define-method imag-part ((self <rect-complex>)) (im self))

(define-method real-part ((self <real>)) self)
(define-method imag-part ((self <real>)) 0)

;;;

(define (make-polar magnitude angle)
  (cond
   ((base=? angle 0)
    magnitude)
   ((base=? angle 90)
    (make-rectangular 0 magnitude))
   ((base=? angle 180)
    (base- 0 magnitude))
   ((base=? angle 270)
    (make-rectangular 0 (base- 0 magnitude)))
   (else
    (let (((a <double-float>) (float* 0.0174532925199432958 
                                      (as-raw-float angle)))
          ((m <double-float>) (as-raw-float magnitude)))
      (make-rectangular (float* m (float-cos a))
                        (float* m (float-sin a)))))))
