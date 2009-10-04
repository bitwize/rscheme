;;;

(define-method line-vector ((self <line>))
  (values (- (x (to self)) (x (from self)))
	  (- (y (to self)) (y (from self)))))

(define-method point-on ((l <line>) (t <real>))
  (bind ((dx dy (line-vector l)))
    (make-point (+ (x (from l)) (* dx t))
		(+ (y (from l)) (* dy t)))))

(define-method distance^2 ((a <point>) (b <point>))
  (let ((dx (- (x a) (x b)))
	(dy (- (y a) (y b))))
    (+ (* dx dx) (* dy dy))))

;;  distance: <geometric> <point> => <real> <point>

(define-method distance ((a <point>) (b <point>))
  (values (sqrt (distance^2 a b)) a))

(define-method intersection-parameter ((l <line>) (p <point>))
  (bind ((dx dy (line-vector l))
	 (p-ax (- (x p) (x (from l))))
	 (p-ay (- (y p) (y (from l)))))
    (/ (+ (* p-ax dx) (* p-ay dy))
       (+ (* dx dx) (* dy dy)))))

;; returns the distance to a line segment
;; in particular, this may involve the distance to
;; one of the end points

(define-method distance ((l <line>) (p <point>))
  (let* ((t (intersection-parameter l p))
	 (a (point-on l (min (max t 0) 1))))
    (distance a p)))

(define-class <bezier-curve> (<curve>)
  (start-point type: <point>)
  (first-handle type: <point>)
  (second-handle type: <point>)
  (end-point type: <point>)
  (coeff-cache type: <vector> init-value: '#()))

(define-method clear-coefficient-cache! ((self <bezier-curve>))
  (set-coeff-cache! self '#())
  (values))

(define (curv x0 y0 x1 y1 x2 y2 x3 y3)
  (make <bezier-curve>
	start-point: (make-point x0 y0)
	first-handle: (make-point x1 y1)
	second-handle: (make-point x2 y2)
	end-point: (make-point x3 y3)))

(define (reverse-bezier (self <bezier-curve>))
  (make <bezier-curve>
        start-point: (end-point self)
        first-handle: (second-handle self)
        second-handle: (first-handle self)
        end-point: (start-point self)))

;;; The radial error in this approximation will be about 0.0273% of the 
;;; circle's radius. [comp.graphics.algorithms FAQ 2003-02-15]

(define (unit-arc-bezier)    ; build a 90-deg unit CCW arc using a bezier curve
  (let ((a (* (/ 4 3) (- (sqrt 2) 1))))
    (make <bezier-curve>
          start-point: (make-point 1 0)
          first-handle: (make-point 1 a)
          second-handle: (make-point a 1)
          end-point: (make-point 0 1))))

;;;
;;;  Return the four "circular" bezier arcs which form a complete
;;;  circle.  The circle is counterclockwise, and the segments
;;;  returned in the order:
;;;      [0]  0-90 degrees (standard angle, i.e., the NE quadrant)
;;;      [1]  90-180 degrees (i.e., the NW quadrant)
;;;      [2]  180-270 degrees (i.e., the SW quadrant)
;;;      [3]  270-360 degrees (i.e., the SE quadrant)
;;;

(define (unit-circle-beziers)
  (let-syntax ((bez (syntax-form (a b c d)
                      (make <bezier-curve>
                            start-point: a
                            first-handle: b
                            second-handle: c
                            end-point: d)))
               (pt (syntax-form (x y)
                     (make-point x y))))
    (let* ((a (* (/ 4 3) (- (sqrt 2) 1)))
           (a- (- a))
           (E (pt 1 0))
           (N (pt 0 1))
           (W (pt -1 0))
           (S (pt 0 -1)))
      (list (bez E (pt 1 a) (pt a 1) N)
            (bez N (pt a- 1) (pt -1 a) W)
            (bez W (pt -1 a-) (pt a- -1) S)
            (bez S (pt a -1) (pt 1 a-) E)))))
                 
;;;  Return four quarter-cycle segments of a sine wave
;;;  (if `cycle:' is specified, returns four quarter-cycles
;;;  other than the one that starts at the origin)
;;;
;;;  The default values for the parameters `a' and `b' are pretty good
;;;  (max error is ~0.001 on the unit curve), but nevertheless determined
;;;  by eye

(define (sine-wave-beziers #key
                           (cycle default: 0)
                           (a default: 0.6) 
                           (b default: 0.53))
  (let-syntax ((bez (syntax-form (a b c d)
                      (make <bezier-curve>
                            start-point: a
                            first-handle: b
                            second-handle: c
                            end-point: d)))
               (pt (syntax-form (x y)
                     (make-point x y))))
    (let* ((x (if (= cycle 0)
                  0
                  (* cycle $Pi 2)))
           (x0 (+ x (/ $Pi 2)))
           (x1 (+ x $Pi))
           (x2 (+ x (/ (* $Pi 3) 2)))
           (x3 (+ x (* $Pi 2))))
      (list (bez (pt x 0) (pt (+ x a) a) (pt (- x0 b) 1) (pt x0 1))
            (bez (pt x0 1) (pt (+ x0 b) 1) (pt (- x1 a) a) (pt x1 0))
            (bez (pt x1 0) (pt (+ x1 a) (- a)) (pt (- x2 b) -1) (pt x2 -1))
            (bez (pt x2 -1) (pt (+ x2 b) -1) (pt (- x3 a) (- a)) (pt x3 0))))))

#|
 Solve[ { p1 == p0 + c/3,
	  p2 == p1 + (c + b) / 3,
	  p3 == p0 + c + b + a },
        {ax,ay,bx,by,cx,cy,dx,dy} ]
 {{ax -> -x0 + 3 x1 - 3 x2 + x3, 
   ay -> -y0 + 3 y1 - 3 y2 + y3, 
   bx -> 3 x0 - 6 x1 + 3 x2, by -> 3 y0 - 6 y1 + 3 y2, 
   cx -> -3 x0 + 3 x1, cy -> -3 y0 + 3 y1}}
|#

(define-method coeffs ((self <bezier-curve>))
  (if (eq? (vector-length (coeff-cache self)) 0)
      (let (((p0 <point>) (start-point self))
	    ((p1 <point>) (first-handle self))
	    ((p2 <point>) (second-handle self))
	    ((p3 <point>) (end-point self)))
	(set-coeff-cache! 
	 self
	 (vector (+ (- (x p0)) (* 3 (x p1)) (* -3 (x p2)) (x p3))
		 (+ (- (y p0)) (* 3 (y p1)) (* -3 (y p2)) (y p3))
		 (+ (* 3 (x p0)) (* -6 (x p1)) (* 3 (x p2)))
		 (+ (* 3 (y p0)) (* -6 (y p1)) (* 3 (y p2)))
		 (+ (* -3 (x p0)) (* 3 (x p1)))
		 (+ (* -3 (y p0)) (* 3 (y p1)))
		 (x p0)
		 (y p0)))
	(coeff-cache self))
      (coeff-cache self)))

(define-method point-on ((c <bezier-curve>) (t <real>))
  (let* ((t^2 (* t t))
	 (t^3 (* t^2 t))
	 ((v <vector>) (coeffs c)))
    (make-point (+ (* t^3 (vector-ref v 0))
		   (* t^2 (vector-ref v 2))
		   (* t (vector-ref v 4))
		   (vector-ref v 6))
		(+ (* t^3 (vector-ref v 1))
		   (* t^2 (vector-ref v 3))
		   (* t (vector-ref v 5))
		   (vector-ref v 7)))))

(define-method tangent-on ((self <bezier-curve>) (t <real>))
  (let* ((t^2 (* t t))
	 ((v <vector>) (coeffs self)))
    (make-size (+ (* 3 t^2 (vector-ref v 0))
                  (* 2 t (vector-ref v 2))
                  (vector-ref v 4))
               (+ (* 3 t^2 (vector-ref v 1))
                  (* 2 t (vector-ref v 3))
                  (vector-ref v 5)))))

(define-method curvature-on ((self <bezier-curve>) (t <real>))
  (let (((v <vector>) (coeffs self)))
    (make-size (+ (* 6 t (vector-ref v 0))
                  (* 2 (vector-ref v 2)))
               (+ (* 6 t (vector-ref v 1))
                  (* 2 (vector-ref v 3))))))

#|
  XXX a bug in the compiler causes this to fail
      when passed a literal `make-point' expression
|#

(define-syntax x-sum
  (syntax-form (p)
    (x p))
  (syntax-form (p q . r)
    (+ (x p) (x-sum q . r))))

(define-syntax y-sum
  (syntax-form (p)
    (y p))
  (syntax-form (p q . r)
    (+ (y p) (y-sum q . r))))

(define-syntax point-count
  (syntax-form (p) 1)
  (syntax-form (p q) 2)
  (syntax-form (p q r) 3)
  (syntax-form (p q r s) 4)
  (syntax-form (p q r s t) 5)
  (syntax-form (p q r s t u . v) (+ 5 (point-count u . v))))

(define (point-average* p . r)
  (let ((n (+ 1 (length r))))
    (make-point (/ (reduce + (x p) (map x r)) n)
                (/ (reduce + (y p) (map y r)) n))))

(define-syntax point-average
  (syntax-form (p . r)
    (make-point (/ (x-sum p . r) (point-count p . r))
                (/ (y-sum p . r) (point-count p . r))))
  (else point-average*))

(define-method subdivide ((c <bezier-curve>))
  ;; page 508
  ;; [p1 p2 p3 p4] -> [p1 l2 l3 j] + [j r2 r3 p4]
  (let* (((p1 <point>) (start-point c))
	 ((p2 <point>) (first-handle c))
	 ((p3 <point>) (second-handle c))
	 ((p4 <point>) (end-point c))
	 ((l2 <point>) (point-average p1 p2))
	 ((h <point>)  (point-average p2 p3))
	 ((l3 <point>) (point-average l2 h))
	 ((r3 <point>) (point-average p3 p4))
	 ((r2 <point>) (point-average h r3))
	 ((j <point>)  (point-average l3 r2)))
    (values (make <bezier-curve>
		  start-point: p1
		  first-handle: l2
		  second-handle: l3
		  end-point: j)
	    (make <bezier-curve>
		  start-point: j
		  first-handle: r2
		  second-handle: r3
		  end-point: p4))))

;;; generalized subdivision (de Casteljau algorithm) for arbitrary `t'

(define-method subdivide-at ((self <bezier-curve>) (t <real>))
  (let ((t1 (- 1 t)))
    (define (weighted-avg a b)
      (make-point (+ (* t1 (x a)) (* t (x b)))
                  (+ (* t1 (y a)) (* t (y b)))))
    ;;
    (let* ((p00 (start-point self))
           (p01 (first-handle self))
           (p02 (second-handle self))
           (p03 (end-point self))
           (p11 (weighted-avg p01 p02))
           (p12 (weighted-avg p02 p03))
           (p21 (weighted-avg p11 p12))
           (p10 (weighted-avg p00 p01))
           (p20 (weighted-avg p10 p11))
           (p30 (weighted-avg p20 p21))
           (p21 (weighted-avg p11 p12))
           (p12 (weighted-avg p02 p03)))
      (values
       (make <bezier-curve>
             start-point: p00
             first-handle: p10
             second-handle: p20 
             end-point: p30)
       (make <bezier-curve>
             start-point: p30
             first-handle: p21
             second-handle: p12
             end-point: p03)))))

(define-method bbox ((self <bezier-curve>) #optional flatness)
  (if flatness
      (flattened-bbox self flatness)
      ;; XXX  Hmmm.  Can the bezier bounding box be computed
      ;;      analytically?  The two dimensions, x(t) and y(t)
      ;;      are independent.  Each will reach a min or max
      ;;      only where the first derivative is zero, which
      ;;      in turn is a quadratic equation and hence has at
      ;;      most two real roots
      (let* (((p0 <point>) (start-point self))
             ((p1 <point>) (first-handle self))
             ((p2 <point>) (second-handle self))
             ((p3 <point>) (end-point self)))
        (let* ((x0 (min (x p0) (x p1) (x p2) (x p3)))
               (x1 (max (x p0) (x p1) (x p2) (x p3)))
               (y0 (min (y p0) (y p1) (y p2) (y p3)))
               (y1 (max (y p0) (y p1) (y p2) (y p3))))
          (bbox-rect x0 y0 x1 y1)))))

(define-method flatness ((self <bezier-curve>))
  ;; Measure the flatness as the largest distance of a control
  ;; point from the straight-line segment 
  (let ((straight (make <line>
                        from: (start-point self)
                        to: (end-point self))))
    (max (distance straight (first-handle self))
         (distance straight (second-handle self)))))
           

;;;

;(define a (curv 0 0 10 100 150 150 100 10))

#|
From: Richard Kinch (truetex@IDT.NET)
Subject: Re: Is it possible to convert Arc to Bezier-curve?
Newsgroups: comp.graphics.algorithms
Date: 1999-12-31 

[...]
> Is it possible to find a formule for intersection
> of Bezier-curve with line if Bezier-curve is not self intersected?

Yes, a closed-form solution is possible:

(1) Change coordinates to place the given line on the X-coordinate.
(2) Find solutions to the cubic y(t) = 0 in closed-form.
(3) Discard solutions which are not real or are outside the range
    of t (typically [0,1]) for the curve definition.
(4) Evaluate curve point(s) at remaining t(s); discard any outside
    the line segment range.
(4) Remaining t's are intersections of the curve with the line.

Richard J. Kinch
More Bezier expertise at http://truetex.com/bezexp.htm

===============================================================================

From: Richard Kinch (truetex@IDT.NET)
Subject: Re: [Q] Bezier curve intersection, reduction of control points, ...
Newsgroups: comp.graphics.algorithms
Date: 1998-10-16

Martin Schaefer <martin_s@fwhk.hk.net> wrote:

> I am working in the field of Computer Aided Font Design, and
> looking for some algorithms to solve the problems below.

I have solutions to all of them, see the Metafog paper on my
Web site below for some dated descriptions of results.

I should note that there is no one document to my knowledge that
explains more than a little of what you want.  I have never seen
solutions to some problems published.

I'll briefly describe the solutions here.  I plan a series of
papers on these topics soon, and would invite private discussion
with anyone with similar interests.

> 1) test input data correctness, i.e. no curve has self-overlaps.

Check every curve against every other curve for intersections.
So you need an intersection finder.  That's explained elsewhere.

> 2) a fast and reliable algorithm to determine the orientation of
> a closed, non-self-overlapping curve.

There are several approaches that are very difficult to code
for degenerate cases.  Instead, the easy method is to polygonize the
curves to some tolerance, and then compute the winding number.

> 3) curve fitting: reduction of the number of control points of
> a curve, given a certain error margin.

I call this "combing".  Remove a knot (inverse closed-form solution
from bisection), check the divergence of the result, if below
some tolerance the knot was redundant and stays out, otherwise
undo the removal.  Repeat for each knot.

> Also, at each extremal
> point of the curve with respect to the x- and y-axes (e.g. at
> the top, bottom, left, and right sides of an 'O') there must
> be a control point connecting two Bezier segments.

The derivative (slope) of a cubic Bezier is a quadratic.  Solve
the quadratic in X (and Y) for roots to get the vertical (and
horizontal) tangent times.  There may be zero, one or two such
roots (tangencies) in each direction.  Bisect the curve at each.

> 4) remove overlap: given a set of closed curves that don't
> self-overlap but maybe overlap each other, i need to construct
> a different set of closed curves that don't overlap each other.

This is an enormous problem, to which I have just finished, but
not published, a general solution in the curve (versus polygon or
bitmap) form.  A partial solution I developed earlier is described
in the Metafog paper.  I note that a "parallel to a Bezier"
solution is also described there, which the c.g.a FAQ says is not
possible.

If you are considering implementing this yourself, may I warn you that
it took me some years and 10,000+ lines of C.  It is a veritable tour
de force of several advanced mathematical topics.

Richard J. Kinch, PhD
Publisher, TrueTeX (R) brand typesetting software.
See http://truetex.com

===============================================================================

From: Richard J Kinch (nobody@nowhere.com)
Subject: Re: Self intersecting bezier
Newsgroups: comp.graphics.algorithms
Date: 2002-07-06 11:38:56 PST

Lenny Kudling writes:

> maybe this is a very trivial problem... but can onyone give me a hint
> how to easiest/fastest determine if a given cubic bezier is
> self-intersecting? 

I don't believe there is any closed form solution, only numerical.  The 
easiest method is to take a good intersection finder, and plug in the same 
curve for both inputs.  Beware that many intersection finders are not 
written generally enough to handle curves that share some (or all, in the 
case of self-intersection) equal portion(s).

Since you can find intersections of a cubic Bezier curve with a line 
segment in closed form, you can use that solution to iteratively compare 
the curve to its own flattened segments.

Richard Kinch
http://www.truetex.com/bezexp.htm

===============================================================================

From: Jacques De Schepper (jacques.deschepperNOSP@Martwork-systems.com)
Subject: Re: outsetting/insetting a bezier path
Newsgroups: comp.graphics.algorithms
Date: 2002-01-09 03:54:26 PST


offset (Bezier(p1,p2,p3,p4),distance) {
    
    normal1 = cross (p2 - p1); // crossing means x=y y=-x
    normal2 = cross (p3 - p2);
    normal3 = cross (p4 - p3);

    line1 = line(p1,p2) translated over (normal1*distance);
    line2 = line(p2,p3) translated over (normal2*distance);
    line3 = line(p3,p4) translated over (normal3*distance);

    new_p1 = p1 translated over (normal1*distance);
    new_p2 = intersection(line1,line2);
    new_p3 = intersection(line2,line3);
    new_p4 = p4 translated over (normal3*distance);
}
|#

;;;

;;;
;;;  Build a <bezier-curve>
;;;  given the following boundary conditions:
;;;
;;;      (1) the starting point (E0X, E0Y)
;;;      (2) the ending point (E1X, E1Y)
;;;      (3) the mid point (EMX, EMY)
;;;      (4) the tangent at the starting point (X0,Y0)..(X1,Y1)
;;;      (5) the tangent at the ending point (X2,Y2)..(X3,Y3)
;;;
;;;  Note that the tangents are specified in the same form
;;;  as would be extracted from another bezier curve which
;;;  the resulting curve is going to be parallel to at the
;;;  start and end points.

(define (fit-curve (e0 <point>)
                   (em <point>)
                   (e1 <point>)
                   (c <bezier-curve>))
  (bind ((x0 y0 x1 y1 x2 y2 x3 y3 (compute-bezier-given-boundary-conditions-1
                                   (x e0) (y e0)
                                   (x e1) (y e1)
                                   (x em) (y em)
                                   (x (start-point c)) (y (start-point c))
                                   (x (first-handle c)) (y (first-handle c))
                                   (x (second-handle c)) (y (second-handle c))
                                   (x (end-point c)) (y (end-point c)))))
    (curv x0 y0 x1 y1 x2 y2 x3 y3)))

  
(define (compute-bezier-given-boundary-conditions-1 E0X E0Y
                                                    E1X E1Y
                                                    EMX EMY
                                                    X0 Y0
                                                    X1 Y1
                                                    X2 Y2
                                                    X3 Y3)
  (let* ((x0 E0X)
         (y0 E0Y)
         (x1
          (+
           (*
            1/9
            E0Y
            (+ (* -12 X0 X2) (* 12 X1 X2) (* 12 X0 X3) (* -12 X1 X3))
            (/
             (+
              (* X2 Y0)
              (- (* X3 Y0))
              (- (* X2 Y1))
              (* X3 Y1)
              (- (* X0 Y2))
              (* X1 Y2)
              (* X0 Y3)
              (- (* X1 Y3)))))
           (*
            1/9
            E1Y
            (+ (* -12 X0 X2) (* 12 X1 X2) (* 12 X0 X3) (* -12 X1 X3))
            (/
             (+
              (* X2 Y0)
              (- (* X3 Y0))
              (- (* X2 Y1))
              (* X3 Y1)
              (- (* X0 Y2))
              (* X1 Y2)
              (* X0 Y3)
              (- (* X1 Y3)))))
           (*
            8/9
            EMY
            (+ (* 3 X0 X2) (* -3 X1 X2) (* -3 X0 X3) (* 3 X1 X3))
            (/
             (+
              (* X2 Y0)
              (- (* X3 Y0))
              (- (* X2 Y1))
              (* X3 Y1)
              (- (* X0 Y2))
              (* X1 Y2)
              (* X0 Y3)
              (- (* X1 Y3)))))
           (*
            8/9
            EMX
            (+ (* -3 X0 Y2) (* 3 X1 Y2) (* 3 X0 Y3) (* -3 X1 Y3))
            (/
             (+
              (* X2 Y0)
              (- (* X3 Y0))
              (- (* X2 Y1))
              (* X3 Y1)
              (- (* X0 Y2))
              (* X1 Y2)
              (* X0 Y3)
              (- (* X1 Y3)))))
           (*
            1/9
            E0X
            (/
             (+
              (* X2 Y0)
              (- (* X3 Y0))
              (- (* X2 Y1))
              (* X3 Y1)
              (- (* X0 Y2))
              (* X1 Y2)
              (* X0 Y3)
              (- (* X1 Y3))))
            (+
             (* 9 X2 Y0)
             (* -9 X3 Y0)
             (* -9 X2 Y1)
             (* 9 X3 Y1)
             (* 3 X0 Y2)
             (* -3 X1 Y2)
             (* -3 X0 Y3)
             (* 3 X1 Y3)))
           (*
            1/9
            E1X
            (/
             (+
              (* X2 Y0)
              (- (* X3 Y0))
              (- (* X2 Y1))
              (* X3 Y1)
              (- (* X0 Y2))
              (* X1 Y2)
              (* X0 Y3)
              (- (* X1 Y3))))
            (+ (* 12 X0 Y2) (* -12 X1 Y2) (* -12 X0 Y3) (* 12 X1 Y3)))))
         (y1
          (+
           (*
            1/9
            E1Y
            (+ (* -12 X2 Y0) (* 12 X3 Y0) (* 12 X2 Y1) (* -12 X3 Y1))
            (/
             (+
              (* X2 Y0)
              (- (* X3 Y0))
              (- (* X2 Y1))
              (* X3 Y1)
              (- (* X0 Y2))
              (* X1 Y2)
              (* X0 Y3)
              (- (* X1 Y3)))))
           (*
            8/9
            EMY
            (+ (* 3 X2 Y0) (* -3 X3 Y0) (* -3 X2 Y1) (* 3 X3 Y1))
            (/
             (+
              (* X2 Y0)
              (- (* X3 Y0))
              (- (* X2 Y1))
              (* X3 Y1)
              (- (* X0 Y2))
              (* X1 Y2)
              (* X0 Y3)
              (- (* X1 Y3)))))
           (*
            1/9
            E0Y
            (+
             (* -3 X2 Y0)
             (* 3 X3 Y0)
             (* 3 X2 Y1)
             (* -3 X3 Y1)
             (* -9 X0 Y2)
             (* 9 X1 Y2)
             (* 9 X0 Y3)
             (* -9 X1 Y3))
            (/
             (+
              (* X2 Y0)
              (- (* X3 Y0))
              (- (* X2 Y1))
              (* X3 Y1)
              (- (* X0 Y2))
              (* X1 Y2)
              (* X0 Y3)
              (- (* X1 Y3)))))
           (*
            8/9
            EMX
            (/
             (+
              (* X2 Y0)
              (- (* X3 Y0))
              (- (* X2 Y1))
              (* X3 Y1)
              (- (* X0 Y2))
              (* X1 Y2)
              (* X0 Y3)
              (- (* X1 Y3))))
            (+ (* -3 Y0 Y2) (* 3 Y1 Y2) (* 3 Y0 Y3) (* -3 Y1 Y3)))
           (*
            1/9
            E0X
            (/
             (+
              (* X2 Y0)
              (- (* X3 Y0))
              (- (* X2 Y1))
              (* X3 Y1)
              (- (* X0 Y2))
              (* X1 Y2)
              (* X0 Y3)
              (- (* X1 Y3))))
            (+ (* 12 Y0 Y2) (* -12 Y1 Y2) (* -12 Y0 Y3) (* 12 Y1 Y3)))
           (*
            1/9
            E1X
            (/
             (+
              (* X2 Y0)
              (- (* X3 Y0))
              (- (* X2 Y1))
              (* X3 Y1)
              (- (* X0 Y2))
              (* X1 Y2)
              (* X0 Y3)
              (- (* X1 Y3))))
            (+ (* 12 Y0 Y2) (* -12 Y1 Y2) (* -12 Y0 Y3) (* 12 Y1 Y3)))))
         (x2
          (+
           (*
            8/9
            EMY
            (+ (* -3 X0 X2) (* 3 X1 X2) (* 3 X0 X3) (* -3 X1 X3))
            (/
             (+
              (* X2 Y0)
              (- (* X3 Y0))
              (- (* X2 Y1))
              (* X3 Y1)
              (- (* X0 Y2))
              (* X1 Y2)
              (* X0 Y3)
              (- (* X1 Y3)))))
           (*
            1/9
            E0Y
            (+ (* 12 X0 X2) (* -12 X1 X2) (* -12 X0 X3) (* 12 X1 X3))
            (/
             (+
              (* X2 Y0)
              (- (* X3 Y0))
              (- (* X2 Y1))
              (* X3 Y1)
              (- (* X0 Y2))
              (* X1 Y2)
              (* X0 Y3)
              (- (* X1 Y3)))))
           (*
            1/9
            E1Y
            (+ (* 12 X0 X2) (* -12 X1 X2) (* -12 X0 X3) (* 12 X1 X3))
            (/
             (+
              (* X2 Y0)
              (- (* X3 Y0))
              (- (* X2 Y1))
              (* X3 Y1)
              (- (* X0 Y2))
              (* X1 Y2)
              (* X0 Y3)
              (- (* X1 Y3)))))
           (*
            1/9
            E0X
            (+ (* -12 X2 Y0) (* 12 X3 Y0) (* 12 X2 Y1) (* -12 X3 Y1))
            (/
             (+
              (* X2 Y0)
              (- (* X3 Y0))
              (- (* X2 Y1))
              (* X3 Y1)
              (- (* X0 Y2))
              (* X1 Y2)
              (* X0 Y3)
              (- (* X1 Y3)))))
           (*
            8/9
            EMX
            (+ (* 3 X2 Y0) (* -3 X3 Y0) (* -3 X2 Y1) (* 3 X3 Y1))
            (/
             (+
              (* X2 Y0)
              (- (* X3 Y0))
              (- (* X2 Y1))
              (* X3 Y1)
              (- (* X0 Y2))
              (* X1 Y2)
              (* X0 Y3)
              (- (* X1 Y3)))))
           (*
            1/9
            E1X
            (+
             (* -3 X2 Y0)
             (* 3 X3 Y0)
             (* 3 X2 Y1)
             (* -3 X3 Y1)
             (* -9 X0 Y2)
             (* 9 X1 Y2)
             (* 9 X0 Y3)
             (* -9 X1 Y3))
            (/
             (+
              (* X2 Y0)
              (- (* X3 Y0))
              (- (* X2 Y1))
              (* X3 Y1)
              (- (* X0 Y2))
              (* X1 Y2)
              (* X0 Y3)
              (- (* X1 Y3)))))))
         (y2
          (+
           (*
            8/9
            EMY
            (+ (* -3 X0 Y2) (* 3 X1 Y2) (* 3 X0 Y3) (* -3 X1 Y3))
            (/
             (+
              (* X2 Y0)
              (- (* X3 Y0))
              (- (* X2 Y1))
              (* X3 Y1)
              (- (* X0 Y2))
              (* X1 Y2)
              (* X0 Y3)
              (- (* X1 Y3)))))
           (*
            1/9
            E1Y
            (/
             (+
              (* X2 Y0)
              (- (* X3 Y0))
              (- (* X2 Y1))
              (* X3 Y1)
              (- (* X0 Y2))
              (* X1 Y2)
              (* X0 Y3)
              (- (* X1 Y3))))
            (+
             (* 9 X2 Y0)
             (* -9 X3 Y0)
             (* -9 X2 Y1)
             (* 9 X3 Y1)
             (* 3 X0 Y2)
             (* -3 X1 Y2)
             (* -3 X0 Y3)
             (* 3 X1 Y3)))
           (*
            1/9
            E0Y
            (/
             (+
              (* X2 Y0)
              (- (* X3 Y0))
              (- (* X2 Y1))
              (* X3 Y1)
              (- (* X0 Y2))
              (* X1 Y2)
              (* X0 Y3)
              (- (* X1 Y3))))
            (+ (* 12 X0 Y2) (* -12 X1 Y2) (* -12 X0 Y3) (* 12 X1 Y3)))
           (*
            1/9
            E0X
            (/
             (+
              (* X2 Y0)
              (- (* X3 Y0))
              (- (* X2 Y1))
              (* X3 Y1)
              (- (* X0 Y2))
              (* X1 Y2)
              (* X0 Y3)
              (- (* X1 Y3))))
            (+ (* -12 Y0 Y2) (* 12 Y1 Y2) (* 12 Y0 Y3) (* -12 Y1 Y3)))
           (*
            1/9
            E1X
            (/
             (+
              (* X2 Y0)
              (- (* X3 Y0))
              (- (* X2 Y1))
              (* X3 Y1)
              (- (* X0 Y2))
              (* X1 Y2)
              (* X0 Y3)
              (- (* X1 Y3))))
            (+ (* -12 Y0 Y2) (* 12 Y1 Y2) (* 12 Y0 Y3) (* -12 Y1 Y3)))
           (*
            8/9
            EMX
            (/
             (+
              (* X2 Y0)
              (- (* X3 Y0))
              (- (* X2 Y1))
              (* X3 Y1)
              (- (* X0 Y2))
              (* X1 Y2)
              (* X0 Y3)
              (- (* X1 Y3))))
            (+ (* 3 Y0 Y2) (* -3 Y1 Y2) (* -3 Y0 Y3) (* 3 Y1 Y3)))))
         (x3 E1X)
         (y3 E1Y))
    (values x0 y0 x1 y1 x2 y2 x3 y3)))

;;; fit a bezier curve through 4 points

(define (bezier-fit (a <point>) (b <point>) (c <point>) (d <point>))
  (let* ((X0 (x a))
         (X1 (x b))
         (X2 (x c))
         (X3 (x d))
         
         (Y0 (y a))
         (Y1 (y b))
         (Y2 (y c))
         (Y3 (y d))

         (x0 X0)
         (x1 (+ (* (/ -5 6) X0) (* 3 X1) (* (/ -3 2) X2) (* (/ 1 3) X3)))
         (x2 (+ (* (/ 1 3) X0) (* (/ -3 2) X1) (* 3 X2) (* (/ -5 6) X3)))
         (x3 X3)

         (y0 Y0)
         (y1 (+ (* (/ -5 6) Y0) (* 3 Y1) (* (/ -3 2) Y2) (* (/ 1 3) Y3)))
         (y2 (+ (* (/ 1 3) Y0) (* (/ -3 2) Y1) (* 3 Y2) (* (/ -5 6) Y3)))
         (y3 Y3))
    (curv x0 y0 x1 y1 x2 y2 x3 y3)))


(define (make-bezier #key 
                     start-point
                     end-point
                     (mid-points default: #f)
                     (mid-point default: #f)
                     (start-tangent default: #f)
                     (end-tangent default: #f)
                     (first-handle default: #f)
                     (second-handle default: #f))
  (cond
   ;;
   ((and first-handle second-handle (not mid-points) (not mid-point)
         (not start-tangent) (not end-tangent))
    (make <bezier-curve>
          start-point: start-point
          first-handle: first-handle
          second-handle: second-handle
          end-point: end-point))
   ;;
   ((and mid-points (not mid-point) (not start-tangent) (not end-tangent)
         (not first-handle) (not second-handle))
    (bezier-fit start-point
                (car mid-points)
                (cadr mid-points)
                end-point))
   ;;
   ((and start-tangent end-tangent 
         (not mid-point)
         (not mid-points)
         (not first-handle) (not second-handle))
    (make <bezier-curve>
          start-point: start-point
          first-handle: (point+ start-point start-tangent)
          second-handle: (point+ end-point (size* end-tangent -1))
          end-point: end-point))
   ;;
   ((and mid-point start-tangent end-tangent (not mid-points)
         (not first-handle) (not second-handle))
    (fit-curve start-point
               mid-point
               end-point
               (make <bezier-curve>
                     start-point: start-point
                     first-handle: (point+ start-point start-tangent)
                     second-handle: (point+ end-point end-tangent)
                     end-point: end-point)))
   ;;
   (else
    (error "make-bezier: don't know how to make one using keyword given"))))
  
                     
;;;

(define (bezier-quadratic-roots (c* <bezier-curve>))
  (let* (((p0 <point>) (start-point c*))
         ((p1 <point>) (first-handle c*))
         ((p2 <point>) (second-handle c*))
         ((p3 <point>) (end-point c*)))
         ;; solve the resulting cubic equation for y(t)
    (let ((a (* 3 (+ (- (y p0))
                     (* 3 (y p1))
                     (* -3 (y p2))
                     (y p3))))
          (b (* 2 (+ (* 3 (y p0))
                     (* -6 (y p1))
                     (* 3 (y p2)))))
          (c (+ (* -3 (y p0))
                (* 3 (y p1)))))
      (compute-quadratic-roots a b c))))

(define (bezier-cubic-roots (c* <bezier-curve>))
  (let* (((p0 <point>) (start-point c*))
         ((p1 <point>) (first-handle c*))
         ((p2 <point>) (second-handle c*))
         ((p3 <point>) (end-point c*)))
    ;; solve the resulting cubic equation for y(t)
    (compute-cubic-roots
     (+ (- (y p0)) (* 3 (y p1)) (* -3 (y p2)) (y p3))
     (+ (* 3 (y p0)) (* -6 (y p1)) (* 3 (y p2)))
     (+ (* -3 (y p0)) (* 3 (y p1)))
     (y p0))))

;;;
;;;   Find (all) intersections of the given line `l' with the curve `self'
;;;   The intersections are returned as a list.
;;;   Each element of the list is a list of two numbers,
;;;   which are the intersection parameters on `self' and `l' respectively.
;;;
;;;   Hence, if (Ta Tb) is in the list returned
;;;   by (intersection-parameter A B),  (A is a <bezier-curve>, B is a <line>)
;;;   then (point-on A Ta) == (point-on B Tb)
;;;

(define-method bez-intersections ((l <line>) (self <bezier-curve>))
  ;; transform ourselves onto the given line, so that
  ;; (0,0) in transformed space is the start point of the line
  ;; and (1,0) is the end point.  With the curve so transformed,
  ;; the x-intercepts (i.e., roots of the y(t) cubic equation) are
  ;; the crossings of the given line
  (let* ((t (rotate-and-scale (translate $identity-transform (from l))
                              (point- (to l) (from l))))
         ;; there's probably a better way to do this, like having
         ;; a `rotate-and-scale-inverse' procedure
         (c* (transform self (invert-transform t)))
         (roots (bezier-cubic-roots c*)))
    ;;
    (define (full-intersection t0)
      (bind ((p (point-on self t0))
             (pp dir t1 (clip-to-segment (from l) (to l) 
                                         p 
                                         (point- (to l) (from l)))))
        (list t0 t1)))
    ;;
    ;; collect just the real roots.  Note that there is some
    ;; instability in the cubic root solutions, so complex->real
    ;; has some (~0.01%) tolerance for complex numbers
    (let loop ((src roots)
               (r '()))
      (if (null? src)
          r
          (let ((n (complex->real (car src))))
            (if n
                (loop (cdr src) (cons (full-intersection n) r))
                (loop (cdr src) r)))))))

;;;

(define-method bez-intersections ((b <bezier-curve>) (a <bezier-curve>))
  (let ((abox (bbox a)))
    ;;
    (define (rec (b <bezier-curve>) t0 t1)
      (if (rects-intersect? (bbox b) abox)
          (if (> (flatness b) 0.01)
              (bind ((l r (subdivide b))
                     (tm (/ (+ t0 t1) 2)))
                ;(format #t "flatness ~d => recursing\n" (flatness b))
                (append (rec l t0 tm) (rec r tm t1)))
              (map (lambda (i)
                     (list (car i) (+ t0 (* (cadr i) (- t1 t0)))))
                   (bez-intersections (make-line (x (start-point b))
                                                 (y (start-point b))
                                                 (x (end-point b))
                                                 (y (end-point b)))
                                      a)))))
    ;;
    (rec b 0 1)))

;;;
;;;  The trick here is to find out where the curve
;;;  intersects each of the horizontal and vertical axes
;;;  that pass through the given point.  If it turns out
;;;  it intersects both axes at the same parameter value,
;;;  then that parameter value must correspond to the given
;;;  point.

(define-method bez-intersections ((b <point>) (a <bezier-curve>))
  (let ((xi (bez-intersections (make-line (x b) -1 (x b) 1) a))
        (yi (bez-intersections (make-line -1 (y b) 1 (y b)) a)))
    ;;
    (define (approx=? a b)
      (< (abs (- a b)) 0.00001))
    ;;
    ;(format #t "Xi => ~s\nYi => ~s\n" xi yi)
    ;;
    (let loop ((xi xi))
      (if (null? xi)
          (begin
            ;(format #t "   -- (no intersection)\n")
            #f)
          (if (any? (lambda (i)
                      (approx=? (car i) (caar xi)))
                    yi)
              (begin
                ;(format #t "  -- intersection at ~s\n" (caar xi))
                (caar xi))
              (loop (cdr xi)))))))

(define-method intersection-parameter ((self <bezier-curve>) other)
  ;; our cheesy way of doing dual-dispatch
  (bez-intersections other self))

;;;


(define-method transform ((self <bezier-curve>) ctm)
  (make <bezier-curve>
        start-point: (transform (start-point self) ctm)
        first-handle: (transform (first-handle self) ctm)
        second-handle: (transform (second-handle self) ctm)
        end-point: (transform (end-point self) ctm)))

(define-method from ((self <bezier-curve>))
  (start-point self))

(define-method to ((self <bezier-curve>))
  (end-point self))

;;;

(define-method distance ((self <bezier-curve>) (b <point>) #optional t)
  (if t
      (bezier-converge self b t)
      (let ((d0 (distance (start-point self) b))
            (d1 (distance (first-handle self) b))
            (d2 (distance (second-handle self) b))
            (d3 (distance (end-point self) b))
            (dm (distance (point-on self 0.5) b)))
        (cond
         ((<= dm (min d0 d1 d2 d3))
          (bezier-converge self b 0.5))
         ((<= d0 (min d1 d2 d3 dm))
          (bezier-converge self b 0))
         ((<= d1 (min d0 d2 d3 dm))
          (bezier-converge self b 0.333))
         ((<= d2 (min d0 d1 d3 dm))
          (bezier-converge self b 0.666))
         (else
          (bezier-converge self b 1))))))

(define (bezier-converge (self <bezier-curve>) (b <point>) t)
  (let loop ((k 0)
             (t t))
    (if (< k 5)
        (loop (+ k 1) (max 0 (min 1 (bezier-converge-step self b t))))
        (let* ((p (point-on self t))
               (d (distance p b)))
          (values d p t)))))

;;;
;;;  The approach we use here is to use the normal vector at `t' 
;;;  to define a line through `b', which we then intersect with
;;;  the curve to find a new `t'.
;;;

(define (bezier-converge-step (self <bezier-curve>) (b <point>) t)
  (let ((v (tangent-on self t)))
    (caar
     (select-min
      (lambda (item)
        (abs (car item)))
      (intersection-parameter
       self
       (make <line>
             from: b
             to: (point+ b (make-size (- (dy v)) (dx v)))))))))

(define (select-min proc lst)
  (if (null? lst)
      '()
      (let loop ((lst (cdr lst))
                 (min (car lst))
                 (min-value (proc (car lst))))
        (if (null? lst)
            (list min)
            (let ((v (proc (car lst))))
              (if (< v min-value)
                  (loop (cdr lst) (car lst) v)
                  (loop (cdr lst) min min-value)))))))
    

;;;

(define (flattened-bbox (self <bezier-curve>) tol)
  (let* (((p0 <point>) (start-point self))
         (x0 (x p0))
         (x1 (x p0))
         (y0 (y p0))
         (y1 (y p0)))
    ;;
    (define (rec (b <bezier-curve>))
      (if (> (flatness b) tol)
          (bind ((l r (subdivide b)))
            (begin
              (rec l)
              (rec r)))
          (let (((p0 <point>) (start-point b))
                ((p1 <point>) (end-point b)))
            (set! x0 (min x0 (x p0) (x p1)))
            (set! x1 (max x1 (x p0) (x p1)))
            (set! y0 (min y0 (y p0) (y p1)))
            (set! y1 (max y1 (y p0) (y p1))))))
    ;;
    (rec self)
    (make-rect x0 y0 (- x1 x0) (- y1 y0))))
      
      

(define (flatten-bezier (self <bezier-curve>) tol)
  ;;
  (define (rec (b <bezier-curve>) t0 t1)
    (if (> (flatness b) tol)
        (bind ((l r (subdivide b))
               (tm (/ (+ t0 t1) 2)))
          (append (rec l t0 tm) (rec r tm t1)))
        (list (list (make <line>
                          from: (start-point b)
                          to: (end-point b))
                    t0
                    t1))))
  (rec self 0 1))

(define (line-length (l <line>))
  (distance (to l) (from l)))

(define-method parameter->distance-along ((self <line>) t)
  (* t (line-length self)))

(define-method distance-along->parameter ((self <line>) d)
  (/ (line-length self) d))

(define-method parameter->distance-along ((self <bezier-curve>) t)
  (let loop ((f (flatten-bezier self 0.01))
             (l 0)
             (t t))
    (if (null? f)
        l       ; ran off the end
        (let ((dl (line-length (caar f)))
              (dt (- (caddar f) (cadar f))))
          (if (> dt t)
              (+ l (* dl (/ t dt)))
              (loop (cdr f) (+ l dl) (- t dt)))))))
        
(define-method distance-along->parameter ((self <bezier-curve>) d)
  (let loop ((f (flatten-bezier self 0.01))
             (l d)
             (t 0))
    (if (null? f)
        1       ; ran off the end
        (let ((dl (line-length (caar f)))
              (dt (- (caddar f) (cadar f))))
          (if (> dl l)
              (+ t (* (/ l dl) dt))
              (loop (cdr f) (- l dl) (+ t dt)))))))
