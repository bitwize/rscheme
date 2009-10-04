;;;
;;;   Draw a dimensional annotation like this:
;;;
;;;    |                                   |
;;;    |<------------- title ------------->|
;;;    |                                   |
;;;    |                                   |
;;;
;;;    *=============BASELINE==============*
;;;
;;;  tic-length : The total length of the tic mark (the thin lines
;;;               perpendicular to BASELINE)
;;;
;;;  setback : The distance from the BASELINE to the (start) of the
;;;            tic mark
;;;
;;;  title-angle : The angle of the title, which may be relative
;;;                to BASELINE or may be absolute with respect to
;;;                the coordinate system in which this whole thing
;;;                is being drawn (which, of course, could be rotated
;;;                with respect to some other, such as page, coords)


(define (draw-dimen-labels (device <graphics-device>)
                           #key
                           (base type: <line>)
                           (title-angle type: <real> default: 0)
                           (title-angle-relative default: 'absolute)
                           (setback type: <real>)
                           (title type: <string>)
                           (tic-length type: <real> default: 10)
                           (title-font type: <text-font>)
                           (title-padding type: <real> default: 1))
  (let-syntax ((debug 
                (syntax-form body (values))
                ;;(syntax-form body (begin . body))
                ))
    ;;
    (assert (memq title-angle-relative '(absolute relative)))
    ;;
    (let* ((norm (normal-on base 0))
           (dev device)
           (norm (if (< setback 0) (size* norm -1) norm))
           (setback (abs setback)))
      ;;
      (debug
       (format #t "base ~s normal ~s setback ~s\n" base norm setback))
      ;;
      (define (drawtic pt)
        (let ((a (point+ pt (size* norm setback)))
              (b (point+ pt (size* norm (+ setback tic-length -4))))
              (c (point+ pt (size* norm (+ setback tic-length)))))
          ;;
          (moveto dev a)
          (lineto dev c)
          (stroke dev)
          b))
      ;;
      (setlinewidth dev 0.5)
      ;;
      (let* ((a (drawtic (from base)))
             (b (drawtic (to base)))
             (mid (point-average a b))
             (along (normalize (point- (to base) (from base))))
             (bb (string-bbox (font-metrics title-font) title))
             (w (size-width bb))
             (title-box (inset-rect bb
                                    (- title-padding)
                                    (- title-padding)))
             (dmid (+ (/ w 2) title-padding)))
        ;;
        (let* ((line-angle (atan (dy along) (dx along)))
               (title-ang (case title-angle-relative
                            ((relative) (+ line-angle (deg->rad title-angle)))
                            ((absolute) (deg->rad title-angle))))
               (tbox-relative-line-angle (rad-mod (- (+ $Pi line-angle)
                                                     title-ang)))
               (tbb0 (rect-boundary-by-angle-rad 
                      title-box 
                      tbox-relative-line-angle))
               (tbb1 (debug (rect-boundary-by-angle-rad 
                             title-box 
                             (+ tbox-relative-line-angle $Pi))))
               (px (make-point (+ (- (origin-x title-box))
                                  (/ (size-width title-box) -2))
                               (+ (- (origin-y title-box))
                                  (/ (size-height title-box) -2))))
               ;; TBTM is the transformation from title box coordinates
               ;; to primary user coordinates
               (tbtm (let* ((ctm $identity-transform)
                            (ctm (translate ctm mid))
                            (ctm (rotate ctm (rad->deg title-ang)))
                            (ctm (translate ctm px)))
                       ctm)))
          ;;
          ;;  now: (transform PT tbtm) => location in current user coordinates
          ;;  of PT, which is expressed in title box coordinates.
          ;;
          (let ((dmid (distance (transform (make-point (center-x title-box)
                                                       (center-y title-box))
                                           tbtm)
                                (transform tbb0 tbtm))))
            ;; since we know dmid is symmetric, no need to compute 
            ;; it with tbb1
            ;;
            (arrowstroke dev (list (point+ mid (size* along (- dmid))) a)
                         setback: 0.25)
            (arrowstroke dev (list (point+ mid (size* along dmid)) b)
                         setback: 0.25))
          ;;
          (debug
           (format #t "line angle: ~s\n" (rad->deg line-angle))
           (format #t "title angle: ~s\n" (rad->deg title-ang))
           (format #t "tbox-relative-angle: ~s\n" 
                   (rad->deg tbox-relative-line-angle)))
          ;;
          
          (with-gstate-saved
           dev
           (lambda ()
             (translate dev mid)
             (debug (setcolor dev (device-color dev '(rgb 1 0 0))))
                                        ;(annotate dev $zero-point 'x)
             ;;
             (rotate dev (rad->deg title-ang))
             (translate dev px)
             (debug
              (rectstroke dev title-box))
             ;;
             (debug
              (annotate dev tbb0 'circle)
              (annotate dev tbb1 'circle))
             ;;
             (debug
              (moveto dev (make-point 0 0))
              (lineto dev (make-point w 0))
              (stroke dev))
             ;;
             (debug (setcolor dev (device-color dev '(rgb 0 1 0))))
             (moveto dev (make-point 0 0))
             (setfont dev title-font)
             (show dev title)
             ))
          (debug
           (with-gstate-saved
            dev
            (lambda ()
              (setcolor dev (device-color dev '(rgb 0 0 1)))
              (annotate dev (transform $zero-point tbtm) 'circle)))))))))

;;;
;;;   Find a point on the boundary of a rectangle, as given
;;;   by the angle it makes with the center of the rect.  Hence,
;;;   small (near 0) angles will be points on the right edge of
;;;   the rectangle.  Near 90 degrees (the angle is specified in
;;;   degrees), the result will be on the top edge of the rectangle
;;;

(define (rect-boundary-by-angle rect angle)
  (rect-boundary-by-angle-rad rect (deg->rad angle)))
 
(define (deg->rad a)
  (* $Pi (/ a 180)))
 
(define (rad->deg a)
  (/ (* a 180) $Pi))

(define (rad-mod r)
  (- r (* 2 $Pi (floor (/ r (* 2 $Pi))))))

(define (rect-boundary-by-angle-rad rect r)
  (let* ((r (rad-mod r))
         (a0 (atan (size-height rect) (size-width rect)))
         (a2 (+ a0 $Pi))
         (a1 (- a2 (* 2 a0)))
         (a3 (+ a1 $Pi)))
    ;;
    (define (h/2) (/ (size-height rect) 2))
    (define (w/2) (/ (size-width rect) 2))
    (define (cx) (center-x rect))
    (define (cy) (center-y rect))
    ;;
    ;; `a_n' are the angles at which we transition 
    ;; from one edge to another.  a0 is also the angle of the
    ;; diagonal from the lower left (origin) to the upper right
    ;; corner.
    (bind ((x y (cond
                 ((<= r a0)
                  (values (limit-x rect)
                          (+ (cy) (* (w/2) (tan r)))))
                 ((<= r a1)
                  (values (+ (cx) (/ (h/2) (tan r)))
                          (limit-y rect)))
                 ((<= r a2)
                  (values (origin-x rect)
                          (- (cy) (* (w/2) (tan r)))))
                 ((<= r a3)
                  (values (- (cx) (/ (h/2) (tan r)))
                          (origin-y rect)))
                 (else
                  (values (limit-x rect)
                          (+ (cy) (* (w/2) (tan r))))))))
      (make-point x y))))
