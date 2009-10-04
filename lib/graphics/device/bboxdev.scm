;;;
;;;  A virtual device to compute a tight bounding box
;;;  (although it doesn't take into account clipped content)
;;;
;;;  Also keeps track of fonts

(define-class <bbox-capture-gstate> (<object>)
  (ctm type: <transform>)
  (current-point init-value: #f)
  (current-font init-value: #f)
  (current-linewidth init-value: 1)
  (subpath-starts init-value: '())
  (current-path init-value: #f))

(define-class <bbox-capture-device> (<graphics-device>)
  (gstate type: <bbox-capture-gstate>)
  (bbox init-value: #f)
  (used-fonts init-function: make-string-table))

(define-method close-graphics-device ((self <bbox-capture-device>))
  (values (bbox self)
          (key-sequence (used-fonts self))))

(define (open-bbox-device)
  (make <bbox-capture-device>
        gstate: (make <bbox-capture-gstate>
                      ctm: (make-affine-transform))))

(define-method currentpoint ((self <bbox-capture-device>))
  (current-point (gstate self)))

(define-method currentfont ((self <bbox-capture-device>))
  (current-font (gstate self)))

(define-method setfont ((self <bbox-capture-device>) (font <text-font>))
  (table-insert! (used-fonts self) (postscript-name font) font)
  (set-current-font! (gstate self) font))

(define-method setlinewidth ((self <bbox-capture-device>) width)
  (set-current-linewidth! (gstate self) width))

(define-method setlinecap ((self <bbox-capture-device>) mode)
  (values))

(define-method setlinejoin ((self <bbox-capture-device>) mode)
  (values))

(define-method setdash ((self <bbox-capture-device>) 
                        (segments <vector>)
                        (offset <real>))
  (values))

(define (bbox-add-device-rect (self <bbox-capture-device>) r)
  (if r
      (let (((r <rect>) r))
        (let ((b (bbox self)))
          (set-bbox! self (if b (union-rect r b) r))
          (values)))))

(define (bbox-add-user-rect (self <bbox-capture-device>) (r <rect>))
  (bbox-add-device-rect self (transform r (ctm (gstate self)))))

(define-method current-point ((self <bbox-capture-device>))
  (inverse-transform (current-point (gstate self))
                     (ctm (gstate self))))

(define-method show ((self <bbox-capture-device>) text)
  (let* ((g (gstate self))
         (fnt (current-font g))
         (cp (current-point self)))
    ;;
    (bbox-add-user-rect
     self 
     (offset-rect (string-bbox (font-metrics fnt) text) (x cp) (y cp)))
    ;;
    (set-current-point! 
     g
     (point+ (current-point g)
             (transform (make-size (string-width (font-metrics fnt) text) 0)
                        (ctm (gstate self)))))
    (values)))

(define-method scale ((self <bbox-capture-device>) (sx <real>) (sy <real>))
  (set-ctm! (gstate self) (scale (ctm (gstate self)) (make-point sx sy))))

(define-method rotate ((self <bbox-capture-device>) (angle <real>))
  (set-ctm! (gstate self) (rotate (ctm (gstate self)) angle)))

(define-method translate ((self <bbox-capture-device>) (delta <point>))
  (set-ctm! (gstate self) (translate (ctm (gstate self)) delta)))

(define-method concat ((self <bbox-capture-device>) tm)
  (set-ctm! (gstate self) (concatenate-transform (ctm (gstate self)) tm)))

;;;

(define-method newpath ((self <bbox-capture-device>))
  (set-current-path! (gstate self) (make-dequeue))
  (set-subpath-starts! (gstate self) '()))

(define-method clearpath ((self <bbox-capture-device>))
  (set-current-path! (gstate self) #f)
  (set-subpath-starts! (gstate self) '()))

(define-method closepath ((self <bbox-capture-device>))
  (let ((cp (current-path (gstate self))))
    (if cp
        (dequeue-push-back! 
         cp
         (dequeue-ref cp (car (subpath-starts (gstate self)))))
        (error "closepath: no current path"))
    (values)))

(define-method moveto ((self <bbox-capture-device>) pt)
  (let ((g (gstate self)))
    (if (not (current-path g))
        (newpath self))
    (set-subpath-starts! g (cons (dequeue-count (current-path g))
                                 (subpath-starts g)))
    (let ((p (transform pt (ctm g))))
      (dequeue-push-back! (current-path g) p)
      (set-current-point! g p)
      (values))))

(define-method curveto ((self <bbox-capture-device>) 
                        (h1 <point>)
                        (h2 <point>) 
                        (pt <point>))
  (lineto self h1)      ; we don't care about the details;
                        ; just capture the coarse bbox
  (lineto self h2)
  (lineto self pt))

(define-method lineto ((self <bbox-capture-device>) pt)
  (let ((p (transform pt (ctm (gstate self)))))
    (dequeue-push-back! (current-path (gstate self)) p)
    (set-current-point! (gstate self) p)
    (values)))

(define (inset-rect* r dx dy)
  (and r (inset-rect r dx dy)))
      
(define-method stroke ((self <bbox-capture-device>))
  ;; XXX doesn't handle miter extensions
  (let ((lw-y (inverse-transform (make-size 0 1) (ctm (gstate self))))
        (lw-x (inverse-transform (make-size 1 0) (ctm (gstate self)))))
    ;;
    (bbox-add-device-rect
     self 
     (inset-rect* (get-path-bbox self) 
                  (- (/ (current-linewidth (gstate self))
                        2
                        (sqrt (inner-product lw-x lw-x))))
                  (- (/ (current-linewidth (gstate self))
                        2
                        (sqrt (inner-product lw-y lw-y))))))
    (clearpath self)))
  
(define-method fill ((self <bbox-capture-device>))
  (bbox-add-device-rect self (get-path-bbox self))
  (clearpath self))

(define (get-path-bbox (self <bbox-capture-device>))
  (let ((q (current-path (gstate self))))
    (if q
        (let loop ((b (make-rect (x (dequeue-ref q 0))
                                 (y (dequeue-ref q 0))
                                 0 0))
                   (i 1))
          (if (< i (dequeue-count q))
              (loop (union-rect b (make-rect (x (dequeue-ref q i))
                                             (y (dequeue-ref q i))
                                             0 0))
                    (+ i 1))
              b))
        #f)))
      
(define-method with-gstate-saved ((self <bbox-capture-device>) thunk)
  (let ((saved (gstate self)))
    (set-gstate! self (with-module
                          usual-inlines
                        (clone (gstate self))))
    (if (current-path saved)
        (set-current-path! (gstate self)
                           (dequeue-copy (current-path saved))))
    (thunk)
    (set-gstate! self saved)
    (values)))

(define (dequeue-copy q)
  (with-module
      usual-inlines
    (%make (object-class q)
           (clone (gvec-ref q 0))
           (gvec-ref q 1)
           (gvec-ref q 2))))

;;;

(define-method setmiterlimit ((self <bbox-capture-device>) limit)
  ;; not taking into account miter limit
  (values))

(define-method setcolor ((self <bbox-capture-device>) 
                         (colspec <boolean>)
                         #optional mode)
  ;; we aren't keeping track of colors...
  (values))

(define-method device-color ((dev <bbox-capture-device>) c)
  #t)

(define (bbox-arc (self <bbox-capture-device>) (center <point>)
                  (radius <real>) 
                  (start-angle <real>)
                  (end-angle <real>)
                  (delta-angle <real>))
  ;;
  (define done? (if (> delta-angle 0) >= <=))
  ;;
  (define (p t)
    (let ((r (* t (/ $Pi 180))))
      (make-point (+ (x center) (* (cos r) radius))
                  (+ (y center) (* (sin r) radius)))))
  ;;
  (if (not (current-path (gstate self)))
      (moveto self (p start-angle)))
  ;;
  ;; this is pretty nasty, but it gives a fairly tight bbox
  ;; and doesn't involve figuring out the right bezier curves
  ;; (although that would be nicer...)
  (let loop ((theta start-angle))
    (if (done? theta end-angle)
        (lineto self (p end-angle))
        (begin
          (lineto self (p theta))
          (loop (+ theta delta-angle))))))
  
(define-method arc ((self <bbox-capture-device>) (center <point>)
                    (radius <real>) 
                    (start-angle <real>)
                    (end-angle <real>))
  (bbox-arc self center radius start-angle end-angle 10))

        
(define-method arcn ((self <bbox-capture-device>) (center <point>)
                     (radius <real>) 
                     (start-angle <real>)
                     (end-angle <real>))
  (bbox-arc self center radius end-angle start-angle -10))

        
(define-method startpage ((self <bbox-capture-device>) #rest r)
  (values))

(define-method endpage ((self <bbox-capture-device>) #rest r)
  (values))

(define-method rectclip ((self <bbox-capture-device>) (r <rect>))
  ;; we should be able to handle rectclip, at least, by intersecting
  ;; the argument here (in device coords) with the remaining paint...
  (values))

(define-method composite-image ((self <bbox-capture-device>) 
                                image 
                                #key (image-matrix default: $identity-transform))
  (let* ((ibox (make-rect 0 0 (image-width image) (image-height image)))
         (tbox (transform ibox image-matrix)))
    (bbox-add-user-rect self tbox)
    (values)))
  
