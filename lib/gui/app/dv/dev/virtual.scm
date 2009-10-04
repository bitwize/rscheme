;;;
;;;  This is a virtual device used to render artwork and
;;;  capture outlines suitable for drawing while dragging
;;;  handles around

(define-class <virtual-gstate> (<object>)
  (current-path init-function: make-dequeue*)
  (closed-subpaths init-value: '())
  (ctm type: <transform> init-value: $identity-transform)
  (local-ctm type: <transform> init-value: $identity-transform)
  (line-width type: <real> init-value: 1)
  (font init-value: #f))

(define (make-virtual-gstate)
  (make <virtual-gstate>))

(define-class <virtual-device> (<graphics-device>)
  (gstate type: <virtual-gstate> init-function: make-virtual-gstate)
  (flatness init-value: #f))

(define-method device-color ((dev <virtual-device>) c)
  c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Bounding-Box Computation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-method transform ((self <virtual-device>) (p0 <point>))
  (let ((t (transform p0 (ctm (gstate self)))))
    t))
  

(define-method setcolor ((self <virtual-device>) colorspec)
  (values))

(define-method setdash ((self <virtual-device>) (segments <vector>)
                                                (offset <real>))
  (values)) ;; XXX ignore on bbox device

(define-method setlinewidth ((self <virtual-device>) width)
  (set-line-width! (gstate self) width)
  (values))

(define-method setfont ((self <virtual-device>) (font <text-font>))
  (set-font! (gstate self) font)
  (values))

(define-method current-point ((self <virtual-device>))
  (let* ((p (current-path (gstate self)))
         (k (dequeue-count p)))
    (if (= k 0)
        (error "current-point: no current point"))
    (let ((pt (dequeue-ref p (- k 1))))
      (values (x pt) (y pt)))))

(define-method newpath ((self <virtual-device>))
  (set-current-path! (gstate self) (make-dequeue))
  (set-closed-subpaths! (gstate self) '()))

(define-method moveto ((self <virtual-device>) (pt <point>))
  (if (> (dequeue-count (current-path (gstate self))) 0)
      (closepath self))
  (dequeue-push-back! (current-path (gstate self)) (transform self pt))
  (values))

(define-method lineto ((self <virtual-device>) (pt <point>))
  (if (= (dequeue-count (current-path (gstate self))) 0)
      (error "lineto: no current point"))
  (dequeue-push-back! (current-path (gstate self)) (transform self pt))
  (values))

;;; `arc' works with both an open current subpath or on a new subpath

(define-method arc ((self <virtual-device>) (center <point>)
                                         (radius <real>) 
                                         (start-angle <real>)
                                         (end-angle <real>))
  (arctrace self center radius start-angle end-angle 
            (case (flatness self)
              ((#t) 0.15)
              ((#f) 0.78)
              ((medium) 0.45))
            <))

(define-method arcn ((self <virtual-device>) (center <point>)
                                          (radius <real>) 
                                          (start-angle <real>)
                                          (end-angle <real>))
  (arctrace self center radius start-angle end-angle 
            (case (flatness self)
              ((#t) -0.15)
              ((#f) -0.78)
              ((medium) -0.45))
            >))

(define (arctrace (self <virtual-device>) (center <point>)
                  (radius <real>) 
                  (start-angle <real>)
                  (end-angle <real>)
                  epsilon
                  keep-going?)
  ;;
  (let ((cp (current-path (gstate self))))
    ;;
    (define (to-point-on-arc t)
      (let ((p (transform self (make-point 
                                (+ (x center) (* radius (cos t)))
                                (+ (y center) (* radius (sin t)))))))
        (dequeue-push-back! cp p)
        (values)))
    ;;
    (let ((end-angle (* $Deg-to-rad end-angle)))
      (let loop ((theta (* $Deg-to-rad start-angle)))
        (to-point-on-arc theta)
        (let ((next (+ theta epsilon)))
          (if (keep-going? next end-angle)
              (loop next)
              (to-point-on-arc end-angle)))))))

(define-method curveto ((self <virtual-device>) (h1 <point>)
                                                (h2 <point>) 
                                                (pt <point>))
  (let (((h1 <point>) (transform self h1))
	((h2 <point>) (transform self h2))
	((pt <point>) (transform self pt)))
    (bind ((cx cy (current-point self))
           (cp (current-path self)))
      (let ((c (curv cx cy (x h1) (y h1) (x h2) (y h2) (x pt) (y pt))))
	(for-each
	 (lambda (t)
           (dequeue-push-back! cp (point-on c t))
           (values))
         (case (flatness self)
           ((#t) '(0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9))
           ((#f) '(0.3333 0.6667))
           ((medium) '(0.2 0.4 0.6 0.8 1))))
	(dequeue-push-back! cp pt)
	(values)))))

(define-method rectstroke ((self <virtual-device>) (r <rect>))
  (moveto self (lower-left r))
  (lineto self (lower-right r))
  (lineto self (upper-right r))
  (lineto self (upper-left r))
  (lineto self (lower-left r))
  (stroke self))

(define-method stroke-subpath ((self <virtual-device>) subpath)
  ;; subclass would override this
  (values))

(define-method fill-subpath ((self <virtual-device>) subpath)
  ;; subclass COULD override this; by default, filling does the
  ;; same thing as stroking
  (stroke-subpath self subpath))

(define-method stroke ((self <virtual-device>))
  (define (visit sp)
    (if (> (dequeue-count sp) 0)
        (stroke-subpath self (dequeue-state sp))))
  ;;
  (for-each visit (closed-subpaths (gstate self)))
  (visit (current-path (gstate self)))
  (newpath self)
  (values))

(define-method fill ((self <virtual-device>))
  (define (visit sp)
    (if (> (dequeue-count sp) 0)
        (fill-subpath self (dequeue-state sp))))
  ;;
  (for-each visit (closed-subpaths (gstate self)))
  (visit (current-path (gstate self)))
  (newpath self))

(define-method show ((self <virtual-device>) (str <string>))
  ;; XXX should update current-point, at least...
  (values))

(define-method closepath ((self <virtual-device>))
  (let ((cp (current-path (gstate self))))
    (if (> (dequeue-count cp) 1)
        (begin
          (dequeue-push-back! cp (dequeue-ref cp 0))
          (set-closed-subpaths! (gstate self) 
                                (cons cp (closed-subpaths (gstate self))))
          (set-current-path! (gstate self) (make-dequeue))))
    (values)))

;;;

(define-method with-gstate-saved ((self <virtual-device>) thunk)
  (let ((saved (gstate self))
        (new (clone (gstate self))))
    (if (> (dequeue-count (current-path saved)) 0)
        (set-current-path! new (dequeue-copy (current-path saved))))
    (set-gstate! self new)
    (thunk)
    (set-gstate! self saved)
    (values)))

(define-method with-ctm-saved ((self <virtual-device>) thunk)
  (let ((saved-ctm (ctm (gstate self)))
        (saved-local-ctm (local-ctm (gstate self))))
    (thunk)
    (set-ctm! (gstate self) saved-ctm)
    (set-local-ctm! (gstate self) saved-local-ctm)
    (values)))

(define-method show-handle ((self <virtual-device>) at)
  (values))
                                   
;;;
;;;  these alter `local' coordinates, too
;;;

(define-method translate ((self <virtual-device>) (delta <point>))
  (translate (gstate self) delta))

(define-method translate ((self <virtual-gstate>) (delta <point>))
  (set-ctm! self (translate (ctm self) delta))
  (set-local-ctm! self (translate (local-ctm self) delta)))

(define-method concat ((self <virtual-device>) tm)
  (concat (gstate self) tm))

(define-method concat ((self <virtual-gstate>) tm)
  (set-ctm! self (concatenate-transform (ctm self) tm))
  (set-local-ctm! self (concatenate-transform (local-ctm self) tm)))

;;;
;;;  these do not update `local' coordinates
;;;

(define-method translate* ((self <virtual-device>) (delta <point>))
  (set-ctm! (gstate self) (translate (ctm (gstate self)) delta)))

(define-method concat* ((self <virtual-device>) tm)
  (set-ctm! (gstate self) (concatenate-transform (ctm (gstate self)) tm)))

