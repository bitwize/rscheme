;;;
;;;  This is a virtual device used to render scripts and
;;;  capture their pick data
;;;

(define-class <pick-gstate> (<object>)
  (current-path init-function: make-dequeue*)
  (closed-subpaths init-value: '())
  (ctm type: <transform> init-value: $identity-transform)
  (local-ctm type: <transform> init-value: $identity-transform))
  
(define-class <pick-device> (<graphics-device>)
  (gstate type: <pick-gstate>)
  (owner init-value: #f)
  (pick-point init-value: $zero-point)
  (handles init-value: '())
  (pick-list init-function: make-dequeue*))

(define (with-pick-device proc)
  (proc (make <pick-device>
              gstate: (make <pick-gstate>))))

(define-method device-color ((dev <pick-device>) c)
  c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Pick list
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-method transform ((self <pick-device>) (p0 <point>))
  (transform p0 (ctm (gstate self))))

(define-method setdash ((self <pick-device>) (segments <vector>)
                                             (offset <real>))
  (values)) ;; XXX ignore on pick device

(define-method setcolor ((self <pick-device>) (pixel-value <fixnum>))
  (values))

(define-method setlinewidth ((self <pick-device>) width)
  (values))

(define-method setfont ((self <pick-device>) (font <text-font>))
  (values))

(define-method current-point ((self <pick-device>))
  (let* ((p (current-path (gstate self)))
         (k (dequeue-count p)))
    (if (= k 0)
        (error "current-point: no current point"))
    (let ((pt (dequeue-ref p (- k 1))))
      (values (x pt) (y pt)))))

(define-method newpath ((self <pick-device>))
  (set-current-path! (gstate self) (make-dequeue))
  (set-closed-subpaths! (gstate self) '()))

(define-method moveto ((self <pick-device>) (pt <point>))
  (if (> (dequeue-count (current-path (gstate self))) 0)
      (closepath self))
  (dequeue-push-back! (current-path (gstate self)) (transform self pt))
  (values))

(define-method lineto ((self <pick-device>) (pt <point>))
  (if (= (dequeue-count (current-path (gstate self))) 0)
      (error "lineto: no current point"))
  (dequeue-push-back! (current-path (gstate self)) (transform self pt))
  (values))

;(define $Pi (with-module mathlib $Pi))
(define $Deg-to-rad (/ $Pi 180))

;;; `arc' works with both an open current subpath or on a new subpath

(define-method arc ((self <pick-device>) (center <point>)
                                         (radius <real>) 
                                         (start-angle <real>)
                                         (end-angle <real>))
  (arcpick self center radius start-angle end-angle 0.15 <))

(define-method arcn ((self <pick-device>) (center <point>)
                                          (radius <real>) 
                                          (start-angle <real>)
                                          (end-angle <real>))
  (arcpick self center radius start-angle end-angle -0.15 >))

(define (arcpick (self <pick-device>) (center <point>)
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
        (let ((next (+ theta 0.15)))
          (if (< next end-angle)
              (loop next)
              (to-point-on-arc end-angle)))))))

(define-method curveto ((self <pick-device>) (h1 <point>)
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
	 '(0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9))
	(dequeue-push-back! cp pt)
	(values)))))

(define-method rectstroke ((self <pick-device>) (r <rect>))
  (moveto self (lower-left r))
  (lineto self (lower-right r))
  (lineto self (upper-right r))
  (lineto self (upper-left r))
  (lineto self (lower-left r))
  (stroke self))

(define-method stroke ((self <pick-device>))
  ;;
  (define (stroke1 subpath)
    (if (> (dequeue-count subpath) 1)
        (for-each
         (lambda (pick)
           (dequeue-push-back! (pick-list self) pick)
           (values))
         (pick-on-path (owner self) 
                       (pick-point self) 
                       $identity-transform
                       (vector->list (dequeue-state subpath))))))
  ;;
  (for-each stroke1 (closed-subpaths (gstate self)))
  (stroke1 (current-path (gstate self)))
  (newpath self)
  (values))

(define-method fill ((self <pick-device>))
  ;;; XXX need to be able to pick out fills, too
  (newpath self))

(define-method show ((self <pick-device>) (str <string>))
  ;; XXX need to be able to pick out text, too
  (values))

(define-method closepath ((self <pick-device>))
  (let ((cp (current-path (gstate self))))
    (if (> (dequeue-count cp) 1)
        (begin
          (dequeue-push-back! cp (dequeue-ref cp 0))
          (set-closed-subpaths! (gstate self) 
                                (cons cp (closed-subpaths (gstate self))))
          (set-current-path! (gstate self) (make-dequeue))))
    (values)))

;;;

(define-method with-gstate-saved ((self <pick-device>) thunk)
  (let ((saved (gstate self))
        (new (clone (gstate self))))
    (if (> (dequeue-count (current-path saved)) 0)
        (set-current-path! new (dequeue-copy (current-path saved))))
    (thunk)
    (set-gstate! self saved)
    (values)))

(define-method with-ctm-saved ((self <pick-device>) thunk)
  (let ((saved-ctm (ctm (gstate self)))
        (saved-local-ctm (local-ctm (gstate self))))
    (thunk)
    (set-ctm! (gstate self) saved-ctm)
    (set-local-ctm! (gstate self) saved-local-ctm)
    (values)))

(define-method show-handle ((self <pick-device>) at)
  (set-handles! self (cons (transform self at) (handles self))))
                                   
;;;
;;;  these alter `local' coordinates, too
;;;

(define-method translate ((self <pick-device>) (delta <point>))
  (translate (gstate self) delta))

(define-method translate ((self <pick-gstate>) (delta <point>))
  (set-ctm! self (translate (ctm self) delta))
  (set-local-ctm! self (translate (local-ctm self) delta)))

(define-method concat ((self <pick-device>) tm)
  (concat (gstate self) tm))

(define-method concat ((self <pick-gstate>) tm)
  (set-ctm! self (concatenate-transform (ctm self) tm))
  (set-local-ctm! self (concatenate-transform (local-ctm self) tm)))

;;;
;;;  these do not up `local' coordinates
;;;

(define-method translate* ((self <pick-device>) (delta <point>))
  (set-ctm! (gstate self) (translate (ctm (gstate self)) delta)))

(define-method concat* ((self <pick-device>) tm)
  (set-ctm! (gstate self) (concatenate-transform (ctm (gstate self)) tm)))

