;;;
;;;  Procedures to support debugging
;;;

(define (the-open-view)
  (next-owner (current-client)))

(define (the-selection)
  (key-sequence (current-selection (the-open-view))))

;;;

(define (identify-point (v <open-view>) (d <point>) #optional mode)
  (if mode
      (let ((v (underlying-object v)))
        (set! d (point+ (transform d (view-ctm v))
                        (size* (point->size (view-origin v)) -1)))))
  (with-x11-device
   (content-window v)
   (lambda (dev)
     (setcolor dev (device-color dev '(rgb 0 0.5 0.5)))
     (setlinewidth dev 3)
     (translate dev d)
     ;;
     (moveto dev (make-point -5 -5))
     (lineto dev (make-point 5 5))
     (stroke dev)
     ;;
     (moveto dev (make-point 5 -5))
     (lineto dev (make-point -5 5))
     (stroke dev)))
  (flush-client))

(define (identify-coords (v <open-view>) tm)
  (let ((o (transform $zero-point tm))
        (xa (transform (make-point 100 0) tm))
        (ya (transform (make-point 0 100) tm)))
    (with-x11-device
     (content-window v)
     (lambda (dev)
       (setcolor dev (device-color dev '(rgb 1 0 0)))
       (setlinewidth dev 1)
       ;;
       (moveto dev ya)
       (lineto dev o)
       (lineto dev xa)
       (stroke dev)))
    (flush-client)))
