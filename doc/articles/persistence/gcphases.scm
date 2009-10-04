
(define *time-scale* (/ 15 10))
(define *vertical-scale* (/ 15 10))

(define *state-section-y0* (* 70 *vertical-scale*))
(define *state-section-y1* (* 90 *vertical-scale*))
(define *activity-section-y0* (* 8 *vertical-scale*))
(define *activity-section-y1* (* 60 *vertical-scale*))

(define *prep-x0* (* 20 *time-scale*))
(define *pending-x0* (* 50 *time-scale*))
(define *tscan-x0* (* 90 *time-scale*))
(define *pscan-x0* (* 150 *time-scale*))
(define *reclaim-x0* (* 190 *time-scale*))
(define *idle-x0* (* 220 *time-scale*))
(define *idle-x1* (* 260 *time-scale*))

(define (diamond)
  (moveto 0 -5)
  (lineto 3 0)
  (lineto 0 5)
  (lineto -3 0)
  (closepath)
  (fill))


(define (draw)
  ;print output...
  ; (translate (make-point (* 72 6) 72))
  ; (rotate 90)
  ; (scale 1.5 1.5)
  ;;
  (let ((type-width 6))
    ;;
    (gsaved
     ;(setcolor (device-color '(rgb 0.5 0.5 1)))
     (setlinewidth 0.5)
     (setdash '#(1 1) 0)
     (for-each
      (lambda (x)
        (moveto x *activity-section-y0*)
        (lineto x *state-section-y1*))
      (list *prep-x0*
            *pending-x0*
            *tscan-x0*
            *pscan-x0*
            *reclaim-x0*
            *idle-x0*
            *idle-x1*))
     (stroke))
    ;;
    (gsaved
     (setcolor (device-color '(gray 0.667)))
     (let ((activities-marker (make-rect 0 *activity-section-y0*
					 type-width
					 (- *activity-section-y1*
					    *activity-section-y0*)))
	   (states-marker (make-rect 0 *state-section-y0*
				     type-width
				     (- *state-section-y1*
					*state-section-y0*))))
       (rectfill activities-marker)
       (rectfill states-marker)))
    ;;
    (setfont *text-font*)
    ;;
    (define (commit-mark x0)
      (gsaved
       (translate (make-point x0 (/ (+ *activity-section-y1* *state-section-y0*) 2)))
       (diamond)
       (moveto 5 -2)
       (show "commit")))
    ;;
    (commit-mark *pending-x0*)
    (commit-mark *reclaim-x0*)
    ;;
    (gsaved
     (translate (make-point -5 (/ (+ *state-section-y0* *state-section-y1*) 2)))
     (rotate 90)
     (cshow 0 0 "states"))
    ;;
    (gsaved
     (translate (make-point -5 (/ (+ *activity-section-y0* *activity-section-y1*) 2)))
     (rotate 90)
     (cshow 0 0 "activities"))
    ;;
    (for-each
     (lambda (info)
       (draw-dimen-labels base: (make-line (cadr info)
                                           *state-section-y0*
                                           (caddr info)
                                           *state-section-y0*)
                          title: (car info)
                          tic-length: (* 20 *vertical-scale*)
                          setback: 0
                          title-font: *text-font*))
     (list
      (list "prep" *prep-x0* *pending-x0*)
      (list "pending" *pending-x0* *tscan-x0*)
      (list "tscan" *tscan-x0* *pscan-x0*)
      (list "pscan" *pscan-x0* *reclaim-x0*)
      (list "reclaim" *reclaim-x0* *idle-x0*)
      (list "idle" *idle-x0* *idle-x1*)))
    ;;
    (let* ((tgch 14)
           (tgc (make-rect *tscan-x0*
                           (- *activity-section-y1* tgch)
                           (- *pscan-x0* *tscan-x0*)
                           tgch)))
      (gsaved
       (setlinewidth 1)
       (rectstroke tgc))
      (setfont *text-font*)
      (cshow (center-x tgc) (- (center-y tgc) 3) "Transient GC"))
    ;;
    (let* ((brace-y2 (- *activity-section-y1* 20))
           (brace-y0 (+ *activity-section-y0* 10))
           (brace-y1 (/ (+ brace-y0 brace-y2) 2)))
      ;;
      (define (lb x0 x1 y text)
        (draw-long-brace base: (make-line x1 y x0 y)
                         ontip: (lambda ()
                                  (rotate 180)
                                  (cshow 0 -12 text
                                         with-frame: (lambda (r)
                                                       (set! r (inset-rect r -2 -1))
                                                       (gsaved
                                                        (setcolor (device-color 'white))
                                                        (rectfill r))
                                                       ;(rectstroke r)
                                                       )))))
      ;;
      (lb *pending-x0* *tscan-x0* brace-y2 "G1 enabled")
      (lb *tscan-x0* *pscan-x0* brace-y2 "G1,G2 enabled")
      (lb *pscan-x0* *idle-x1* brace-y1 "lazy cleaning")
      (lb *prep-x0* *pending-x0* brace-y1 "eager cleaning")
      (lb *pending-x0* *pscan-x0* brace-y1 "allocate black")
      (lb *pending-x0* *reclaim-x0* brace-y0 "persistent traversal")
      ;;
      (values))))
                                      
      

(define *text-font* (get-text-font "Times" "Roman" 9))

