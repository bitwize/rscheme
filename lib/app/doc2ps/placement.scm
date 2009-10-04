(define-class <placement-group> (<object>) :abstract)

(define-class <placement-subframe> (<placement-group>)
  ;; for placements within a a text frame (i.e., a column of a <text-frame>)
  page
  frame
  column)

(define-class <placement-cell> (<placement-group>)
  ;; for placements within a table cell
  cell
  width
  height)

(define-method subframe-rect ((self <placement-cell>))
  (inset-rect (make-rect 0 0 (width self) (height self))
              (cell-margin (cell self))
              (cell-margin (cell self))))

(define-class <placement> (<object>)
  ;; a particular vertical position within a <placement-group>
  (subframe type: <placement-group>)
  (y type: <real>))                           ; relative to TOP of frame

(define-method to-string ((self <placement-cell>))
  (bind ((row col (cell-posn (cell self))))
    (format #f "Cell[~d,~d]" row col)))

(define-method to-string ((self <placement-subframe>))
  (format #f "~a/~s/~s" 
          (page-number (page self))
          (flow (frame self))
          (column self)))

(define-method write-object ((self <placement>) port)
  (format port "#[<placement> ~a @ ~d]"
          (to-string (subframe self))
          (y self)))


(define-method subframe-rect ((self <placement>))
  (subframe-rect (subframe self)))

(define-method subframe-rect ((self <placement-subframe>))
  (case (column self)
    ((primary-sidebar)
     (primary-sidebar-subframe (frame self)))
    ((secondary-sidebar)
     (secondary-sidebar-subframe (frame self)))
    ((across-primary-sidebar)
     (let ((ss (primary-sidebar-subframe (frame self)))
           (lc (last (column-subframes (frame self)))))
       (if ss
           (union-rect ss lc)
           lc)))
    (else
     (list-ref (column-subframes (frame self)) (column self)))))
  
(define-method origin-x ((self <placement>))
  (origin-x (subframe-rect self)))

(define-method origin-y ((self <placement>))
  (- (limit-y (subframe-rect self))
     (y self)))

