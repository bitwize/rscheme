
(define-interactive (group-objects view selection)
  (interactive (owner) (selection))
  (clear-current-selection! view)
  (let* ((parent (page-contents (view-page (underlying-object view))))
         (g (make <user-group>
                  in-document: (in-document parent)
                  parent-object: parent
                  origin: $zero-point
                  group-contents: selection
                  graphic-bounding-box: $zero-rect)))
    (for-each
     (lambda (item)
       (let ((old-parent (parent-object item)))
         ;; unlink the selection from its old parent
         (set-group-contents! old-parent 
                              (delq item (group-contents old-parent)))
         ;; relink the item into the new group
         (set-parent-object! item g)))
     selection)
    (do-select view g 0)
    (values)))

(define-method accum-handles ((self <user-group>) proc)
  (let ((adj (general-transform self)))
    (for-each
     (lambda (mem)
       (accum-handles mem
                      (lambda (owner point id)
                        (proc self (transform point adj) 0)))))
   (group-contents self)))

;;; `paint-artwork*' is already implemented in redraw.scm

(define-method pick-list ((self <user-group>) (pt <point>) ctm)
  (let ((ctm (concatenate-transform ctm (general-transform self))))
    (let loop ((lst (group-contents self))
               (a '()))
      (if (null? lst)
          (begin
            (if (pair? a) (dm "Group pick: ~s" a))
            a)
          (let ((q (pick-list (car lst) pt ctm)))
            (if (null? q)
                (loop (cdr lst) a)
                (loop (cdr lst) (append!
                                 a
                                 (map (lambda (picked)
                                        (cons (car picked) self))
                                      q)))))))))
                                      
                                     
;;;

(define-method start-active-drag ((self <user-group>)
                                  (in-view <open-view>)
                                  (initial-pt <point>))
  (start-active-drag-handle self in-view -1 initial-pt))

;;;

(define-method start-active-drag-handle ((self <user-group>)
                                         (in-view <open-view>)
                                         handle-id
                                         (initial-pt <point>))
  (generic-dragger self in-view 0 initial-pt))

;;

(define (whole-path-mover (self <user-group>) start-pt)
  (lambda ((p <point>))
    (point- p start-pt)))

(define-method make-adjuster ((self <user-group>)
                               handle-id
                               o->d
                               initial-thunk)
  (mha
   (let ((start-pt (initial-thunk)))
     (lambda ((p <point>))
       (point- p start-pt)))
   (group-draw-proc self o->d)))

;;;   the draw proc procedure, when the whole thing is moving

(define (group-draw-proc self o->d)
  (lambda (shift)
    (lambda (win gc)
      (with-x11-device
       win
       (lambda (dev)
         (set-x-gc! dev gc)
         (translate dev (size->point (size* shift -1)))
         (concat dev (general-transform self))
         (concat dev o->d)
         (for-each
          (lambda (mem)
            (paint-artwork mem dev))
          (group-contents self)))))))
