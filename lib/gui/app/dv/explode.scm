;;;
;;;  Part of the <graphic-object> protocol
;;;

(define-interactive (explode view items)
  (interactive (owner) (selection))
  (let ((sel (key-sequence (current-selection view))))
    (clear-current-selection! view)
    ;
    (for-each
     (lambda (item)
       (for-each
        (lambda (new-item)
          (add-to-current-selection! view new-item))
        (explode->list item)))
     items)
    (set-need-to-recompute-handles! view #t)
    (need-to-clear-and-redraw (content-window view))
    (values)))

