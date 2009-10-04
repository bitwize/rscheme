;;;
;;;  Deactivate (car rp).
;;;  TeX#860
;;;
;;;  If prev-r == #f, then we are deactivating the first
;;;  node in the active list
;;;
;;;  Return the new rp and the new cur-active-width

(define (deactivate (ctx <para-context>) r cur-active-width)
  (li-delete-current! r)
  (if (li-start? r)
      ;; we just deleted the first active node...
      ;; collapse an initial delta node, if any
      (let ((n (li-current r)))
        (if (delta-node? n)
            (let ((w (space+ (active-width ctx) (delta n))))
              (set-active-width! ctx w)
              (li-delete-current! r)
              w)
            cur-active-width))
      ;; check to see if the preceding node is a delta node,
      ;; in which case we may need to coalesce with a following
      ;; delta node, or drop it if the delta is now at the end
      ;; of the active list
      (if (delta-node? (li-prev r))
          (if (li-end? r)
              (let ((w (space+ cur-active-width (delta (li-prev r)))))
                (li-back r)
                (li-delete-current! r)
                w)
              (if (delta-node? (li-current r))
                  (let* ((d (delta (li-current r)))
                         (w (space+ cur-active-width d)))
                    ;; combine_two_deltas
                    (set-delta! (li-prev r) (space+ (delta (li-prev r)) d))
                    (li-delete-current! r)
                    w)
                  cur-active-width))
          cur-active-width)))
      
            
      
