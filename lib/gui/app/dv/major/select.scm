
(define (multiple-drag items in-view at0)
  (let ((lst (map (lambda (item)
                    (start-active-drag item in-view at0))
                  items)))
    (set-active-drag-proc! in-view
                           (vector
                            ;; mouse-motion handler
                            (lambda (new-pt flags)
                              (for-each (lambda (c)
                                          ((vector-ref c 0) new-pt flags))
                                        lst))
                            ;; mouse-up handler
                            (lambda (new-pt flags)
                              (for-each (lambda (c)
                                          ((vector-ref c 1) new-pt flags))
                                        lst))))))
(define (? expr)
  (dm "Result => ~s" expr)
  expr)

(define (selection-tool-button-press (in-view <open-view>)
				     (at0 <point>) ;; window device coords
				     modifier-state)
  (let ((at (window->sheet in-view at0)))
    (if (and (not (shift-state? modifier-state))
             (> (table-size (current-selection in-view)) 1)
             (or ; note that it might be nice to let the multiple-
              ; selection drag happen even if the handle clicked was
              ; in the geometry for a different object.  That would
              ; allow you to do translations snapped to different
              ; objects, which may be very convenient sometimes.
              ; (e.g., move this box by the size of this other line)
              (table-lookup (current-handles in-view) at)
              (any? (lambda (p)
                      (table-lookup (current-selection in-view) (cdr p)))
                    (pick-list in-view at))))
        ;; handle multiple selection or non-handle drag differently
        (multiple-drag (key-sequence (current-selection in-view))
                       in-view
                       at0)
        (bind ((h h-at (table-lookup (current-handles in-view) at)))
          (dm "at ~s, h => ~s @ ~s" at h h-at)
          (if h
              ; clicked in an active handle...
              (set-active-drag-proc!
               in-view
               (start-active-drag-handle (car h) in-view (cdr h) at0))
              ; clicked elsewhere...
              (let ((lst (pick-list in-view at)))
                ; cancel any currently active drag
                (set-active-drag-proc! in-view #f)
                (dm "PICK: ~s" lst)
                ;
                (if (pair? lst)
                    ; if any of the picked items are currently selected,
                    ; then just drag the whole bunch
                    (if (any? (lambda (p)
                                (table-lookup (current-selection in-view) 
                                              (cdr p)))
                              lst)
                        (multiple-drag (key-sequence (current-selection in-view))
                                       in-view
                                       at0)
                        ; otherwise, select the new one
                        (let ((item (do-select in-view (cdar lst) modifier-state)))
                          ; start an active drag
                          (set-active-drag-proc! 
                           in-view
                           (start-active-drag item in-view at0))))
                    ; nothing was picked... it was a click in a blank area
                    (begin
                      (clear-current-selection! in-view)
                      (set-status-line! in-view "")))
                (set-need-to-recompute-handles! in-view #t)
                (clear-area (content-window in-view))
                (redraw-open-view in-view)))))))

;;;  the "selectable object" protocol is the following:
;;;
;;;    (draw-handle <SO> <id> <point> <x-window> <x-gcontext>)
;;;       Draw the given (id'th) handle of <SO> in the given
;;;       window
;;;
;;;    (status-line-when-sel <SO>)  ==>  <string>
;;;
;;;    (compute-view-ctm-for <SO>)  ==>  <affine-transform>
;;;
;;;    (paint-artwork* <SO> <device>)
;;;       Paint the artwork for selectable object
;;;
;;;    (accum-handles <SO> <procedure>)
;;;       Call the given procedure for each handle
;;;       on the selectable object
;;;
;;;    (start-active-drag-handle <SO> ...)
;;;

(define (do-select (in-view <open-view>) item mstate)
  (set-status-line! in-view (status-line-when-sel item))
  (if (not (shift-state? mstate))
      (clear-current-selection! in-view))
  (add-to-current-selection! in-view item)
  (set-need-to-recompute-handles! in-view #t)
  (need-to-clear-and-redraw (content-window in-view))
  item)

(add-major-mode!
 (make <major-mode>
       name: 'select
       button-press-proc: selection-tool-button-press))

(define-interactive (select-all-visible in-view)
  (interactive (owner))
  (clear-current-selection! in-view)
  (select-all-visible* (page-contents (view-page (underlying-object in-view)))
                       in-view)
  (set-need-to-recompute-handles! in-view #t)
  (clear-area (content-window in-view) exposures?: #t)
  ;(redraw-open-view in-view)
  (values))

(define-method select-all-visible* ((self <graphic-object>) view)
  (add-to-current-selection! view self))

(define-method select-all-visible* ((self <root-group>) view)
  (for-each
   (rcurry select-all-visible* view)
   (group-contents self)))

(graphic-set-key '(#\M-a) select-all-visible)
