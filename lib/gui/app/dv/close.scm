
(define (save-before-close? t)
  (verify-something "Save changes?" 
		    (format #f "Save changes to `~a' before closing?" t)))

(define-interactive (close-view-with-review (self <open-view>))
  (interactive (open-view))
  (if (and (dirty? self)
	   (save-before-close? (file-name (in-document self)))
           (<= (length (open-views (in-document self))) 1))
      (save-file (in-document self)))
  (close-view self))

(define-interactive (close-view (self <open-view>))
  (interactive (open-view))
  (unmap-window (main-window self)))

;;

(global-set-key '(#\C-x #\C-k) close-view-with-review)
