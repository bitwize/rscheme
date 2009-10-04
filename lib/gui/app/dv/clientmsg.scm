
(define (my-client-message-handler display #rest ignore 
				           #key window type format data)
  (let ((t (atom->symbol type)))
    (dm 134 "~s: client message ~s ~s" window t data)
    (case t
      ((WM_PROTOCOLS)
       (let ((p (atom->symbol (vector-ref data 0))))
	 (case p
	   ((WM_DELETE_WINDOW)
	    (let ((f (get-property window 'wm-delete-window #f)))
              (if f
                  (f (list window type format data)))))
	   (else
	    (dm 135 "~s: WM protocol `~s' ignored" window p)))))
      (else
       (dm 136 "~s: client message (type `~s') ignored" window t)))))

;;;
;;;  Utility procedure to add a WM_DELETE_WINDOW handler to a window
;;;

(define (add-close-handler window proc)
  (change-property window
                   "WM_PROTOCOLS"
                   (list (intern-atom (drawable-display window)
                                      "WM_DELETE_WINDOW"))
                   "ATOM"
                   32)
  (set-property! window 'wm-delete-window proc))
