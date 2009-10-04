
(define (review-before-exit?)
  (verify-something "Review changes?" 
		    "Review changes before exiting?"))

(define-interactive (exit-client-with-review client)
  (interactive (client))
  (if (and (any? dirty? (open-views client))
	   (review-before-exit?))
      (for-each close-view-with-review (open-views client))
      (for-each close-view (open-views client)))
  (exit-client client))

;
(define-interactive (exit-client client)
  (interactive (client))
  (dm 107 "exit-client ~s" client)
  ;
  (unmap-window (main-menu client))
  (if (toolbox-menu client)
      (unmap-window (toolbox-window (toolbox-menu client))))
  ;; queue the event so it is processed by the main event-loop thread
  ;; (since we are running in another thread at this point, its
  ;; kind of hard to get back over there and kill the event loop)
  (queue-event (on-display client) 'exit-loop))

(global-set-key '(#\C-x #\C-c) exit-client-with-review)


(define-interactive (halt)
  (interactive)
  (process-exit 0))

(global-set-key '(#\C-\) halt)
