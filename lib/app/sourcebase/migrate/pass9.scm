
(define (migrate-unlock-dirs)
  (for-each
   (lambda ((u <user>))
     (let ((codirs (select (lambda ((co <checkout>))
			     (instance? co <dir-checkout>))
			   (check-outs u))))
       (for-each
	(lambda ((co <dir-checkout>))
	  (dir-node-delta (file-system co)
			  (string->fs-path "/bogus")
			  u
			  '()
			  "migrating file structure from CMVC"
			  (versioned-object (checked-out co))))
	codirs)))
   (value-sequence (user-table *application*))))

(define (migrate-defect-finalize)
  (for-each (lambda (cr)
	      (set-history! 
	       cr
	       (sort (history cr)
		     (lambda (a b)
		       (time>? (timestamp (close-audit-entry a))
			       (timestamp (close-audit-entry b)))))))
	    (value-sequence (change-request-table *application*)))
  #t)

(define (migrate-finalize)
  ;;disable destruction of state data for testing
  (set-service-state! *application* 'available)
  (set-state! *application* 'ready)
  (set-mail-queue! *application* '())
  (format #t "Migration complete\n"))

