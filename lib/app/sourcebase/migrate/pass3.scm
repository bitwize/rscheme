
;;
;;  Pass 3 (step 3XX)
;;
;;  defects
;;

(define (migrate-defects)
   ;;  construct the <change-request>'s
   (for-each (lambda ((d Defect))
   		(let ((n (make-migrated-change-req d)))
		   (table-insert! (change-request-table *application*) 
		                   (id n) 
				   n)
		   (set-forward! d n)))
	     (cmvc-table 'Defects))
   (set-num-change-requests! *application*
			     (let loop ((m 0)
					(d (value-sequence 
					    (change-request-table
					     *application*))))
			       (if (null? d)
				   m
				   (loop (max m (id (car d))) (cdr d)))))
   #t)

(define (migrate-defect-ownership)
  (for-each migrate-one-defect-owner (cmvc-table 'Defects))
  #t)

(define (migrate-one-defect-owner (d Defect))
  (let ((cr (forward d))
	(my-id (id d))
	(d-owner (forward (cmvc-user (owner-id d)))))
    (case (string->symbol (state d))
      ((open)
       (set-state! cr 'open)
       (work-item-activate
	(work-item-open d-owner cr <work-request> state: 'open)))
      ((verify)
       (set-state! cr 'check-off)
       ;; create a <work-request> work item for each
       ;; verification record
       (for-each (lambda ((v Verify))
		   (if (eq? (defect-id v) my-id)
		       (work-item-activate
			(work-item-open
			 (forward (cmvc-user (user-id v)))
			 cr
			 <work-request>
			 state: 'check-off))))
		 (cmvc-table 'Verify)))
      ((working)
       (if (not (any? (lambda ((t Track))
			(eq? (defect-id t) my-id))
		      (cmvc-table 'Tracks)))
	   ;; there will not be a <fs-change> work item,
	   ;; so put it in the research state and add a 
	   ;; work item
	   (begin
	     (set-state! cr 'research)
	     (work-item-activate
	      (work-item-open d-owner cr <work-request> state: 'research)))
	   ;; there will be an <fs-change>
	   (set-state! cr 'fixing)))
      ((canceled)
       (set-state! cr 'cancelled))
      ((closed)
       (set-state! cr 'closed)))))

(define (migrate-notes)
   (for-each
     (lambda ((n Note))
        (let* ((mr (make-migrated-note n))
	       ((cr <change-request>) (base-request mr)))
	   (set-history! cr (cons mr (history cr)))))
     (cmvc-table 'Notes))
   #t)

;;

(define (make-migrated-note (n Note))
   (let* (((u <user>) (forward (cmvc-user (user-id n))))
          (a (make <audit-log-entry>
	 	   user: u
		   operation: 'change-request-comment
		   arg-list: '()
		   result: #t
		   info: '()
		   timestamp: (cmvc->time (add-date n)))))
     (make <comment-request>
           owner: u
	   base-request: (forward (cmvc-defect (defect-id n)))
	   activate-audit-entry: a
	   close-audit-entry: a
	   comment: (remarks n))))
			    

(define (make-migrated-change-req (d Defect))
   (make <change-request>
   	 %alloc-area: (make-area)
	 id: (string->number (name d))
	 title: (abstract d)
	 group: (forward (cmvc-component (component-id d)))
	 summary: (abstract d)
	 state: 'migrate
	 properties: (migrate-defect-properties d)))

(define-syntax (migrate-user-attribute name d)
  (cons (mquote name)
        (parse-property-value
		(table-lookup (property-table *application*)
		 	      (symbol->string (mquote name)))
		(name d))))

(define-syntax (migrate-user-attributes d . attribs)
   (letrec-syntax ((ua (syntax-form () '())
   		       (syntax-form (a . more)
		       	 (let ((rest (ua . more)))
			   (if (a d)
			       (cons (migrate-user-attribute a d) rest)
			       rest)))))
     (ua . attribs)))
	
(define (migrate-defect-properties (d Defect))
   (let ((most (migrate-user-attributes d
					prefix
					severity
					reference
					answer)))
     (cons (cons 'requestor (forward (cmvc-user (originator-id d))))
           most)))

(define (history<? (a History) (b History))
  (if (string=? (add-date a) (add-date b))
      (let-syntax ((action-val (syntax-form (x)
				 (cadr (assoc x '(("open" 0)
						  ("accept" 1)
						  ("cancel" 2)
						  ("return" 3)
						  ("note" 4)
						  ("modify" 5)
						  ("assign" 6)
						  ("verify" 7)
						  ("close" 8)))))))
	(< (action-val (action a))
	   (action-val (action b))))
      (time<? (cmvc->time (add-date a))
	      (cmvc->time (add-date b)))))

(define (migrate-defect-history)
  (for-each
   (lambda ((d Defect))
     (let* ((my-id (id d))
	    (hist (sort (select (lambda ((h History))
				  (eq? (defect-id h) my-id))
				(cmvc-table 'History))
			history<?))
	    (cr (forward (cmvc-defect my-id))))
       (migrate-one-defect-history cr hist)))
   (cmvc-table 'Defects))
  #t)

(define (migrate-one-defect-history (cr <change-request>) hist)
  ;;
  (migration-progress " ~a: ~j\n" (id cr) (map action hist))
  (let loop ((hist (cdr hist))
	     (s 'open)
	     (crh (history cr)))
    (if (null? hist)
	(set-history! cr crh)
	(let (((h History) (car hist)))
	  (let ((u (forward (cmvc-user (user-id h))))
		(action (string->symbol (action h)))
		(t (cmvc->time (add-date h))))
	    (if (memq action '(verify return cancel close accept))
		(let* ((a (make <audit-log-entry>
				user: u
				operation: 'change-request-advance
				arg-list: (list cr u)
				result: #t
				info: '()
				timestamp: t))
		       (i (make <work-request>
				owner: u
				base-request: cr
				activate-audit-entry: a
				close-audit-entry: a
				state: s)))
		  (loop (cdr hist)
			(cadr (assq action '((verify check-off)
					     (return returned)
					     (cancel cancelled)
					     (close closed)
					     (accept fixing))))
			(cons i crh)))
		;; else, its one of: (note assign modify)
		(case action
		  ((note modify)
		   ;; ignore them... picked up by notes migration
		   (loop (cdr hist) s crh))
		  ((assign)
		   (let* ((a (make <audit-log-entry>
				   user: u
				   operation: 'change-request-assign
				   arg-list: (list cr u)
				   result: #t
				   info: '()
				   timestamp: t))
			  ;; this is actually the work-request that would
			  ;; have been *closed* by the `assign', not the new
			  ;; one created by the assign.
			  (i (make <work-request>
				   owner: u
				   base-request: cr
				   activate-audit-entry: a
				   close-audit-entry: a
				   state: s)))
		     (loop (cdr hist) s (cons i crh))))
		  (else
		   (error "~a: unknown action `~a'" cr action)))))))))
