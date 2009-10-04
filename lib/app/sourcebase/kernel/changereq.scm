
(define (alloc-change-req-id)
   (let ((n (add1 (num-change-requests *application*))))
     (set-num-change-requests! *application* n)
     n))

;;
;; this is the hook for controlling the evolution in state
;; of a change request.
;;
;; it is called when all the active work items have been closed
;;

(define (next-change-request-state (cr <change-request>) closed-item)
  (bind ((ns u (compute-next-state-and-owner cr closed-item)))
    (set-state! cr ns)
    (if (instance? u <user>)
	(work-item-activate (work-item-open u cr <work-request> state: ns)))))

(define (compute-next-state-and-owner (cr <change-request>) closed-item)
  (case (state cr)
    ;;
    ((open)
     (values 'research (owner closed-item)))
    ;;
    ((research)
     (values 'fixing (owner closed-item)))
    ;;
    ((fixing)
     (let* ((fss (map file-system
		      (select (rcurry instance? <fs-change>)
			      (history cr))))
	    (ws (select (curry policy-want-snapshot? (group cr)) fss)))
       (if (pair? ws)
	   (begin
	     (for-each (lambda (fs)
			 (work-item-activate
			  (work-item-open
			   (owner fs)
			   cr
			   <integration-request>
			   file-system: fs)))
		       ws)
	     (values 'integration #t))
	   (completion-phase cr))))
    ;;
    ((integration)
     (completion-phase cr))
    ;;
    ((check-off)
     (values 'closed #f))
    ;;
    ;;  will be re-activated (in check-off state) when original CR
    ;;  is moved to check-off state
    ;;
    ((duplicate)
     (values 'duplicate #f))
    ;;
    (else
     (error "~a: can't procede from state `~s'" (id cr) (state cr)))))

;; drag duplicates along completion phase

(define (completion-phase (cr <change-request>))
  (let ((d (assq 'duplicates (properties cr))))
    (if d
	(for-each (lambda (dup)
		    (if (and (policy-check-off? (group dup))
			     (assq 'requestor (properties dup)))
			(begin
			  (set-state! dup 'check-off)
			  (work-item-activate
			   (work-item-open
			    (cdr (assq 'requestor (properties cr)))
			    dup
			    <work-request>
			    state: 'check-off)))
			(set-state! dup 'closed)))
		  (cdr d)))
    ;;
    ;; return the next state and owner of this CR
    ;;
    (if (and (policy-check-off? (group cr))
	     (assq 'requestor (properties cr)))
	(values 'check-off (cdr (assq 'requestor (properties cr))))
	(values 'closed #f))))
