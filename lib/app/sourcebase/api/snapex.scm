(define-class <version-chain-error> (<error>)
  (terminal-version type: <node-version>)
  (delta-versions type: <list>)
  (needs-to-extend? type: <boolean>)) ; #t=>need forward chain, #f=>reverse

(define-class <ambiguous-version-chain> (<version-chain-error>)
  (multiple-next-steps type: <list>))

(define-class <incomplete-version-chain> (<version-chain-error>)
  (unmatched-versions type: <list>))

(define-method display-object ((self <ambiguous-version-chain>) port)
  (format port "SRV-000 Ambiguous version chain\n")
  (format port "   terminal version: ~s\n" (terminal-version self))
  (format port "   delta versions: ~s\n" (delta-versions self))
  (format port "   extending?: ~s\n" (needs-to-extend? self))
  (format port "   next steps: ~s\n" (multiple-next-steps self)))

(define-method display-object ((self <incomplete-version-chain>) port)
  (format port "SRV-000 Incomplete version chain\n")
  (format port "   terminal version: ~s\n" (terminal-version self))
  (format port "   delta versions: ~s\n" (delta-versions self))
  (format port "   extending?: ~s\n" (needs-to-extend? self))
  (format port "   unmatched: ~s\n" (unmatched-versions self)))


;;
;;  snapshot extend and retract
;;

(define-api (snapshot-extend (snap <snapshot>) (change-reqs <list>))
  (compute-snapshot-extensions snap 
			       (unionq '() change-reqs)
			       last-in-complete-chain))

;; `retract' is the inverse of `extend'

;;(define-api (snapshot-retract (snap <snapshot>) (change-reqs <list>))
;;  (compute-snapshot-extensions snap change-reqs first-in-complete-chain))

(define (compute-snapshot-extensions (snap <snapshot>)
				     (change-reqs <list>)
				     (compute-chain <function>))
  (if (not (eq? (snapshot-state snap) 'active))
      (error "~a: snapshot in filesys ~a is in state `~a', not active"
	     (name snap)
	     (name (versioned-object snap))
	     (snapshot-state snap)))
  ;;
  (let ((nvtbl (make-table eq? integer->hash))
	(ovtbl (node-version-map snap))
	(fs (versioned-object snap))
	(ext (assq 'extend (properties snap)))
	(ireq-list (get-active-integration-requests (versioned-object snap)
						    change-reqs)))
    ;;
    (if (not ext)
	(error "~a: not an extensible snapshot" (name snap)))
    ;;
    ;;  build an index of all the new versions
    ;;
    (for-each
     (lambda ((cr <change-request>))
       (if (not (eq? (state cr) 'integration))
	   (error "~a: change request is not ready for integration: ~d"
		  (name snap)
		  (id cr)))
       ;;
       (for-each
	(lambda (wi)
	  (if (and (instance? wi <fs-change>)
		   (eq? (file-system wi) fs))
	      (begin
		(format #t "extending with... ~s\n" wi)
		(for-each (lambda (v)
			    (format #t "    ~s\n" v)
			    (let ((i (id (versioned-object v))))
			      (table-insert! 
			       nvtbl
			       i
			       (cons v (or (table-lookup nvtbl i) '())))))
			  (new-versions wi)))))
	(history cr)))
     change-reqs)
    ;;
    ;; for each node, make sure it is consistent, at the same
    ;; time accumulating the last version of each node
    ;;
    (let ((lasts-list '()))
      (table-for-each
       nvtbl
       (lambda (h node-id vlist)
	 (let ((ov (table-lookup ovtbl node-id)))
	   (format #t "processing ~d ~a\n" 
		   node-id
		   (if ov
		       (node->paths (versioned-object snap)
				    (versioned-object ov) )
		       "-new-"))
	   (set! lasts-list 
		 (cons (cons node-id (compute-chain vlist ov))
		       lasts-list)))))
      ;;
      ;; implement the changes
      ;;
      ;;  1. update the snapshot's node-id map
      ;;
      (for-each (lambda ((p <pair>))
		  (table-insert! ovtbl (car p) (cdr p)))
		lasts-list)
      ;;
      ;;  2. add this set of change-req's to the "extend" snapshot
      ;;     property
      ;;
      (set-cdr! ext (cons change-reqs (cdr ext)))
      ;;
      ;;  3. add this snapshot to the change-req's integration requests
      ;;
      (for-each (lambda ((ireq <integration-request>))
		  (set-snapshots! ireq (cons snap (snapshots ireq))))
		ireq-list)
      ;;
      lasts-list)))

;;
;; checks if the list of versions forms a complete and exact chain
;; of revisions extending the last node version
;;
;; if tip-most is #f, then new-versions must form a complete chain
;;

;; probably a better way than this..!

(define (last-in-complete-chain new-versions old-version)
  (compute-complete-chain new-versions old-version #t))

;;(define (first-in-complete-chain removing-versions newest-version)
;;  (compute-complete-chain removing-versions newest-version #f))

(define (compute-complete-chain (version-steps <list>) tip-most extends?)
  (let ((steps-queue (unionq '() (append version-steps '()))))
    (let loop ((tip tip-most))
      (format #t "=> ~s\n" tip)
      (let ((next-step (select (if extends?
				   (lambda (v)
				     (eq? (previous-version v) tip))
				   (lambda (v)
				     (eq? v (previous-version tip))))
			       steps-queue)))
	(if (null? next-step)
	    (if (null? steps-queue)
		tip
		(error
		 (make <incomplete-version-chain>
		       unmatched-versions: steps-queue
		       terminal-version: tip-most
		       delta-versions: version-steps
		       needs-to-extend?: extends?)))
	    (if (null? (cdr next-step))
		(begin
		  (format #t "post: ~s\n" (car next-step))
		  (set! steps-queue (delq! (car next-step) steps-queue))
		  (loop (car next-step)))
		(error
		 (make <ambiguous-version-chain>
		       multiple-next-steps: next-step
		       terminal-version: tip-most
		       delta-versions: version-steps
		       needs-to-extend?: extends?))))))))


#|
  (display "===> changes being submitted:\n" port)
  (print vlist)
  (display "===> current (snapshot) version:\n")
  (print (table-lookup ovtbl node-id))
  (error "~a: some change requests are missing for ~a" 
	 (name snap)
	 (let ((x (table-lookup (pathname-table fs) node-id)))
	   (if x
	       (string-join #\/ (reverse (car x)))
	       (format #f "[node ~d]" node-id))))
|#
