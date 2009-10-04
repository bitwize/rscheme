

(define-api (make-change-request (title <string>)
				 (group <group>)
				 summary             ;; <string> or #f
				 remarks             ;; <string> or #f
				 (properties <list>))
  (let ((cr (make <change-request>
		  id: (alloc-change-req-id)
		  title: title
		  %alloc-area: (make-area)
		  group: group
		  properties: (cons* (cons 'requestor *user*)
				     properties)
		  summary: (or summary title))))
    (table-insert! (change-request-table *application*) (id cr) cr)
    (if remarks
	(set-history! cr (cons (make <comment-request>
				     %alloc-area: (area-of cr)
				     owner: *user*
				     base-request: cr
				     activate-audit-entry: *audit-entry*
				     close-audit-entry: *audit-entry*
				     comment: remarks)
			       (history cr))))
    (work-item-activate
     (work-item-open (owner group) cr <work-request>
		     state: 'open))
    cr))


(define (find-comment-request (u <user>) (cr <change-request>))
  (let loop ((a (active-items u)))
    (if (null? a)
	#f
	(if (and (eq? (base-request (car a)) cr)
		 (instance? (car a) <comment-request>))
	    (car a)
	    (loop (cdr a))))))

(define-api (request-change-comment (cr <change-request>) (u <user>))
  (work-item-activate
   (work-item-open u cr <comment-request>)))

(define-api (change-request-no-comment (cr <change-request>))
  (let ((f (find-comment-request *user* cr)))
    (if f
	(work-item-close f))))

(define-api (change-request-assign (cr <change-request>)
                                   (new-owner <user>))
  ;; delete all pending work requests and create one
  ;; for the targetted owner
  (let ((zap (select (lambda (i)
                       (instance? i <work-request>))
                     (active-items cr))))
    (format #t "zapping ~s\n" zap)
    (for-each print zap)
    (if (null? zap)
        (error "no work to assign")
        (let ((s (state (car zap))))
          (format #t "opening new work request...\n")
          (work-item-activate
           (work-item-open new-owner
                           cr
                           <work-request>
                           state: s))
          (format #t "closing old ones...\n")
          (for-each work-item-close zap)
          (print cr)
          (values)))))
  
(define-api (change-request-change-title (cr <change-request>) 
					 (new-title <string>))
  (work-item-close
   (work-item-activate
    (work-item-open *user*
		    cr
		    <title-change>
		    new-title: new-title
		    old-title: (title cr))))
  (set-title! cr new-title))

(define-api (change-request-change-summary (cr <change-request>) 
					   (new-summary <string>))
  (work-item-close
   (work-item-activate
    (work-item-open *user*
		    cr
		    <summary-change>
		    new-summary: new-summary
		    old-summary: (summary cr))))
  (set-summary! cr new-summary))

(define-api (change-request-set-property (cr <change-request>) 
					 (property <property>)
					 new-value)
  (work-item-close
   (work-item-activate
    (if (has-property? cr property)
	(work-item-open *user*
			cr
			<property-change>
			the-property: property
			old-value: (get-property cr property)
			new-value: new-value)
	(work-item-open *user*
			cr
			<property-add>
			the-property: property
			new-value: new-value))))
  (set-property! cr property new-value))

(define-api (change-request-add-comment (cr <change-request>) (text <string>))
  (let ((rfc (find-comment-request *user* cr)))
    (if rfc
	(begin
	  (set-comment! rfc text)
	  (work-item-close rfc))
	(let ((cmt (make <comment-request>
			 %alloc-area: (area-of cr)
			 owner: *user*
			 base-request: cr
			 activate-audit-entry: *audit-entry*
			 close-audit-entry: *audit-entry*
			 comment: text)))
	  (set-history! cr (cons cmt (history cr)))))))

(define-api (change-request-work-complete (cr <change-request>) (u <user>))
  (cr-workitem-completed cr u))

(define (cr-workitem-completed (cr <change-request>) (u <user>))
  (let loop ((a (active-items cr)))
    (if (null? a)
	(error "~a: no <work-request> for user ~a" cr u)
	(if (and (eq? (owner (car a)) u)
	         (instance? (car a) <work-request>))
	    (work-item-close (car a))
	    (loop (cdr a))))))


(define-api (fs-change-complete (cr <change-request>) 
				(u <user>) 
				(fs <file-system>))
  (let loop ((a (active-items cr)))
    (if (null? a)
	(error "~a: no <fs-change> for user ~a and filesystem ~a" cr u fs)
	(if (and (eq? (owner (car a)) u)
	         (instance? (car a) <fs-change>)
		 (eq? (file-system (car a)) fs))
	    (work-item-close (car a))
	    (loop (cdr a))))))

(define-api (request-is-duplicate (cr <change-request>)
				  (dup-of <change-request>))
  (if (not (memq (state cr) '(open research)))
      (error "~a: state `~s' not `open' or `research'"
	     cr (state cr)))
  ;;
  (let ((d (assq 'duplicates (properties dup-of))))
    ;;
    (if d
	(set-cdr! d (cons cr (cdr d)))
	(set-properties! dup-of (cons (list 'duplicates cr)
				      (properties dup-of))))
    ;;
    (set-state! cr 'duplicate)
    (set-properties! cr (cons (cons 'duplicate dup-of)
			      (properties cr)))
    (cr-workitem-completed cr *user*)))
