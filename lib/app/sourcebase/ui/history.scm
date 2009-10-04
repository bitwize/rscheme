
(define $history-item-kinds
  '((<comment-request> "Comment")
    (<title-change> "Modify")
    (<summary-change> "Modify")
    (<property-change> "Modify")
    (<property-add> "Modify")
    (<integration-request> "Integrate")
    (<fs-change> "File Changes")
    (<work-request> "Modify")))

(define-method render-history-item ((self <work-item>))
  (let* ((n (class-name (object-class self)))
	 (a (assq n $history-item-kinds))
	 (lbl (if a (cadr a) (symbol->string n))))
    (format #t "-------- ~a -- ~a ~a ~a --------\n"
	    (time->string (timestamp (close-audit-entry self))
			  "%Y-%m-%d %H:%M:%S")
	    lbl
	    (make-string (max 2 (- 18 (string-length lbl))) #\-)
	    (name (owner self)))))

(define-method render-history-item ((self <comment-request>))
  (next-method)
  (display (comment self))
  (let (((s <string>) (comment self)))
    (if (not (eq? (string-ref s (sub1 (string-length s))) #\newline))
	(newline))
    (newline)))

(define-method render-history-item ((self <code-review>))
  (next-method)
  (format #t "Changes to ~a reviewed\n\n" (name (group self))))

(define-method render-history-item ((self <work-request>))
  (next-method)
  (if (eq? (operation (close-audit-entry self)) 
           'change-request-assign)
      (begin
        (format #t "Assign old owner: ~a\n" 
                (name (owner self)))
        (format #t "       New owner: ~a\n\n" 
                (name (vector-ref (arg-list (close-audit-entry self)) 1))))
      (begin
        (format #t "Finish old state: ~a\n" (state self))
        (format #t "       New state: ~a\n\n" (work-request-new-state self)))))

(define (work-request-new-state (self <work-request>))
  (let loop ((h (history (base-request self)))
             (s (state (base-request self))))
    (if (and (pair? h)
             (not (eq? (car h) self)))
        (if (instance? (car h) <work-request>)
            (loop (cdr h) (state (car h)))
            (loop (cdr h) s))
        s)))

(define-method render-history-item ((self <title-change>))
  (next-method)
  (format #t "Old title: ~a\n" (old-title self))
  (format #t "New title: ~a\n\n" (new-title self)))

(define-method render-history-item ((self <summary-change>))
  (next-method)
  (format #t "Old summary:\n~a\n" (old-summary self))
  (format #t "New summary:\n~a\n\n" (new-summary self)))

(define-method render-history-item ((self <property-add>))
  (next-method)
  (format #t "Property: ~a\n" (name (the-property self)))
  (format #t "New value: ~a\n\n" (property-value-string-form
				  (new-value self))))

(define-method render-history-item ((self <property-change>))
  (next-method)
  (format #t "Property: ~a\n" (name (the-property self)))
  (format #t "Old value: ~a\n" (property-value-string-form
				  (old-value self)))
  (format #t "New value: ~a\n\n" (property-value-string-form
				  (new-value self))))

(define-method render-history-item ((self <fs-change>))
  (next-method)
  (format #t "~d new versions in filesystem ~a\n\n" 
	  (length (new-versions self))
	  (name (file-system self))))

(define-method render-history-item ((self <fs-change>))
  (next-method)
  (format #t "Filesystem: ~a\n" (name (file-system self)))
  (format #t "Changes:    ~d\n\n" (length (new-versions self)))
  (for-each 
   (lambda (v)
     (format #t "    ~6a "
	     (if (version-tag v)
		 (version-tag->string
		  (version-tag v))
		 ;; new version hasn't been checked in yet
		 "*co*"))
     (let ((p (node->paths 
	       (file-system self)
	       (versioned-object v))))
       (if (null? p)
	   (format #t "[node ~d]\n" 
		   (id (versioned-object v)))
	   (format #t "~a\n"
		   (fs-path->string (car p))))))
   (new-versions self))
  (newline))

(define-method render-history-item ((self <integration-request>))
  (next-method)
  (format #t "Filesystem: ~a\n" (name (file-system self)))
  (for-each (lambda (sn)
	      (format #t "Snapshot:   ~a\n" (name sn)))
	    (snapshots self))
  (newline))
