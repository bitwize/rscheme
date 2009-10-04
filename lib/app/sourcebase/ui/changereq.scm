
(define-method requestor ((self <change-request>))
  (get-property self 'requestor #f))
 
(define-method render-full ((self <change-request>))
  (format #t "Id:        ~d\n" (id self))
  (format #t "State:     ~a\n" (state self))
  (format #t "Title:     ~a\n" (title self))
  (format #t "Group:     ~a\n" (name (group self)))
  (if (not (null? (interest self)))
      (format #t "Interest:  ~j\n" (map name (interest self))))
  (if (not (null? (active-items self)))
      (format #t "Workers:   ~j\n" 
	      (map name (unionq (map owner (active-items self)) '()))))
  (for-each (lambda (p)
	      (if (symbol? (car p))
		  (format #t "~11a~j\n" 
			  (format #f "~C:" (car p))
			  (if (pair? (cdr p))
			      (cdr p)
			      (list (cdr p))))
		  (format #t "~11a~a\n"
			  (format #f "~C:" (name (car p)))
			  (property-value-string-form (cdr p)))))
	    (properties self))
  (format #t "\nSummary\n")
  (format #t "-------\n")
  (display (summary self))
  (newline)
  (let ((changes (select (lambda (wi)
			   (instance? wi <fs-change>))
			 (active-items self))))
    (if (pair? changes)
	(begin
	  (format #t "\nOutstanding Changes\n")
	  (format #t "-------------------\n")
	  (for-each (lambda ((fsc <fs-change>))
		      (format #t " ~a ~a:\n" 
			      (name (owner fsc))
			      (name (file-system fsc)))
		      (for-each 
		       (lambda (v)
			 (format #t "    ~6a "
				 (if (version-tag v)
				     (version-tag->string
				      (version-tag v))
				     ;; new version hasn't been checked in yet
				     "*co*"))
			 (let ((p (node->paths 
				   (file-system fsc)
				   (versioned-object v))))
			   (if (null? p)
			       (format #t "[node ~d]\n" 
				       (id (versioned-object v)))
			       (format #t "~a\n"
				       (fs-path->string (car p))))))
		       (new-versions fsc)))
		    changes))))
  (format #t "\nHistory\n")
  (format #t "-------\n")
  (for-each render-history-item (reverse (history self))))

(define (newest-of-heads . lst)
  (let loop ((lst lst)
             (best #f)
             (audit #f)
             (t #f))
    (if (null? lst)
        best
        (if (pair? (car lst))
            (let ((audit (or (close-audit-entry (caar lst))
                             (activate-audit-entry (caar lst)))))
              (if (not t)
                  (loop (cdr lst) (caar lst) audit (timestamp audit))
                  (if (time>? (timestamp audit) t)
                      (loop (cdr lst) (caar lst) audit (timestamp audit))
                      (loop (cdr lst) best audit t))))
            (values audit best)))))

(define-method render-xml ((self <audit-log-entry>))
  `(event (login ,(name (user self)))
          (fullname ,(full-name (user self)))
          (date ,(time->string (timestamp self) "%Y-%m-%d"))
          (time ,(time->string (timestamp self) "%H:%M:%S"))))

(define (work-item-xml-times (self <work-item>))
  `((open ,(render-xml (activate-audit-entry self)))
    ,@(if (close-audit-entry self)
          `((close ,(render-xml (close-audit-entry self))))
          '())))

(define-method render-xml ((self <work-item>))
  `(remark ,@(work-item-xml-times self)
           (content ,(format #f "A ~a work item" 
                             (class-name (object-class self))))))

(define-method render-xml ((self <work-request>))
  `(work-request (from-state ,(state self))
                 ,@(work-item-xml-times self)))
                 
                 
(define-method render-xml ((self <fs-change>))
  `(fs-change (file-system ,(name (file-system self)))
              ,@(work-item-xml-times self)))

(define-method render-xml ((self <code-review>))
  `(code-review (group ,(name (group self)))
                ,@(work-item-xml-times self)))
                

(define-method render-xml ((self <title-change>))
  `(title-change (from ,(old-title self))
                 (to ,(new-title self))
                 ,@(work-item-xml-times self)))

(define-method render-xml ((self <summary-change>))
  `(summary-change (from ,(old-summary self))
                   (to ,(new-summary self))
                   ,@(work-item-xml-times self)))

(define-method render-xml ((self <property-change>))
  `(property-change (property ,(name (the-property self)))
                    (from ,(name (old-value self)))
                    (to ,(name (new-value self)))
                    ,@(work-item-xml-times self)))

(define-method render-xml ((self <property-add>))
  `(property-add (property ,(name (the-property self)))
                 (to ,(name (new-value self)))
                 ,@(work-item-xml-times self)))

(define-method render-xml ((self <integration-request>))
  `(integration-request (filesystem ,(name (file-system self)))
                        (snapshots ,@(map (lambda (s)
                                            (list 'snapshot (name s)))
                                          (snapshots self)))
                        ,@(work-item-xml-times self)))


(define-method linked-xml ((self <string>))
  self)

(define-method linked-xml ((self <property-value>))
  (name self))

(define-method linked-xml ((self <user>))
  `(user ,(name self)))
  
(define-method render-xml ((self <comment-request>))
  `(remark ,(render-xml (or (close-audit-entry self)
                            (activate-audit-entry self)))
           (content ,(comment self))))

(define-method render-xml ((self <change-request>))
  (let ((o (owner (if (null? (active-items self))
                      (car (history self))
                      (car (active-items self)))))
        (l (newest-of-heads (history self)
                            (waiting-items self)
                            (active-items self))))
    (with-output-to-port
     (current-error-port)
     (lambda ()
       (print self)))
    ;
    `(changereq
      (id ,(id self))
      (state ,(state self))
      (updated ,(render-xml l))
      (group ,(name (group self)))
      (title ,(title self))
      (owner (login ,(name o))
             (fullname ,(full-name o)))
      (opened ,(render-xml (activate-audit-entry (last (history self)))))
      (summary ,(summary self))
      (properties
       ,@(map (lambda (p)
                (if (symbol? (car p))
                    `(,(car p) ,(linked-xml (cdr p)))
                    `(userdef (name ,(name (car p)))
                              ,(linked-xml (cdr p)))))
              (properties self)))
      (current
       ,@(map render-xml (reverse (active-items self))))
      (appendix
       ,@(map render-xml (reverse (history self)))))))
