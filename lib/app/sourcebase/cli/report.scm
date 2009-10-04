
(define (list->iterator lst)
  (lambda ()
    (if (null? lst)
	#f
	(let ((nxt (car lst)))
	  (set! lst (cdr lst))
	  nxt))))

    
(define (user-iterator)
  (list->iterator (value-sequence (user-table *application*))))

(define (changereq-iterator)
  (changereq-iterator* (lambda (x) #t)))

(define (changereq-state-iterator s)
  (changereq-iterator* (lambda ((cr <change-request>))
			 (eq? (state cr) s))))

(define (changereq-iterator* ok?)
  (list->iterator 
   (select ok?
	   (value-sequence (change-request-table *application*)))))

(define (fixing-changereq-iterator)
  (changereq-state-iterator 'fixing))

(define (open-changereq-iterator)
  (changereq-state-iterator 'open))

(define (research-changereq-iterator)
  (changereq-state-iterator 'research))

(define (verify-changereq-iterator)
  (changereq-state-iterator 'check-off))

(define (group-iterator)
  (list->iterator (value-sequence (group-table *application*))))

(define (filesystem-iterator)
  (list->iterator (value-sequence (file-system-table *application*))))

(define (snapshot-iterator)
  (list->iterator 
   (apply append
	  (map (lambda (fs)
		 (value-sequence (snapshot-table fs)))
	       (value-sequence (file-system-table *application*))))))


(define (fs-file-iteration fsname fs emit vmap)
  ;;
  (define (rec path node)
    (emit (make <file-record>
                filesystem: fsname
                type: (if (instance? node <directory-version>)
                          "d"
                          "f")
                version: (to-string (version-tag node))
                path: (or path "/")))
    ;;
    (if (instance? node <directory-version>)
        (for-each (lambda (sub)
                    (rec (string-append (or path "") "/" (car sub))
                         (vmap (cdr sub))))
                  (contents node))))
  ;;
  (rec #f (current-version (root-directory fs))))

(define (file-iterator)
  (proc->iterator
   (lambda (emit)
     (table-for-each
      (file-system-table *application*)
      (lambda (h k fs)
        (fs-file-iteration k fs emit
                           current-version))))))
  
(define (locked-files-iterator)
  (proc->iterator
   (lambda (item)
     (table-for-each
      (user-table *application*)
      (lambda (h k usr)
	(format #t "checking: ~s\n" usr)
	(for-each item (check-outs usr)))))))

(define (workitem-iterator ok?)
  (proc->iterator
   (lambda (emit)
     (table-for-each
      (change-request-table *application*)
      (lambda (h k cr)
	(for-each
	 (lambda (item)
	   (if (ok? item)
	       (emit item)))
	 (active-items cr))
	(for-each
	 (lambda (item)
	   (if (ok? item)
	       (emit item)))
	 (history cr)))))))
     
(define (active-fs-changes-iterator)
  (workitem-iterator
   (lambda (item)
     (and (instance? item <fs-change>)
	  (not (close-audit-entry item))))))

(define (integrations-iterator)
  (workitem-iterator
   (lambda (item)
     (and (instance? item <integration-request>)
	  (not (close-audit-entry item))))))

(define (snapshot-members-iterator)
  (workitem-iterator
   (lambda (item)
     (instance? item <integration-request>))))

(define $virtual-tables
  (list (list "user" user-iterator <user>)
        (list "file" file-iterator <file-record>)
	(list "changereq" changereq-iterator <change-request>)
	(list "filesystem" filesystem-iterator <file-system>)
	(list "snap" snapshot-iterator <snapshot>)
	(list "active-fs-changes" active-fs-changes-iterator <fs-change>)
	(list "group" group-iterator <group>)
	(list "fixing-changereqs" fixing-changereq-iterator <change-request>)
	(list "open-changereqs" open-changereq-iterator <change-request>)
	(list "locked-files" locked-files-iterator <checkout>)
	(list "check-off-changereqs" verify-changereq-iterator <change-request>)
	(list "integrations" integrations-iterator <integration-request>)
	(list "snapshot-members" snapshot-members-iterator <integration-request>)
	(list "researching-changereqs" research-changereq-iterator <change-request>)))

(define $value-parsers
  (list (list <user> string->user)
	(list <symbol> string->symbol)
	(list <string> identity)
	(list <file-system> string->filesystem)
	(list <fixnum> (lambda (str)
			 (let ((n (string->number str)))
			   (if (fixnum? n)
			       n
			       (service-error 720 "could not parse <fixnum> constant ~s" str)))))
	(list <boolean> (lambda (str)
			  (cond
			   ((member str '("true" "t" "#t"))
			    #t)
			   ((member str '("false" "f" "#f"))
			    #f)
			   (else
			    (service-error 719 "could not parse boolean constant ~s\n-- use `true' or `false'" str)))))
        (list <group> string->group)
	(list <change-request> string->changereq)))

;;; `other-accessors' are virtual slots that can be used
;;; in a `--where' clause.
;;;
;;; Somewhat similar to `user-format-gfs' in `--format'; maybe
;;; they should be unified?

(define $other-accessors
  (list 
   ;;
   ;; virtual slots for <integration-request>
   ;;
   (list <integration-request>
	 (list 'snapshot
	       <string>
	       (lambda (ir)
		 (if (pair? (snapshots ir))
		     (name (car (snapshots ir)))
		     "")))
	 (list 'in-snapshot?
	       <boolean>
	       (lambda (ir)
		 (not (null? (snapshots ir))))))
   ;;
   ;; virtual slots for <snapshot>
   ;;
   (list <snapshot>
	 (list 'state <symbol> snapshot-state))
   ;; end
   ))

(define (get-where-accessor class (slot-name <symbol>))
  (let ((slot (slot-descriptor-by-name class slot-name)))
    (if slot
	;; determine the range for the slot, so we know how
	;; to parse the comparison value
	(values (getter slot) (type-restriction slot))
	(let* ((alt-lst (assq class $other-accessors))
	       (alt (and alt-lst (assq slot-name (cdr alt-lst)))))
	  (if alt
	      (values (caddr alt) (cadr alt))
	      ;; check for a property reference
	      (if (property-slot? slot-name)
		  (let ((p (property-slot-property slot-name)))
		    (values
		     (lambda (obj)
		       (let ((v (get-property obj p (default-value p))))
			 (format #t "~s of ~s => ~s\n" p obj v)
			 v))
		     ;; instead of returning a type, return a value parser
		     (lambda (val)
		       (parse-property-value p val))))
		  (service-error 714 "`~s' is not a slot of ~s objects" 
				 slot-name
				 (name class))))))))

(define (property-slot? (name <symbol>))
  (and (> (string-length (symbol->string name)) 9)
       (string=? "property." (substring (symbol->string name) 0 9))))

(define (property-slot-property (name <symbol>))
  (let ((p (table-lookup (property-table *application*)
			 (substring (symbol->string name) 9))))
    (if p
	p
	(service-error 716 "No such property `~s'" name))))

(define (build-record-predicate clause class)
  (let ((p (handler-case
	    (parse-where-clause clause)
	    ((<condition> condition: c)
	     #f))))
    (if p
	(compile-predicate p class)
	(service-error 715 "Couldn't parse where predicte:\n--> ~j" clause))))

(define (compile-predicate expr class)
  (case (car expr)
    ;;
    ((=)
     (build-atomic-predicate/= (cadr expr) (caddr expr) class))
    ;;
    ((member)
     (build-atomic-predicate/member (cadr expr) (caddr expr) class))
    ;;
    ((<>)
     (let ((l (build-atomic-predicate/= (cadr expr) (caddr expr) class)))
       (lambda (rec)
         (not (l rec)))))
    ;;
    ((not)
     (let ((l (compile-predicate (cadr expr) class)))
       (lambda (rec)
	 (not (l rec)))))
    ;;
    ((and)
     (let ((l (compile-predicate (cadr expr) class))
	   (r (compile-predicate (caddr expr) class)))
     (lambda (rec)
       (and (l rec) (r rec)))))
    ;;
    ((or)
     (let ((l (compile-predicate (cadr expr) class))
	   (r (compile-predicate (caddr expr) class)))
     (lambda (rec)
       (or (l rec) (r rec)))))
    ;;
    (else
     (error "unknown expr type: ~s" expr))))

(define (get-value-parser tr slot-name class)
  (let ((value-parser (assq tr $value-parsers)))
    (if value-parser
	(cadr value-parser)
	(service-error 715 "accessor `~s' for ~s returns a ~s,\nfor which I don't know how to parse a value"
	 slot-name
	 (name class)
	 (name tr)))))

(define (get-where-info class slot-name)
  (bind ((get tr (get-where-accessor class slot-name)))
    (values get
	    (if (procedure? tr)
		tr
		(get-value-parser tr slot-name class)))))
	 
(define (build-atomic-predicate/member slot-name value-list class)
  (if (null? value-list)
      (lambda (record)
	#f)
      (bind ((get value-parser (get-where-info class slot-name))
	     (value-set (map value-parser value-list)))
	(cond
	 ((string? (car value-set))
	  (lambda (record)
	    (member (get record) value-set)))
	 (else
	  (lambda (record)
	    (memq (get record) value-set)))))))

(define (build-atomic-predicate/= slot-name value-str class)
  (bind ((get value-parser (get-where-info class slot-name))
	 (value (value-parser value-str)))
    (cond
     ((string? value)
      (lambda (record)
	(string=? (get record) value)))
     (else
      (lambda (record)
	(eq? (get record) value))))))

(define (handle-report-request args req inp out (u <user>))
  (let ((tbl (assoc (car args) $virtual-tables)))
    (if (not tbl)
	(service-error 713 "unrecognized table: ~a" (car args)))
    (let ((include? (let ((w (assq 'where req)))
		      (if w
			  (build-record-predicate (cdr w) (caddr tbl))
			  (lambda (record) #t))))
	  ;; instantiate the iterator
	  (iterator ((cadr tbl)))
	  (render (rendition-proc (caddr tbl) req render-report-line-item)))
      (let loop ((r '()))
	(let ((rec (iterator)))
	  (if rec
	      (loop (if (include? rec)
			(cons rec r)
			r))
	      (if (null? r)
		  (service-error 401 "No records selected")
		  (client-print-message
		   out
		   (with-output-to-string
		     (lambda ()
		       (for-each
			render
			(sort r (default-sort-order (car r))))))))))))))
