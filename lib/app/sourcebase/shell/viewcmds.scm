(define (ls . args)
   (let ((paths (if (null? args)
   		    (list $null-path)
		    (map string->fs-path args))))
    (for-each
      (lambda (path)
        (node-ls* (vsh-path->version path) path))
	paths)
    (values)))

(define (node-ls* (v <node-version>) path)
    (if (instance? v <directory-version>)
	(for-each
	 (lambda (pr)
	   (node-ls (cdr pr) 
		    (fs-append-path path (string->fs-path (car pr)))))
	 (sort (call-with-list-extending
		(lambda (add)
		  (fs-dir-for-each-version
		   (current-filespace *vsh-state*)
		   v
		   (lambda ((name <string>) (v <node-version>))
		     (add (cons name v))))))
	       (lambda (a b)
		 (string<? (car a) (car b)))))
	(node-ls v path)))

(define (node-ls (node <node-version>) (p <fs-path>))
  (format #t "~c~a ~10a ~10a ~-7d ~a ~a ~c~a\n"
	  (if (instance? node <directory-version>)
	      #\d
	      #\-)
	  (list->string (map (lambda (i)
			       (if (eq? (bitwise-and (permissions node) 
						     (logical-shift-right #o400 i)) 
					0)
				   #\-
				   (string-ref 
				    (if (instance? node <directory-version>)
					"lcs"
					"rwx")
				    (remainder i 3))))
			     (range 9)))
	  ;(link-count node)
	  (let ((locker (locked-by node)))
	    (if locker
		(name locker)
		"-"))
	  (name (group (versioned-object node)))
	  (if (instance? node <directory-version>)
	      (length (contents node))
	      (total-size (line-tree (contents node))))
	  (time->string (modification-time node) "%b %d %H:%M")
	  (fs-path->string p)
	  $version-delim
	  (if (version-tag node)
	      (version-tag->string (version-tag node))
	      $version-delim)))

(define (total-size item)
  (if (string? item)
      (add1 (string-length item))
      (if (vector? item)
	  (reduce + 0 (vector->list (vector-map total-size item)))
	  (error "invalid item type: ~s" item))))
       
(define (cat-cmd (file <string>))
  (let ((p (current-output-port))
  	(v (vsh-lookup-version file)))
    (if (instance? v <directory-version>)
        (error "~a: is a directory" file))
    (display (content->string (contents v)))
    (values)))

(define (stat-cmd (file <string>))
  (let (((v <node-version>) (vsh-lookup-version file)))
    (display "Base Object\n========\n")
    (print (versioned-object v))
    (format #t "Current Version (~a)\n========\n" 
    	    (version-tag->string (version-tag v)))
    (print v)
    (display "Versions\n========\n")
    (print (versions (versioned-object v)))
    v))
   
(define (itemqueue action . args)
  (case (string->symbol (substring action 1))
    ((r)
       (for-each print (active-items (user *vsh-state*))))
    (else
     (error "usage: iq -r"))))

(define-method display-history-item ((self <comment-request>))
   (format #t ">>> From ~a (~a)\n" 
	    (name (user (close-audit-entry self))) 
	    (timestamp (close-audit-entry self)))
   (display (comment self))
   (newline)
   (newline))
   
(define (list-change-req arg)
   (let ((cr (string->changereq arg)))
     (format #t "Change Request: ~d\n" (id cr))
     (format #t "Title:          ~a\n" (title cr))
     (format #t "Group:          ~a\n" (name (group cr)))
     (format #t "History:\n")
     (for-each display-history-item (reverse (history cr)))))
   
(define (df)
   ;;
   (define (snap-report (snap <snapshot>))
     (list (name (versioned-object snap))
           (name snap)
	   ""
	   ""
	   ""))
   ;;
   (define (fs-report (fs <file-system>))
     (list (name fs)
           ""
           (name (group fs))
	   (name (owner fs))
	   ""))
   ;;
   (let ((fs (current-filespace *vsh-state*)))
	(print-columns
	    '(("file system" "snapshot" "group" "owner" "last modified"))
	    (if (instance? fs <file-system>)
		(cons (fs-report fs)
		    (sort (map snap-report 
				(value-sequence 
				(snapshot-table fs)))
			    (lambda (a b)
			      (string<? (cadr a) (cadr b)))))
		(list (fs-report (versioned-object fs))
		    (snap-report fs))))))
