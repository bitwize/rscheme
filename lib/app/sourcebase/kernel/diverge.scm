(define (do-fs-divergence (fs <file-system>)
			  (fromfs <file-system>)
			  (on <node>)
			  (path <fs-absolute-path>)
			  (fsc-list <list>))
  (if (instance? on <directory>)
      (begin
	;; lock all directories...
	(if (not (active-checkout on))
	    (raw-node-lock fs path *user*))
	;;
	(for-each (lambda (sub)
		    (do-fs-divergence
		     fs
		     fromfs
		     (cdr sub)
		     (fs-append-path path (string->fs-path (car sub)))
		     fsc-list))
		  (append (contents (current-dir-version on)) '())))
      ;;
      (if (let ((v (stat-node fromfs path)))
	    (and v (eq? (versioned-object v) on)))
	  (let ((n (clone on)))
	    ;;
	    (set-active-checkout! on #f)
	    (if (active-checkout on)
		(format #t "divergent: ~a (locked only in new)" 
			(fs-path->string path))
		(format #t "divergent: ~a" (fs-path->string path)))
	    ;;
	    (let* ((old-v (current-version n))
		   (nv (clone old-v)))
	      ;;
	      (if (active-checkout on)
		  (set-checked-out! (active-checkout on) nv))
	      ;;
	      (let ((k '()))
		(for-each
		 (let ((node-id (id n)))
		   (lambda ((s <snapshot>))
		     (if (eq? (table-lookup (node-version-map s) node-id)
			      old-v)
			 (begin
			   (set! k (cons (name s) k))
			   (table-insert! (node-version-map s) node-id nv)))))
		 (value-sequence (snapshot-table fs)))
		(format #t " ~d snapshots: ~j\n" (length k) k))
	      ;;
	      (set-current-version! n nv)
	      (set-versioned-object! nv n)
	      (set-change-items! nv '()) ;; nil out pre call to replace...link!
	      ;;
	      (edit-dir fs path #f fsc-list)
	      (edit-dir fs path n fsc-list)
	      ;;
	      (for-each 
	       ;; for each such <fs-change> (see below), replace it's
	       ;; pointer in it's new-versions list with a pointer to
	       ;; the cloned version, remove the <fs-change> from the
	       ;; old-nodev's 'change-items' list and add it to the
	       ;; new-nodev's change-items list
	       (lambda ((fsc <fs-change>))
		 (replace-fs-change-link! old-v nv fsc))
			;; pick out those <fs-change>'s that association this
			;; node-version with the file system which is diverging
			;; (ie, the one getting the cloned <node-version>)
			(select (lambda (fsc)
				  (eq? (file-system fsc) fromfs))
				(change-items old-v))))))))
