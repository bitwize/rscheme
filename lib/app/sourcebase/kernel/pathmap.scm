
(define (node->paths (fs <file-space>) (node <node>))
  (map (lambda (p)
	 (make <fs-absolute-path>
	       steps: (reverse p)))
       (or (table-lookup (pathname-table fs) (id node))
	   '())))

(define (pathname-table (self <file-space>))
  (or (node->path-cache self)
      (let ((tbl (make-table eq? integer->hash))
	    (proc (the-version-proc self)))
	(fill-table tbl proc '() (the-root-version self))
	(set-node->path-cache! self tbl)
	tbl)))

(define (fill-table tbl sel-version path v)
  (let ((my-id (id (versioned-object v))))
    (table-insert! tbl
		   my-id
		   (cons path (or (table-lookup tbl my-id) '())))
    (if (instance? v <directory-version>)
	(for-each (lambda ((p <pair>))
		    (fill-table tbl 
				sel-version
				(cons (car p) path) 
				(sel-version (cdr p))))
		  (contents v)))))


(define (pathname-table-unlink (self <file-system>) 
			       (path <fs-absolute-path>)
			       (node <node>))
  (if (instance? node <directory>)
      ;; punt
      (set-node->path-cache! self #f)
      ;; keep up to date
      (let ((tbl (node->path-cache self)))
	(if tbl
	    (let* ((ent (kassert (table-lookup tbl (id node))))
		   (m (kassert (member (reverse (steps path)) ent)))
		   (d (delq! (car m) ent)))
	      (if (null? d)
		  (table-remove! tbl (id node))
		  (table-insert! tbl (id node) d)))))))

(define (pathname-table-link (self <file-system>) path node)
  (if (instance? node <directory>)
      ;; punt
      (set-node->path-cache! self #f)
      ;; keep up to date
      (let ((tbl (node->path-cache self)))
	(if tbl
	    (table-insert! tbl 
			   (id node)
			   (cons (reverse (steps path))
				 (or (table-lookup tbl (id node)) '())))))))

