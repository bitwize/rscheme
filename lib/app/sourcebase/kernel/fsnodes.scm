;;

(define (alloc-node-id)
   (let (((n <fixnum>) (num-nodes-alloced *application*)))
     (set-num-nodes-alloced! *application* (add1 n))
     n))

(define (fs-dir-lookup (d <directory-version>) name)
    (let ((a (assoc name (contents d))))
	(if a
	    (cdr a)
	    #f)))

(define (fs-dir-for-each (d <directory-version>) proc)
    (for-each (lambda ((p <pair>))
                (proc (car p) (cdr p)))
	      (contents d)))

(define-method the-root-version ((self <file-system>))
  (current-dir-version (root-directory self)))

(define-method the-root-version ((self <snapshot>))
  (table-lookup (node-version-map self)
		(id (root-directory (versioned-object self)))))

(define-method the-version-proc ((self <file-system>))
   current-dir-version)

(define-method the-version-proc ((self <snapshot>))
  (let ((tbl (node-version-map self)))
    (lambda ((n <node>))
    	(let ((v (table-lookup tbl (id n))))
	  (if v
	      v
	      (error "~d: id not in ~a#~a map"
	      	     (id n)
		     (name (versioned-object self))
		     (name self)))))))
		       
(define (fs-dir-for-each-version (fs <file-space>) 
				 (d <directory-version>) 
				 proc)
  (let ((the-version (the-version-proc fs)))
    (for-each (lambda ((p <pair>))
      		(let (((v <node-version>) (the-version (cdr p))))
		    ;(phash (car p) v)
		    (proc (car p) v)))
	      (contents d))))


(define (locked-by (nodev <node-version>))
   (let ((c (active-checkout (versioned-object nodev))))
     (if (and c (checked-out c))
         (user c)
	 #f)))

(define (find-node (self <file-system>) (path <fs-absolute-path>))
  (versioned-object (find-version self path)))

(define (find-version (self <file-space>) (path <fs-absolute-path>))
  (or (find-version* self path)
      (service-error 312 "~a: no such file or directory in ~a"
		     path
		     (name self))))

;;
;;   node sharing
;;
;;  node sharing is handled by having a (stable) property, 
;;  `filesystems', which is a list of all filesystems the
;;  file is reachable from, if more than one.
;;  (if a file is in only one FS, it doesn't have a filesystems property)
;;
;;  the problem is noticing when to _remove_ an FS entry
;;  from a <node>'s list, especially since a node can have
;;  multiple paths in a single FS, so unlinking it from an FS
;;  doesn't necessarily make it unreachable
;;

(define (get-shared-node 
...))

;;  node sharing II

