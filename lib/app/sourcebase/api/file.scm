(define (validate-reasons (path <fs-absolute-path>)
			  (fs <file-system>) 
			  (group <group>) 
			  (reasons <list>))
   (if (and (null? reasons)
            (policy-require-reasons? group fs))
       (error "~a: required reasons missing" path)
       (for-each (lambda ((r <change-request>))
		   (if (not (eq? (state r) 'fixing))
		       (error "~a: not in `fixing' state" (id r))))
		 reasons)))

;; the created object is not locked

(define-api (make-file (fs <file-system>)
		       (path <fs-absolute-path>)
		       (owner <user>)
		       (group <group>)
		       (initial-content <file-content>)
		       (reasons <list>)
		       comment
		       mtime)
   (validate-reasons path fs group reasons)
   (bind ((area (make-area fs))
          (fv (make <file-version>
		       contents: initial-content
		       %alloc-area: area
		       previous-version: #f
		       versioned-object: #f
		       version-tag: #f
                       change-items: (if *audit-entry*
                                         (list *audit-entry*)
                                         '())
		       comment: (or comment "Initial version")
		       modification-time: (or mtime *timestamp*)))
	  (vmap v1 (make-version-map fv))
	  (fn (make <file>
		    versions: vmap
		    %alloc-area: area
		    group: group
		    id: (alloc-node-id)
		    current-version: fv))
	  (fscs (reasons->fs-changes owner fs reasons)))
	(set-version-tag! fv v1)
	(set-versioned-object! fv fn)
	(edit-dir fs path fn fscs)
	(for-each (lambda ((fsc <fs-change>))
		    (install-fs-change-link! fv fsc))
		  fscs)
	fn))

;;

(define-api (node-unlock (fs <file-system>)
			 (path <fs-absolute-path>)
			 (unlocker <user>))
   (let* (((node <node>) (find-node fs path))
	  (co (active-checkout node)))
     (if (not co)
         (error "~a: not locked" path))
     (if (not (eq? (user co) unlocker))
         (error "~a: can't unlock; locked by ~a" path (user co)))
     (set-active-checkout! node #f)
     (set-check-outs! (user co) (delq! co (check-outs (user co))))
     (current-version node)))

(define-api (node-lock (fs <file-system>)
		       (path <fs-absolute-path>)
		       (locker <user>))
   (let (((node <node>) (find-node fs path)))
     (if (active-checkout node)
         (error "~a: already locked by ~a" path (user (active-checkout node))))
     (klock-node node path locker fs)
     (current-version node)))
     

;;

(define-api (file-delta (fs <file-system>)
			(path <fs-absolute-path>)
			(owner <user>)
			(new-content <file-content>)
			(reasons <list>) ;; list of <change-req>'s
			comment
			mtime		; #f or a <time>
			(other-fss <list>)
			diverge?)
 (let* (((node <node>) (find-node fs path))
	(co (active-checkout node)))
   ;;
   (let ((also (assq 'fs-list (stable-properties node))))
     (if also
	 ;; this node is shared... must either specify the list
	 ;; of shared filesystems, or `diverge?'
	 (if (not
	      (or diverge?
		  (equal? (sort (map name (cdr also)) string<?)
			  (sort (map name other-fss) string<?))))
	     (error "node at ~a is shared" path))
	 (if (or diverge? (pair? other-fss))
	     (error "node at ~a is not shared" path))))
   ;;
   (validate-reasons path fs (group node) reasons)
   (for-each (lambda (fs)
	       (validate-reasons path fs (group node) reasons))
	     other-fss)
   ;;
   (if (not (instance? node <file>))
       (error "~a: is a ~a, not a file)" 
	      path 
	      (class-name (object-class node))))
   (if (not co)
       (error "~a: not checked out" path))
   (if (not (eq? (user co) owner))
       (error "~a: can't checkin; locked by ~a" path (user co)))
   ;;
   (set-active-checkout! node #f)
   ;;
   (if diverge?
       (set! node (raw-diverge-node (list (cons fs path)) reasons)))
   ;;
   (set-check-outs! (user co) (delq! co (check-outs (user co))))
   (let* ((v (make <file-version>
		   contents: new-content
		   versioned-object: node
		   previous-version: (current-version node)
                   permissions: (permissions (current-version node))
                   version-tag: #f
                   comment: comment
                   change-items: (if *audit-entry*
                                     (list *audit-entry*)
                                     '())
                   modification-time: (or mtime *timestamp*)))
	     (vtag (new-leaf (version-tag (previous-version v)) v)))
       (set-version-tag! v vtag)
       (set-current-version! node v)
       (for-each 
	(lambda (fs)
	  (for-each (lambda ((fsc <fs-change>))
		      (install-fs-change-link! v fsc))
		    (reasons->fs-changes owner fs reasons)))
	(cons fs other-fss))
       v)))

;;

(define (node-property-info prop)
  (case prop
    ((group) (values 0 group set-group! (lambda (x) (instance? x <group>))))
    ((mode) (values 1 permissions set-permissions! integer?))
    (else
     (error "~s: unrecognized node property" prop))))

#|
(define-api (node-modify (fs <file-system>)
			 (path <fs-absolute-path>)
			 (reasons <list>) ;; list of <change-req>'s
			 comment
			 new-props)
  (let (((node <node>) (find-node fs path)))
    (validate-reasons path fs (group node) reasons)
    (if co
	(error "~a: can't modify; locked by ~a" path (user co)))
    ;;
    (let ((nv #f))
      (let loop ((plist new-props))
	(if (null? plist)
	    (begin
	      (if nv
		  (for-each (lambda ((fsc <fs-change>))
			      (install-fs-change-link! v fsc))
			    (reasons->fs-changes owner fs reasons)))
	      node)
	    (bind ((type getter setter predicate (node-property-info 
						  (caar plist))))
	      (if (not (predicate (cdar plist)))
		  (error "~a: property ~a value ~s is invalid"
			 path
			 (caar plist)
			 (cdar plist)))
	      (case type
		((0) (setter node (cdar plist)))
		((1) (if (not nv)
			 (begin
			   (set! nv (clone (current-version node)))
			   (set-previous-version! nv (current-version node))
			   (set-comment! nv comment)
			   (set-modification-time! nv *timestamp*)
			   (set-version-tag! nv 
					     (new-leaf
					      (current-version node)
					      nv))
			   (set-current-version! node nv)))
		     (setter nv (cdar plist))))
	      (loop (cdr plist))))))))
|#
