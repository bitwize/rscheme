(define-api (link-node (fs <file-system>)
		       (from-path <fs-absolute-path>)
		       (to-path <fs-absolute-path>)
		       (reasons <list>))
  (let* ((n (find-node fs from-path))
	 (fsc-list (reasons->fs-changes *user* fs reasons)))
    (edit-dir fs to-path n fsc-list)
    n))

;;
;; `diverge-node' works by replacing a set of previously eq?
;; nodes with a clone of the node.  Since it's a clone, it will
;; have its own current-version, but will share the version-map
;; with the sibling nodes.
;;
;; in order to work with snapshots properly, it has to have the
;; same node id, which IS BROKEN because then you can't link in
;; the other logically distinct node into the FS (they would *collide*
;; in the snapshot table then)
;; 
;; in the interest of expediency (the need to link most of 0.7 to 0.6),
;; this problem is being ignored
;;

(define-api (diverge-node (fs-path-pairs <list>)
			  (reasons <list>))
    (if (null? fs-path-pairs)
	(error "diverge-node: empty list"))
    ;;
    (error "`diverge-node' is broken; see diverge-fs for proper
utilization of replace-fs-change-link")
    ;;
    (let ((ns (map
	       (lambda (fspp)
		 (let (((fs <file-system>) (car fspp))
		       ((p <fs-absolute-path>) (cdr fspp)))
		   (let ((n (find-node fs p)))
		     (validate-reasons p fs (group n) reasons)
		     n)))
	       fs-path-pairs))
	  (fsc-lists (map
		      (lambda (fspp)
			(reasons->fs-changes *user* (car fspp) reasons))
		      fs-path-pairs)))
      ;;
      (if (not (every? (lambda (n)
			 (eq? n (car ns)))
		       (cdr ns)))
	  (error "diverge-node: not all the same node already"))
      ;;
      (let ((n (clone (car ns))))
	;;
	(if (active-checkout n)
	    (error "diverge-node: currently checked out"))
	;;
	(let* ((old-v (current-version n))
	       (nv (clone old-v)))
	  (set-current-version! n nv)
	  (set-versioned-object! nv n)
	  (set-change-items! nv '()) ;; nil out pre call to replace...link!
	  ;;
	  (for-each 
	   (lambda (fspp fsc-list)
	     (let (((fs <file-system>) (car fspp))
		   ((p <fs-absolute-path>) (cdr fspp)))
	       (edit-dir fs p #f fsc-list)
	       (edit-dir fs p n fsc-list)
	       (for-each (lambda ((fsc <fs-change>))
			   (replace-fs-change-link! old-v nv fsc))
			 fsc-list)))
	   fs-path-pairs
	   fsc-lists)
	  n))))

;;
;;  diverge an entire FS
;;

(define-api (diverge-fs (newfs <file-system>)
			(oldfs <file-system>)
			(reasons <list>))
  ;;
  (validate-reasons $root-path newfs (group newfs) reasons)
  ;;
  (do-fs-divergence newfs
		    oldfs
		    (root-directory newfs) 
		    $root-path 
		    (reasons->fs-changes *user* newfs reasons)))

;;

  
(define-api (cross-link-node (from-fs <file-system>)
			     (from-path <fs-absolute-path>)
			     (to-fs <file-system>)
			     (to-path <fs-absolute-path>)
			     (reasons <list>))
  (let ((n (find-node from-fs from-path))
	(to-fsc-list (reasons->fs-changes *user* to-fs reasons)))
    ;; make sure that, if reasons are required, that
    ;; all of the supplied reasons are "active"
    (validate-reasons to-path to-fs (group n) reasons)
    ;; make the pathname `to-path' (in `to-fs') refer to
    ;; the given object (`n'), which we found by looking
    ;; for `from-path' in `from-fs'
    (edit-dir to-fs to-path n to-fsc-list)
    ;; do more stuff...
    (let ((fv (complete-version-chain (current-version n))))
      (for-each (lambda ((fsc <fs-change>))
		  (for-each (lambda (v)
			      (install-fs-change-link! v fsc))
			    fv))
		to-fsc-list)
      n)))

(define-api (cross-link-node-rec (from-fs <file-system>)
				 (from-path <fs-absolute-path>)
				 (to-fs <file-system>)
				 (to-path <fs-absolute-path>)
				 (reasons <list>)
				 (comment <string>))
  (let ((n (find-node from-fs from-path))
	(to-fsc-list (reasons->fs-changes *user* to-fs reasons)))
    (validate-reasons from-path from-fs (group n) reasons)
    (let loop ((n n)
	       (dst to-path)
	       (src from-path))
      (format #t "linking: ~s => ~s (~s)\n" src dst n)
      (if (instance? n <directory>)
	  (begin
	    (raw-make-directory to-fs dst *user* (group n) reasons)
	    (raw-node-lock to-fs dst *user*)
	    (for-each (lambda (sub)
			(let ((step (string->fs-path (car sub))))
			  (loop (cdr sub)
				(fs-append-path dst step)
				(fs-append-path src step))
			  (values)))
		      (contents (current-dir-version n)))
	    (dir-node-delta to-fs dst *user* reasons comment 
			    (find-node to-fs dst)))
	  ;;
	  ;; arrange to include *ALL* versions of this file from the
	  ;; current version to the original (first) version
	  ;; into the <fs-change>.  This is so the snapshot extension
	  ;; facility can work
	  ;;
	  (let ((fv (complete-version-chain (current-version n))))
	    (edit-dir to-fs dst n to-fsc-list)
	    (for-each (lambda ((fsc <fs-change>))
			(for-each (lambda (v)
				    (install-fs-change-link! v fsc))
				  fv))
		      to-fsc-list))))
    n))

(define (complete-version-chain (v <node-version>))
  (if (previous-version v)
      (cons v (complete-version-chain (previous-version v)))
      (cons v '())))

(define-api (unlink-file (fs <file-system>)
			 (from-path <fs-absolute-path>)
			 (reasons <list>))
  (let ((n (find-node fs from-path))
	(fsc-list (reasons->fs-changes *user* fs reasons)))
    ;;
    (if (instance? n <directory>)
        (error "~a: is a directory" from-path))
    (if (active-checkout n)
        (error "~a: currently locked by ~s" 
               from-path 
               (user (active-checkout n))))
    ;;
    (edit-dir fs from-path #f fsc-list)
    n))

(define-api (unlink-dir (fs <file-system>)
                        (from-path <fs-absolute-path>)
                        (reasons <list>))
  (let ((n (find-node fs from-path))
	(fsc-list (reasons->fs-changes *user* fs reasons)))
    ;;
    (define (checkout-scan n)
      (if (active-checkout n)
          (error "~a: some nodes currently locked" from-path)
          (if (instance? n <directory>)
              (for-each checkout-scan
                        (map cdr (contents (current-version n)))))))
    ;;
    (if (not (instance? n <directory>))
        (error "~a: not a directory" from-path))
    (checkout-scan n)
    ;;
    (edit-dir fs from-path #f fsc-list)
    n))

(define-api (rename-node (fs <file-system>)
			 (from-path <fs-absolute-path>)
			 (to-path <fs-absolute-path>)
			 (reasons <list>))
  (let ((n (find-node fs from-path))
	(fsc-list (reasons->fs-changes *user* fs reasons)))
     (edit-dir fs to-path n fsc-list)
     (edit-dir fs from-path #f fsc-list)
     n))

(define-api (make-directory (fs <file-system>)
		            (path <fs-absolute-path>)
			    (owner <user>)
			    (group <group>)
			    (reasons <list>))
   (validate-reasons path fs group reasons)
   (let ((d (make-dir group))
	 (fsc-list (reasons->fs-changes owner fs reasons)))
     ;;
     (for-each (lambda ((fsc <fs-change>))
		 (install-fs-change-link! (current-version d) fsc))
	       fsc-list)
     ;;
     (edit-dir fs path d fsc-list)
     d))

(define (make-dir group)
   (phash "make-dir.group" group)
   (bind ((area (make-area))
   	  (v (make <directory-version>
			  %alloc-area: area
	     		  versioned-object: #f
			  version-tag: #f
			  modification-time: *timestamp*
			  previous-version: #f
			  permissions: #o755
			  contents: '()))
	    (vmap firstv (make-version-map v))
	    (d (make <directory>
		    %alloc-area: area
		    group: group
		    current-version: v
		    active-checkout: #f
		    id: (alloc-node-id)
		    versions: vmap)))
	(set-version-tag! v firstv)
	(set-versioned-object! v d)
	d))

(define-api (directory-delta (fs <file-system>)
			     (path <fs-absolute-path>)
			     (owner <user>)
			     (reasons <list>)
			     comment)
  (let ((n (find-node fs path)))
    (validate-reasons path fs (group n) reasons)
    (dir-node-delta fs path owner reasons comment n)))

(define (dir-node-delta (fs <file-system>)
			(path <fs-absolute-path>)
			(owner <user>)
			(reasons <list>)
			comment
			(node <node>))
  (if (not (instance? node <directory>))
      (error "~a: not a directory" path))
  (if (not (eq? (user (active-checkout node)) owner))
      (error "~a: can't checkin; locked by ~a" 
	     path (user (active-checkout node))))
  (let* (((ac <dir-checkout>) (active-checkout node))
	 ((v <directory-version>) (new-version ac)))
    (set-comment! v comment)
    (set-version-tag! v (new-leaf (version-tag (previous-version v)) v))
    (set-modification-time! v *timestamp*)
    (kassert (eq? (versioned-object v) node))
    (set-current-version! node v)
    (set-active-checkout! node #f)
    (set-check-outs! (user ac) (delq! ac (check-outs (user ac))))
    (for-each (lambda ((fsc <fs-change>))
		(install-fs-change-link! v fsc))
	      (reasons->fs-changes owner fs reasons))
    v))

