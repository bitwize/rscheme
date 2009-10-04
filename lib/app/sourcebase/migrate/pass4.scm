;;
;;   Pass 4 (step 4XX)
;;
;;   releases, files, tracks, levels, levelmembers
;;

(define (migrate-releases)
  ;;  construct the <file-system>'s
  (for-each (lambda ((r Release))
	      (if (not (zero? (id r)))
		  (let ((fs (make-migrated-file-system r)))
		    ;; lock it initially so we can create dirs later on
		    (klock-node (root-directory fs) $root-path (owner fs) fs)
		    (table-insert! (file-system-table *application*) 
				   (name fs) 
				   fs)
		    (set-forward! r fs))))
	    (cmvc-table 'Releases))
  #t)

(define (make-migrated-file-system (r Release))
  (let ((fs (make <file-system>
		  %alloc-area: (make-area)
		  name: (name r)
		  snapshot-table: (make-table string=? string->hash)
		  root-directory: #f
		  owner: (forward (cmvc-user (user-id r)))
		  group: (forward (cmvc-component (component-id r)))
		  properties: (make-policy-property 
			       r
			       track-subprocess?   'require-reasons
			       approve-subprocess? 'get-fs-approval
			       fix-subprocess?     'notify-group
			       level-subprocess?   'want-snapshot
			       test-subprocess?    'test-changes)
		  audit-log: '())))
    (set-root-directory! fs (make-dir (group fs)))
    fs))

;;
;;  when files are linked in CMVC, they share a common source id
;;  I think their evolution even thereafter is still linked, which
;;  is how you can get branching SCCS id's
;;

;;  this function creates a table mapping source-id's to 
;;  our <file> objects.  These aren't installed into a <filesystem>
;;  until `migrate-files'

(define (migrate-source-files)
  (let ((tbl (or (vector-ref *migration-state* 15)
		 (make-table eq? integer->hash))))
    (vector-set! *migration-state* 15 tbl)
    (let loop ((flst (cmvc-table 'Files))
	       (work 0))
      (if (null? flst)
	  ;; return an indication that we're done with this step
	  #t
	  (if (> work 100)
	      ;; return indicator that we're not done yet but we want
	      ;; to take a break
	      #f
	      (let ((f (car flst)))
		(if (table-lookup tbl (source-id f))
		    ;; already did this source file
		    (loop (cdr flst) work)
		    ;; a new one
		    (let ((srcid (source-id f)))
		      (table-insert! tbl
				     srcid
				     (migrate-one-source-file 
				      srcid
				      (select (lambda ((v Version))
						(eq? (source-id v) srcid))
					      (cmvc-table 'Versions))
				      (type f)))
		      (loop (cdr flst) (+ work 1))))))))))

(define (load-vxf src-id type)
  (let* ((p (vxf-path src-id type))
	 (i (image->object (file->string p))))
    (format #t "vxf: ~a => ~d SID's\n" p (vector-length i))
    (if (string=? type "long")
	;; this is really a normal text file, it just so happens
	;; that SCCS (and hence CMVC) was too stupid to handle it
	;; properly
	(content-list->shared (vector->list i))
	;; this is a normal text file.
	(vector->list i))))

(define (uniquify-lines lst)
  (let ((tbl (make-table string=? string->hash)))
    (map (lambda (str)
	   (or (table-lookup tbl str)
	       (begin
		 (table-insert! tbl str str)
		 str)))
	 lst)))

(define (content-list->shared contents)
  (let loop ((prev #f)
	     (src contents)
	     (lst '()))
    (if (null? src)
	(reverse lst)
	(let* ((sid (caar src))
	       (content (string-split (cdar src) #\newline))
	       (vect (if prev
			   (make-new-vec-tree prev content)
			   (make-vec-tree! (uniquify-lines content)))))
	  (loop vect 
		(cdr src)
		(cons (cons sid vect) lst))))))

(define (vxf-path src-id type)
  (string-append
   (vector-ref *migration-state* 14)
   (format #f "/~d/~d/~d/~d/~c.~02d.vxf"
	   (quotient src-id 100000)
	   (remainder (quotient src-id 10000) 10)
	   (remainder (quotient src-id 1000) 10)
	   (remainder (quotient src-id 100) 10)
	   (if (string=? type "text")
	       #\s
	       #\b)
	   (remainder src-id 100))))

;;

;;
;;  migrating a file consists of creating a <fs-node> to represent
;;  the file, with a version-map containing all the versions
;;  of the file
;;
;;  furthermore, each Change record will create or add the corresponding
;;  version to a <fs-change> object
;;

(define (versions->content-map alloc-area versions)
  (map (lambda (v)
	 (let ((c (cdr v)))
	   (cons (car v)
		 (if (string? c)
		     (let ((cpy (bvec-alloc-in-area alloc-area
						    <byte-vector>
						    (string-length c)
						    0)))
		       (bvec-copy cpy 0
				  c 0
				  (string-length c))
		       (make <binary-file-content>
			     %alloc-area: alloc-area
			     data: cpy))
		     ;; don't explicitly copy the contents; let normal
		     ;; commit mechanism preserve vectree object identity
		     (make <text-file-content>
			   %alloc-area: alloc-area
			   line-tree: c)))))
       versions))

;;
;;  ***NOTE***
;;  this only works for single-line "1.xx" source derivation
;;  migrating more general branching structures is a future
;;  enhancement and not necessary to get rsfam ported
;;  (it also doesn't handle skips...)
;;  (it also doesn't handle not having all SCCS versions in CMVC)

(define (migrate-one-source-file src-id v-list type)
  (let* ((a (make-area))
	 ;; compute the ordered list of versions... expensive but easy
	 (v-list (sort v-list 
		       (lambda (a b)
			 (< (string->number (substring (s-id a) 2))
			    (string->number (substring (s-id b) 2))))))
	 ;; get the list of versions in the source (content) file
	 (c-list (versions->content-map a (load-vxf src-id type)))
	 (vmap #f))
    (format #t "src-id ~d versions: ~j\n" src-id (map s-id v-list))
     (let loop ((s v-list)
		(prev-leaf #f)
		(vs '()))
       (if (null? s)
	   (let ((f (make <file>
			  %alloc-area: a
			  id: (alloc-node-id)
			  current-version: (value prev-leaf)
			  versions: vmap
			  group: (world-group *application*)
			  stable-properties: '())))
	     (for-each (lambda ((fv <file-version>))
			 (set-versioned-object! fv f))
		       vs)
	     f)
	   (let* (((v Version) (car s))
		  ((c <file-content>) (cdr (assoc (s-id v) c-list)))
		  (fv (make <file-version>
			    %alloc-area: a
			    version-tag: #f
			    versioned-object: #f
			    previous-version: (and prev-leaf (value prev-leaf))
			    modification-time: (cmvc->time (change-date v))
			    comment: (remarks v)
			    contents: c))
		  (nl (if prev-leaf
			  (new-leaf prev-leaf fv)
			  (bind ((m l (make-version-map fv)))
			    (set! vmap m)
			    l))))
	     (set-forward! v fv)
	     (set-version-tag! fv nl)
	     (if (not (string=? (version-tag->string nl) (s-id v)))
		 (error "version ID mismatch ~s != ~s" 
			(version-tag->string nl)
			(s-id v)))
	     (loop (cdr s) nl (cons fv vs)))))))

;;  this step actually installs the files discovered by `migrate-source-files'
;;  into their respective releases under the correct path names
;;
;;  the `new-*' fields in a File apparently indicate uncommitted changes
;;  hence, (??) they are ignored during this step in processing, which
;;  only picks up committed files
;;


(define (migrate-one-file (f File))
  (let* (((fpath <string>) (name (cmvc-path (path-id f))))
	 ((fs <file-system>) (forward (cmvc-release (release-id f))))
	 ((nf <file>) (migrated-source-file (source-id f)))
	 (fpath (string->fs-path (string-append "/" fpath)))
	 (perm (string->number (file-mode f) 8)))
    (format #t "~s: ~a => ~s (~o)\n" fs fpath nf perm)
    (set-group! nf (forward (cmvc-component (component-id f))))
    (for-each-version (versions nf)
		      (lambda (label tag)
			(set-permissions! (value tag) perm)))
    ;; only link the file into the current release if it hasn't
    ;; been dropped
    (if (not (drop-date f))
	(begin
	  (migration-mkdirs fs (steps fpath))
	  (edit-dir fs fpath nf '())))
    nf))

(define (migration-mkdirs (fs <file-system>) steps)
  (let loop ((so-far '())
	     (target steps))
    (if (pair? target)
	(let ((dpath (make <fs-absolute-path>
			   steps: (reverse so-far))))
	  ;(format #t "checking: ~s\n" dpath)
	  (if (not (find-version* fs dpath))
	      (let ((d (make-dir (group fs))))
		(edit-dir fs dpath d '())
		(klock-node d dpath (owner fs) fs)
		(vector-set! *migration-state*
			     16
			     (cons (active-checkout d)
				   (vector-ref *migration-state* 16)))))
	  (loop (cons (car target) so-far)
		(cdr target))))))

(define (migrate-files)
  (let loop ((f (cmvc-table 'Files))
	     (work 0))
    (if (null? f)
	#t ;; done
	(if (> work 100)
	    #f ;; take a break
	    (if (forward (car f))
		(loop (cdr f) work) ;;  do nothing
		(begin
		  ;; do something
		  (set-forward! (car f) (migrate-one-file (car f)))
		  (loop (cdr f) (+ work 1))))))))

;;
;;  migrate CMVC Tracks.  These will become <fs-change>'s
;;  attached to <change-request>'s
;;

(define (migrate-tracks)
  (for-each migrate-one-track (cmvc-table 'Tracks))
  #t)

(define (migrate-one-track (t Track))
  (let (((fs <file-system>) (forward (cmvc-release (release-id t))))
	((cr <change-request>) (forward (cmvc-defect (defect-id t))))
	((u <user>) (forward (cmvc-user (user-id t)))))
    (let ((item (make <fs-change>
		      owner: u
		      base-request: cr
		      file-system: fs)))
      (set-forward! t item)
      (set-activate-audit-entry! 
       item
       (make <audit-log-entry>
	     user: u
	     operation: 'open
	     arg-list: (list item)
	     info: item
	     result: item
	     timestamp: (cmvc->time (add-date t))))
      (if (actual t)
	  (begin
	    (set-history! cr (cons item (history cr)))
	    (set-close-audit-entry!
	     item
	     (make <audit-log-entry>
		   user: u
		   operation: 'close
		   arg-list: (list item)
		   info: item
		   result: #t
		   timestamp: (cmvc->time (last-update t)))))
	  (begin
	    (set-active-items! cr (cons item (active-items cr)))
	    (set-active-items! u (cons item (active-items u))))))))

;;
;;  migrate CMVC Levels into <snapshot> objects
;;
;; this is a little tricky because we need to be able to find directory
;; versions that correspond to consistent snapshots of source files
;;
;;  the basic algorithm is as follows:
;;   (1) for each Level, create a <snapshot> containing the appropriate
;;       version map for the file.  this may involve creating
;;       new directories to link to renamed files (they will be
;;       in the current version under their most recent name)

(define (migrate-one-release-level (r Release) (fs <file-system>) (l Level))
  (let* ((t (table-lookup (property-table *application*) "leveltype"))
	 (s (make <snapshot>
		 %alloc-area: (area-of fs)
		 name: (name l)
		 node-version-map: (make-table eq? integer->hash)
		 properties: (list (cons 'state (string->symbol (state l)))
				   (cons t (parse-property-value t (type l))))
		 versioned-object: fs))
	 (lmap (build-or-get-level-map r l)))
    ;; initialize the node version map from the level map
    (migrate-version-map (root-directory fs)
			 (cdr lmap)
			 (node-version-map s)
			 (cmvc->time (or (commit-date l)
					 (last-update l)
					 (add-date l))))
    ;;
    s))


(define (build-or-get-level-map (r Release) (l Level))
  (if (commit-date l)
      (parse-level-map (string-append
			(vector-ref *migration-state* 17)
			"/"
			(name r)
			"/"
			(name l)))
      (error "migrating uncommitted levels not implemented: ~a ~a"
	     (name r)
	     (name l))))
	

(define (migrate-version-map (dir <directory>)
			     level-contents
			     nv-map
			     t)
  (let ((n (make <directory-version>
		 versioned-object: dir
		 version-tag: #f
		 previous-version: (current-version dir)
		 modification-time: t
		 contents: (map (lambda ((p <pair>))
				  (cons 
				   (car p)
				   (versioned-object
				    (if (level-map-dir? p)
					(migrate-dir-version dir p nv-map t)
					(migrate-file-version dir p nv-map t)))))
				level-contents)
		 permissions: #o755)))
    (set-version-tag! n (new-leaf (version-tag (current-version dir)) n))
    (set-current-version! dir n)
    (table-insert! nv-map (id dir) n)
    n))

(define (migrate-file-version (parent <directory>)
			      level-map
			      nv-map
			      t)
  (let ((v (forward (cmvc-version (cadr level-map)))))
    (table-insert! nv-map (id (versioned-object v)) v)
    v))

(define (migrate-dir-version (parent <directory>)
			     level-map
			     nv-map
			     t)
  (let ((dir (assoc (car level-map) (contents (current-version parent)))))
    (migrate-version-map 
     (if dir
	 (cdr dir)
	 (make-dir (group parent)))
     (cdr level-map) 
     nv-map
     t)))

(define (migrate-release-levels (r Release) level-list)
  (let ((fs (forward r)))
    (for-each (lambda ((l Level))
		(if (commit-date l)
		    (begin
		      (migration-progress "  migrating release ~a level ~a\n"
					  (name r)
					  (name l))
		      (let ((s (migrate-one-release-level r fs l)))
			(table-insert! (snapshot-table fs)
				       (name s)
				       s)
			(set-forward! l s)))
		    (migration-progress "  skipping uncommitted level ~a\n" (name l))))
	      level-list)))

(define (migrate-levels)
  (for-each (lambda ((r Release))
	      (let ((lst (select (lambda ((l Level))
				   (eq? (release-id l) (id r)))
				 (cmvc-table 'Levels))))
		(if (pair? lst)
		    (migrate-release-levels 
		     r 
		     (sort lst
			   (lambda (a b)
			     (if (and (commit-date a)
				      (commit-date b))
				 (time<? (cmvc->time (commit-date a))
					 (cmvc->time (commit-date b)))
				 (if (commit-date a)
				     #t
				     #f))))))))
	    (cmvc-table 'Releases))
  #t)

;;
;;  migrate CMVC Change objects.  these will become entries
;;  in <fs-change>'s
;;

(define (migrate-changes)
  (for-each migrate-one-change (cmvc-table 'Changes))
  #t)

(define (migrate-one-change (c Change))
  (let (((fsc <fs-change>) (forward (cmvc-track (track-id c))))
	((path <string>) (name (cmvc-path (path-id c))))
	((file <file>) (forward (cmvc-file (file-id c))))
	((v <file-version>) (forward (cmvc-version (version-id c))))
	(level (and (level-id c)
		    (forward (cmvc-level (level-id c)))))
	(type (string->symbol (type c)))
	((u <user>) (forward (cmvc-user (user-id c)))))
    (set-new-versions!
     fsc
     (cons v (new-versions fsc)))))


;;

(define (migrate-files-out)
  (for-each migrate-one-file-out (cmvc-table 'FilesOut))
  #t)

(define (migrate-one-file-out (fo FileOut))
  (let (((f <file>) (forward (cmvc-file (file-id fo))))
	((fs <file-system>) (forward (cmvc-release 
				      (release-id 
				       (cmvc-file (file-id fo))))))
	((v <file-version>) (forward (cmvc-version (version-id fo))))
	((u <user>) (forward (cmvc-user (user-id fo))))
	(t (cmvc->time (checkout-date fo))))
    (assert (not (active-checkout f)))
    (let ((co (make <checkout>
		    %alloc-area: (area-of f)
		    user: u
		    file-system: fs
		    checked-out: v
		    checkout-time: t)))
      (set-active-checkout! f co)
      (set-check-outs! u (cons co (check-outs u))))))

(define (migrate-fix-records)
  (for-each 
   (lambda ((f Fix))
     (let (((fsc <fs-change>) (forward (cmvc-track (track-id f))))
	   ((u <user>) (forward (cmvc-user (user-id f))))
	   ((g <group>) (forward (cmvc-component (component-id f))))
	   (addt (and (add-date f) (cmvc->time (add-date f))))
	   (updt (and (last-update f) (cmvc->time (last-update f))))
	   (s (string->symbol (state f))))
       (let* ((cr (base-request fsc))
	      (i (make <code-review>
		       %alloc-area: (area-of cr)
		       owner: u
		       base-request: (base-request fsc)
		       group: g)))
	 (assert addt)
	 (set-activate-audit-entry!
	  i
	  (make <audit-log-entry>
		%alloc-area: (area-of cr)
		user: u
		operation: 'open
		arg-list: (list i)
		info: i
		result: i
		timestamp: addt))
	 (case s
	   ((complete)
	    (assert updt)
	    (set-close-audit-entry!
	     i
	     (make <audit-log-entry>
		   %alloc-area: (area-of cr)
		   user: u
		   operation: 'close
		   arg-list: (list i)
		   info: i
		   result: i
		   timestamp: addt))
	    (set-history! cr (cons i (history cr))))
	   ((active)
	    (set-active-items! cr (cons i (active-items cr))))
	   (else 
	    (error "Fix record state `~a' not understood" s))))))
   (cmvc-table 'Fix)))

	  