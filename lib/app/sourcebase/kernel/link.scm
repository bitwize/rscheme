
;; if node is #f, deletes the link
;; otherwise, adds the link

(define (edit-dir (fs <file-system>)
		  (path <fs-absolute-path>)
		  node
		  (fschanges <list>))
  (format #t "(edit-dir ~s ~s ~s\n" fs path node)
  (print fschanges)
  (format #t "\n")


  (let loop (((s <pair>) (steps path))
             ((d <directory>) (root-directory fs)))
    (phash (format #f "step[~a]" (car s)) d)
    (let ((step-name (car s))
	  (rest (cdr s)))
      (if (null? rest)
	  ;;
	  ;; This is the last step.
	  ;;
	  (let ((v (active-checkout d)))
	    (print v)
	    (if v
		(let* ((dirv (new-version v))
		       (old (fs-dir-lookup dirv step-name)))
		  (if node
		      (if old
			  (error "~a: path exists" path)
			  (dir-create-link v dirv step-name node))
		      (if old
			  (dir-remove-link v dirv step-name)
			  (error "~a: no such link ~a, in ~a" 
				 path
				 step-name
				 (name fs))))
		  ;; update the pathname table (node id => path map)
		  (if node
		      (pathname-table-link fs path node)
		      (pathname-table-unlink fs path old))
		  ;; make sure this <fs-change> is
		  ;; on the list of changes for the
		  ;; updated directory node
		  (for-each (lambda ((fsc <fs-change>))
			      (install-fs-change-link! dirv fsc))
			    fschanges))
		(error "~a: not checked out" path)))
	  ;;
	  ;; follow the next link in the chain
	  ;;
	  (let ((nxt (fs-dir-lookup (current-dir-version d) step-name)))
	     (if (instance? nxt <directory>)
	         (loop rest nxt)
		 (if nxt
		     (error "~a: ~a not a directory" path step-name)
		     (error "~a: ~a doesnt exist" path step-name))))))))
		     
			  
(define (dir-create-link (co <dir-checkout>) 
			 (v <directory-version>)
			 (name <string>) 
			 node)
    (set-contents! v (cons (cons name node) (contents v))))

(define (dir-remove-link (co <dir-checkout>) 
			 (v <directory-version>) 
			 (name <string>))
    (set-checked-out-shared-content!
	co
	(kunlink v name (checked-out-shared-content co))))

;; make sure the association between the <fs-change> and the <node-version>
;; is there (add it if not)
;;

(define (install-fs-change-link! (v <node-version>) (fsc <fs-change>))
   ;(format #t "installing fs change link: ~s <==> ~s\n" v fsc)
   (if (not (memq fsc (change-items v)))
       (begin
	(set-change-items! v (cons fsc (change-items v)))
	(kassert (not (memq v (new-versions fsc))))
	(set-new-versions! fsc (cons v (new-versions fsc))))))

;;
;;  used when a <node-version> is diverged
;;

(define (replace-fs-change-link! (old <node-version>)
				 (new <node-version>)
				 (fsc <fs-change>))
  (kassert (memq old (new-versions fsc)))
  (kassert (memq fsc (change-items old)))
  (set-change-items! old (delq! fsc (change-items old)))
  (set-change-items! new (cons fsc (change-items new)))
  (set-new-versions! 
   fsc 
   (cons new (delq! old (new-versions fsc)))))

;;
;; unlinks a node from a single directory
;; returns the new shared content ptr

(define (kunlink (dir <directory-version>) (name <string>) sh)
    (let private-loop ((p (contents dir))
                       (prev #f))
      (if (eq? p sh)
          ;;
	  ;; must eventually hit shared content, even if it's null
	  ;;
          (let shared-loop ((p p)
                            (prev prev))
	    ;; has to be in the list or else internal or synchronization error
            (kassert (not (null? p)))

                (if (string=? name (caar p))
                    ;; found it in a shared cell
                    (let ((rest (cdr p)))
                      (if prev
                          (set-cdr! prev rest)
                          (set-contents! dir rest))
                      rest)
                    ;; not found yet, but (unless its missing entirely)
                    ;; we need to copy this cell so it's cdr can be relinked
                    (let ((private-p (cons (car p) (cdr p))))
                      (if prev
                          (set-cdr! prev private-p)
                          (set-contents! dir private-p))
                      (shared-loop (cdr p) private-p))))
	  ;;
          ;; not in the shared content yet... must be at a real cell
	  ;;
          (if (string=? name (caar p))
              ;; found it in private content, can delete it directly
              (begin
                (if prev
                    (set-cdr! prev (cdr p))
                    (set-contents! dir (cdr p)))
                ;; no change in shared content
		sh)
              ;; haven't found it yet
              (private-loop (cdr p) p)))))
