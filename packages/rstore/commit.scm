
;;
;; commit the current state of the persistent store
;; (may not create a consistent snapshot of the store
;; if other threads are writing while one thread is trying
;; to commit)

;;
;; if called with just one argument, doesn't change the root object
;;

(define (get-tip-size (self <persistent-store>))
  (let ((l (underlying-lss self)))
    (lss-get-vol-size l (lss-get-tip l))))

(define (did-commit (self <persistent-store>) commit-id start-size)
  (let ((hook (compaction-hook self)))
    (if hook
        (hook commit-id start-size))
    commit-id))

(define-method commit ((ps <persistent-store>) 
                       #optional (root default: (root-object ps)))
  (let ((reloc (make-object-table))
        (start-size (get-tip-size ps)))
    (let loop ((i 0))
      (let ((cf (pstore-commit* ps root reloc #f)))
	(if (pair? cf)
	    (begin
	      (copy-in* ps reloc cf)
	      (loop (+ i 1)))
	    ;;
	    ;; return the new commit record ID
	    ;;
            (did-commit ps cf start-size))))))
