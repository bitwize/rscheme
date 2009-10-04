
(define *loaded-files* (make-table string=? string->hash))

(set-load-hook! (lambda (path envt)
		  (xaction "LOAD HOOK: ~s\n" path)
		  (let ((pstr (pathname->string path)))
		    (table-insert! *loaded-files* 
				   pstr
				   (stat-mtime (stat pstr)))
		    path)))

(define (re-load)
  (table-for-each
   *loaded-files*
   (lambda (h k v)
     (xaction "reload: checking ~a" k)
     (if (time>? (stat-mtime (stat k)) v)
	 (begin
	   (xaction "...RELOADING\n")
	   (load k))
	 (xaction "...still ok\n")))))

