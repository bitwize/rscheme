(define (string->group (str <string>))
   (or (table-lookup (group-table *application*) str)
       (error "~a: no such group" str)))
   
(define (string->user (str <string>))
   (or (table-lookup (user-table *application*) str)
       (error "~a: no such user" str)))
   
(define (string->filesystem (str <string>))
   (or (table-lookup (file-system-table *application*) str)
       (error "~a: no such filesystem" str)))
       
(define (string->snapshot (fs <file-system>) (str <string>))
   (or (table-lookup (snapshot-table fs) str)
       (error "~a: no such snapshot in fs `~a'" str (name fs))))
       
(define (string->changereq (str <string>))
  (let ((n (string->number str)))
    (if n
	(or (table-lookup (change-request-table *application*) 
			  (string->number str))
	    (error "~a: no such change request" str))
	(error "~a: badly formatted change request number" str))))


(define (vsh-lookup-version (str <string>))
    (vsh-path->version (string->fs-path str)))

(define (vsh-path->version (path <fs-path>))
    (find-version (current-filespace *vsh-state*)
		  (fs-append-path (current-path *vsh-state*) path)))

(define (parse-kvlist lst)
   (map (lambda (a)
   	   (let ((i (string-search a #\=)))
	      (if i
	          (cons (string->symbol (substring a 0 i))
		        (substring a (+ i 1)))
		  (cons (string->symbol a) #t))))
        lst))
	
(define (local-file (path <string>))
  (let ((z (substring (fs-path->string (current-path *vsh-state*)) 1)))
   (pathname->string
    (append-path (if (string=? z "")
    		     (current-local-top *vsh-state*)
		     (append-dirs (current-local-top *vsh-state*)
    			          (string->dir z)))
    		 (string->file path)))))

;; projects the given path (taken relative to the local filesystem)
;; onto the virtual (versioned) filesystem
;;
;; for example, if the current-local-top is /u/donovan/foo
;;             and the current-local-dir is /u/donovan/foo/quux/data

(define (projected-path (path <string>))
  (fs-append-path (current-path *vsh-state*)
		  (string->fs-path path)))

