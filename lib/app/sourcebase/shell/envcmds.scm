(define (cd (where <string>))
  (set-current-path! *vsh-state*
  		     (fs-append-path (current-path *vsh-state*)
		     		     (string->fs-path where)))
  (values))

(define (chtop (where <string>))
   (set-current-local-top! *vsh-state* 
   			   (append-dirs (current-local-top *vsh-state*)
			   		(string->dir where))))

(define (pwd . args)
    (if (equal? args '("-long"))
	(let ((fs (current-filespace *vsh-state*)))
	    (format #t "FileSystem: ~a\n" (name fs)) ))
    (format #t "~a\n" (fs-path->string (current-path *vsh-state*)))
    (values))

(define (chroot (fs <string>))
   (let ((i (string-search fs $version-delim)))
      (if i
	(set-current-filespace!
	    *vsh-state* 
	    (string->snapshot (string->filesystem (substring fs 0 i)) 
			      (substring fs (+ i 1))))
	(set-current-filespace! *vsh-state* (string->filesystem fs))))
   (set-current-path! *vsh-state* $root-path)
   (values))

(define (why . args)
   (if (string=? (car args) "not")
       (set-reasons! *vsh-state* '())
       (set-reasons! *vsh-state* (append (reasons *vsh-state*)
       					 (map string->changereq args))))
   (values))
   

(define (sync)
  (let ((id (commit *pstore*)))    
    (format #t "commit record: ~d,~d\n" (car id) (cdr id))
    (values)))

(define (proc-id)
   (print (user *vsh-state*))
   (values))
