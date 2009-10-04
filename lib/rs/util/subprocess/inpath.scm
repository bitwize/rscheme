
(define *cmd-path* #f)
(define (cmd-path)
  (if (not *cmd-path*)
      (set! *cmd-path* (string-split (table-lookup (process-environment)
						   "PATH")
				     #\:)))
  *cmd-path*)

(define (cmd-in-path cmd)
  (let loop ((p (cmd-path)))
    (if (null? p)
	(error "~a: could not find" cmd)
	(let ((t (string-append (car p) "/" cmd)))
	  (if (file-access? t (access-mask execute))
	      t
	      (loop (cdr p)))))))

