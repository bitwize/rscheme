#|
(define (MM #optional (file default: (current-error-port)))
  (let ((MM-port (if (string? file)
		     (open-output-file file)
		     file)))
    (format MM-port "#\n# ~a\n#\n" file)
    (if (string? file)
	(begin
	  (on-exit (lambda () (close-output-port MM-port)))
	  (add-image-save-hook! 
	   (lambda ()
	     (if MM-port
		 (begin
		   (set! MM-port 
		   (close-output-port MM-port)
			    (set! MM-port #f))
    (set-load-hook!
     (lambda (file)
       (with-module paths
	 (format MM-port
		 "$(PRODUCT): ~a\n" (append-path (current-directory) file))
	 (flush-output-port MM-port)
	 file)))))
|#

(define (setup-path-hook file-or-port)
  (let ((port (cond
	       ((string? file-or-port)
		(open-output-file file-or-port))
	       ((output-port? file-or-port)
		file-or-port)
	       (else
		(error "~s: not a file name or port" file-or-port)))))
    ;
    (define (file-hook file envt)
      (format port "~a\n" (append-path (current-directory) file))
      (flush-output-port port)
      file)
    ;
    (define (done-hook)
      (close-output-port port)
      (set-load-hook! #f)
      (remove-image-save-hook! done-hook))
    ;
    (add-image-save-hook! done-hook)
    (set-load-hook! file-hook)
    (values)))

(if (getenv "RSQ_PICKY_LIST")
    (setup-path-hook (getenv "RSQ_PICKY_LIST")))
