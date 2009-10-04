
(define (MM #optional (file default: (current-error-port)))
  (let ((MM-port (if (string? file)
		     (open-output-file file)
		     file)))
    (format MM-port "#\n# ~a\n#\n" file)
    (if (string? file)
	(on-exit
	 (lambda ()
	   (close-output-port MM-port))))
    ((with-module repl set-load-hook!)
     (lambda (file envt)
       (with-module paths
	 (format MM-port
		 "$(PRODUCT): ~a\n" (append-path (current-directory) file))
	 (flush-output-port MM-port)
	 file)))))
