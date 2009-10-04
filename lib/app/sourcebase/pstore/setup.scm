
;;; NOTE: This procedure is clobbered by rsfam/legacy.scm
;;;       to also set up indirect page #2

(define (setup-app-indirect-pages ps)
  (setup-indirect-page ps 1 $app-classes))

(define (setup-pstore ps arg)
  (set-compression-method! ps "zlib-fast")
  ;;
  (setup-app-indirect-pages ps)
  (if (symbol? arg)
      (set! *access-mode* 'read-write)
      (begin
	(commit ps arg)
	(set! *access-mode* 'read-write)))
  (set! *application* (root-object ps))
  (set! *world-group* (world-group *application*))
  ;;
  (format #t "----------------------------------------\n")
  (format #t "Connected to database: ~a\n" (name *application*))
  (format #t "  created ~a\n"
	  (time->string (creation-time *application*) 
			"%Y-%m-%d %H:%M:%S"))
  (format #t "  application version ~a\n" 
	  (application-version *application*))
  (format #t "  structure version ~d\n" (structure-version *application*))
  (format #t "----------------------------------------\n")
  ;;
  (set! *pstore* ps)
  *application*)
