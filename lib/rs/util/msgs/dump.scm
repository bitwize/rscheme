;;;
;;;  dump out the message tables
;;;

(define-message-table rs.util.msgs 413)

(define (module->message-table m)
  (with-module
      repl
    (with-module
	compiler
      (let ((tle (top-level-envt m)))
	(and (lookup tle '*messages*)
	     (eval-in-envt '*messages* tle))))))

(define (dump-message-table (dir <directory-name>) #optional module)
  (within-directory dir
   (lambda ()
     (let ((tbl (make-symbol-table)))
       (define (dump1 mtab)
	 (if mtab
	     (let ((mtab-name (name mtab)))
	       (if (not (table-lookup tbl mtab-name))
		   (let ((file (string->file (format #f "~a.mls" mtab-name))))
		     (table-insert! tbl mtab-name file)
		     (call-with-output-file
			 (pathname->string file)
		       (lambda (port)
			 (dump-to-port port mtab))))))))
       ;
       (if module
	   (dump1 (module->message-table module))
	   (for-each
	    (lambda (m)
	      (dm "checking module: ~s" m)
	      (dump1 (module->message-table m)))
	    (map cdr (installed-modules))))))))

(define (dump-to-port port (mtab <message-table>))
  (format port ";;; -*-Scheme-*-\n")
  (format port "(message-table\n")
  (format port "    ~s ~s\n" (name mtab) (id mtab))
  (for-each
   (lambda ((m <message>))
     (format port " (~d ~s ~s)\n" (id m) (type m) (default-text m)))
   (sort
    (value-sequence (message-index mtab))
    (lambda (m1 m2)
      (< (id m1) (id m2)))))
  (format port ")\n"))

(dump-message-table (string->dir "/tmp/mt"))
