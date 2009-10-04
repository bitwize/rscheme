;;;
;;;  go and compile it
;;;

(define (get-makefile-preamble)
  (file->string
   (pathname->os-path
    (string->file "[resource]/buildenv/preamble.mak")))) 

(define (get-from-preamble var)
  (with-module regex
   (bind ((mfp (get-makefile-preamble))
	  (pat (reg-expr->proc `(seq ,var
				     "="
				     (save (* (not #\newline)))))))
    (let loop ((r "")
	       (pat pat)
	       (i 0))
      (bind ((s e str (pat mfp i)))
        (if (char=? (string-ref str (- (string-length str) 1)) #\\)
	    (loop (string-append r (substring str 0 (- (string-length str) 1)))
		  (reg-expr->proc '(prefix
				    (seq
				     (* space)
				     (save (* (not #\newline))))))
		  (+ e 1))
	    (string-append r str)))))))

(define *CFLAGS* (string-split (get-from-preamble "CFLAGS1") #\space))
(define *CC* (get-from-preamble "CC"))

(define *include-dirs*
  (append
   (if (stat "/opt/freeware/include")
       '("/opt/freeware/include")
       '())
   (list (pathname->os-path (string->file "[install]/include")))))

(define *lib-dirs*
  (append
   (if (stat "/opt/freeware/lib")
       '("/opt/freeware/lib")
       '())
   ;; note: the -Wl,-bI:[install]/lib/rs.exp
   ;;       below depends on this being the last entry
   (list (pathname->os-path (string->file "[install]/lib")))))

,(use rs.util.subprocess)

;;; returns a <process> object

;;;  other-o-files is a list of `.o' files to be included,
;;;  other-c-files is a list of (additional) `.c' files to be included,
;;;  other-libs is a list of libraries to be included (e.g., ("z" "termcap"))

(define (start-c-compiler src dst 
			  #key (other-o-files default: '())
			       (other-c-files default: '())
                               (other-c-flags default: '())
			       (other-libs default: '())
			       (other-local-include-dirs default: '())
			       (other-include-dirs default: '())
			       (other-lib-dirs default: '())
			       (partial default: #f)
			       (in-directory default: #f))
  (let* ((aix? (and (memq 'aix (os-type)) #t))
	 (args `(,@*CFLAGS*
                 ,@other-c-flags
		 ,@(map (lambda (dir)
			  (string-append "-I" dir))
			(if in-directory
			    (cons "." other-local-include-dirs)
			    other-local-include-dirs))
		 "-I-"
		 ,@(map (lambda (dir)
			  (string-append "-I" dir))
			(append *include-dirs* other-include-dirs))
		 ,@(if (not partial)
		       '("-shared")
		       '("-c"))
		 "-fPIC"
		 ,src
		 ,@other-o-files
		 ,@other-c-files
		 "-o"
		 ,dst
		 ,@(if partial
		       '()
		       (map (lambda (libdir)
			      (string-append "-L" libdir))
			    (append *lib-dirs* other-lib-dirs)))
                 ;; special tweaks for AIX
                 ,@(if aix?
                       (if (not partial)
                           (list (format #f "-Wl,-bI:~a/rs.exp" 
                                         (last *lib-dirs*)))
                           '())
                       '())
                 ;;
		 ,@(if partial
		       '()
		       (map (lambda (lib)
			      (string-append "-l" lib))
			    other-libs)))))
    (if (with-module repl *compile-verbose*)
	(dm 219 "  ccompile: ~s in ~s" args in-directory))
    (with-module rs.sys.threads.manager
      (if in-directory
          (within-directory
           (string->dir in-directory)
           (lambda ()
             (apply run *CC* args)))
          (apply run *CC* args)))))

(define (sync-with-c-compiler p)
  (with-module
      rs.sys.threads.manager
    (check-exit-status p)))
