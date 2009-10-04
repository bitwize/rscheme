#|------------------------------------------------------------*-Scheme-*--|
 | File:	    rs/backend/c/cload.scm
 |
 |          Copyright (C)1998 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |	    See <http://www.rscheme.org/> for the latest info.
 |
 | File version:     1.4
 | File mod date:    2006-01-28 16:33:38
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  rs.backend.c
 |
 | Purpose:          Compile and load some code
 |		     (as in, REALLY compile it)
 `------------------------------------------------------------------------|#

,(use syscalls unixm)

(define *temp-dir* "~/lib/rs.so")
(define *dir-block* 0)

(define (assign-so-name outf)
  (let ((solib-dir (getenv "RS_DYNAMIC_LIB")))
    (if solib-dir
        (let loop ((n 1))
          (let ((p (format #f "~a/dynamic_~05d.so" solib-dir n)))
            (if (os-file-exists? p)
                (loop (+ n 1))
                p)))
        (string-append outf ".so"))))
  
(define (assign-temp-name)
  (let* ((dbs (format #f "~02d" *dir-block*))
	 (f (string-append
	     (pathname->os-path (string->file *temp-dir*))
	     "/"
	     dbs)))
    (if (not (stat f))
	(mkdirs f))
    (if (not (stat-directory? (stat f)))
	(error "~a: not a directory" f))
    (let loop ((n 0))
      (if (> n 100)
	  (begin
	    (set! *dir-block* (+ *dir-block* 1))
	    (assign-temp-name))
	  (let ((js (format #f "~04d" 
			    (modulo
			     (bitwise-and
			      (bitwise-xor
			       (string->hash (time->string (time)))
			       (random))
			      #xFFFFFF)
			     10000))))
	    ;; the `#' may be used someday as a signal
	    ;; to the image saver that the ld.so is brand-new
	    ;; (and hence, perhaps, should be renamed)
	    (if (stat (string-append f "/#unit-" js ".c"))
		(loop (+ n 1))
		(values (string-append f "/#unit-" js)
			(string-append "dl_" dbs "_" js))))))))

(define (gen-sccs-id)
  (let ((pw (getpw (getuid)))
	(t (time->string (time) "%Y-%m-%d %H:%M:%S"))
	(h (hostname)))
    (if pw
	(format #f "~a <~a@~a> ~a"
		(if (vector-ref pw 6)
		    (car (string-split (vector-ref pw 6) #\,))
		    "")
		(vector-ref pw 0)
		h
		t)
	(format #f "<@~a> ~a" h t))))

;;;
;;;  order-preserving union of strings
;;;

(define (collect-rqmts cds prop)
  (let ((tbl (make-string-table))
	(lst '()))
    (for-each
     (lambda (cd)
       (for-each
	(lambda (rqmt)
          (let ((key (if (pair? rqmt)
                         (car rqmt)
                         rqmt)))
            (if (not (table-lookup tbl key))
                (begin
                  ;;(dm 991 "~s => ~s" prop rqmt)
                  (table-insert! tbl key #t)
                  (set! lst (cons rqmt lst))))))
	(get-property cd prop '())))
     cds)
    (reverse lst)))

;;;

(define (compile-and-load cds)
  (if (with-module repl *compile-verbose*)
      (format #t "generating code for ~d procs...\n" (length cds)))
  (if (pair? cds)
      (compile-and-load* cds)
      (error "compile-and-load: nothing to flush")))

(define (compile-prereq-c-files lst)
  (map
   (lambda (cf-spec)
     ;;
     (define (get-prop kwd)
       (if (and (pair? cf-spec)
                (memq kwd cf-spec))
           (let ((t (cadr (memq kwd cf-spec))))
             (if (string? t)
                 (list t)
                 t))
           '()))
     ;;
     (bind ((cf (if (pair? cf-spec) (car cf-spec) cf-spec))
            (outf (string-append (assign-temp-name) ".o"))
	    (dir (pathname->os-path (file-directory (string->file cf)))))
       (if (with-module repl *compile-verbose*)
	   (dm 212 "compiling: ~a in ~s" cf dir))
       (sync-with-c-compiler
	(start-c-compiler 
         cf outf
         other-c-flags: (get-prop 'other-c-flags:)
         ;; -IXXX -I- ...
         other-local-include-dirs: (get-prop 'other-local-include-dirs:)
         ;; ... -I- -IXXX
         other-include-dirs: (get-prop 'other-include-dirs:)
         partial: #t
         in-directory: dir))
       outf))
   lst))

(define (compile-and-load* cds)
  (bind ((co-files (compile-prereq-c-files 
		    (collect-rqmts cds 'other-c-files)))
	 (outf un (assign-temp-name))
	 (sofile (assign-so-name outf))
	 (cfile (string-append outf ".c"))
	 (other-h-files (collect-rqmts cds 'other-h-files))
	 (p (open-part (or (get-property (car cds) 'source-file #f)
			   (string->file "unknown.scm"))
		       (string->file cfile)
		       un
		       "dyn_u"
		       `#(tag 1
			  build-id ,(with-module start *version*)
			  includes ,other-h-files
			  sccs-id ,(gen-sccs-id)))))
    ;;
    (for-each (lambda (cd)
		(write-into-part p cd))
	      cds)
    ;;
    (write-unit-linkage (get-output-port p)
			unit-name: un
			parts: (list p))
    ;;
    (close-part p)
    ;;
    (if (with-module repl *compile-verbose*)
	(format #t "compiling unit <~a>...\n" un))
    ;;
    (sync-with-c-compiler 
     (start-c-compiler 
      cfile sofile 
      other-o-files: (append (collect-rqmts cds 'other-o-files)
			     co-files)
      other-c-flags: (collect-rqmts cds 'other-c-flags)
      other-libs: (collect-rqmts cds 'other-libs)
      other-local-include-dirs: (collect-rqmts cds 'other-local-include-dirs)
      other-include-dirs: (collect-rqmts cds 'other-include-dirs)
      other-lib-dirs: (collect-rqmts cds 'other-lib-dirs)))
    ;;
    (if (with-module repl *compile-verbose*)
	(dm 210 "loading: ~a" sofile))
    (dl-c-unit sofile un)
    (let* ((m_ptr (find-linked-module un))
	   (p_ptr (find-part-in-linked-module m_ptr 1)))
      (for-each
       (lambda (cd)
	 (let* ((t (template cd))
		(k (code-pointer t)))
	   ;(format #t "linking[~d] ~s ~s\n" k (function-scope cd) t)
	   (bind ((cp fn (find-code-ptr-in-part p_ptr k)))
	     (set-code-pointer! t cp)
	     (set-linkage-info! t fn))))
       cds)
      cds)))

(define-method display-object ((self <dynamic-link-error>) port)
  (format port "**> ~a\n" (message self)))
