#|------------------------------------------------------------*-Scheme-*--|
 | File:    compiler/cgen/bldfiles.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.10
 | File mod date:    2005-04-08 17:43:36
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  (rsc)
 |
 `------------------------------------------------------------------------|#


;; build the various files that go in the module directory
;;
;; in particular:
;;    foo.h     the public interface file
;;    foo_p.h   the private interface file
;;    foo_r.h   the root definitions file
;;    Makefile  the makefile for building foo.o from sources
;;    foo_l.c   the main module linkage file (defines module_foo)

(define (support-file-path (bcx <build-context>) filename ext)
  (make <file-name> 
	filename: filename
	extension: ext
	file-directory: (dest-dir bcx)))
 
(define-method link-name ((m <module>))
  (assert (eq? (length (link-names m)) 1))
  (car (link-names m)))
  
(define (build-support-files (bcx <build-context>))
  (let ((link-file (alloc-file-name bcx 
				    (string-append (base-filename bcx) "_l")
				    "c"))
	(roots-file (alloc-file-name bcx 
				     (string-append (base-filename bcx) "_r")
				     "c"))
	(m (building bcx)))
    ;
    (set-c-files! bcx (cons link-file (c-files bcx)))
    (set-proj-names! m (list (base-filename bcx)))
    ;
    (call-with-output-file
	(pathname->string (support-file-path bcx "includes" "lst"))
      (lambda (port)
	(for-each (lambda (h)
		    (format port "~a.h\n" h))
		  (public-h-files bcx))))

    (call-with-output-file
	(pathname->string (support-file-path bcx link-file "c"))
      (lambda (port)
	(write-module-linkage-file bcx port)))

    (call-with-header-output-file
	(support-file-path bcx roots-file "h")
      (lambda (port)
	(write-root-declarations bcx port)))

    (call-with-header-output-file
	(private-interface-file bcx)
      (lambda (port)
	(write-private-interface bcx port roots-file)))
    
    (call-with-header-output-file
	(public-interface-file bcx)
      (lambda (port)
	(write-public-interface bcx port)))
    
    (call-with-output-file
	(pathname->string (make <file-name>
				filename: (makefile-name bcx)
				extension: #f
				file-directory: (dest-dir bcx)))
      (lambda (port)
	(write-makefile bcx port)))))

(define (call-with-header-output-file (path <file-name>) proc)
  (call-with-output-file
      (pathname->string path)
    (lambda (port)
      (let ((guard (string-upcase (filename path))))
	(display-disclaimer port)
	(format port "\n#ifndef _H_~a\n" guard)
	(format port "#define _H_~a\n\n" guard)
	(proc port)
	(format port "#endif /* _H_~a */\n" guard)))))

(define (write-makefile (bcx <build-context>) port)
 (let ((m (building bcx))
       (usr-iflags (user-inc-path bcx))
       (sys-iflags (system-inc-path bcx)))
  (write-char #\# port)
  (center-* port "Makefile for Module `~a'" (link-name m))
  (format port "#\n")
  (format port 
	  "# constructed by rsc for RScheme (~a),\n"
	  *rscheme-build*)
  (format port "# module `~a'\n" (name bcx))
  (format port "#\n")
  (format port "CFILES=")
  (for-each (lambda (f)
	      (format port " ~a.c" f))
	    (c-files bcx))
  (newline port)
  (format port "INCFILES=~a\n" (file-within-dir (public-interface-file bcx)))
  (format port "PRODUCT=~a.o\n" (link-name m))
  (if (pair? sys-iflags)
      (format port "SYS_IFLAGS=~a\n" (string-join
                                      " "
                                      (map (lambda (d)
                                             (string-append "-I" d))
                                           sys-iflags))))
  (if (pair? usr-iflags)
      (format port "USR_IFLAGS=~a\n" (string-join
                                      " "
                                      (map (lambda (d)
                                             (string-append "-I" d))
                                           usr-iflags))))
  (if (fluid-ref *package-mode*)
      (begin
	(format port "MIFBASEFILE=~a\n"
		(make <file-name>
		      filename: (base-filename bcx)
		      file-directory: (dir-from-to (dest-dir bcx)
					      (image-dest-dir bcx))
		      extension: #f))
	(format port "MIFNAME=~a\n" (base-filename bcx))))
  (newline port)
  ;; the $module-makefile winds up in install/resource/buildenv/module.mak,
  ;; although it is in src/... before the base system gets installed
  (format port "include ~a\n" $module-makefile)
  (if *make-copy-mode*
      (for-each (lambda (copied-file)
		  (format port "~a: ~a\n\tcp -p ~a .\n\n"
			  (cadr copied-file)
			  (car copied-file)
			  (car copied-file)))
		(copied-files bcx)))))

;; 
;; by default, don't emit Makefile instructions to copy
;; the source files.  Can be enabled with the "-make-recopies" flag
;; 

(define *make-copy-mode* #f)

(define (write-public-interface (bcx <build-context>) port)
  (let ((m (building bcx)))
    (center-* port "Public Interface for Module `~a'" (link-name m))
    (format port "#include <rscheme/linktype.h>\n")
    (format port 
	    "extern struct module_descr module_~a;\n"
	    (link-name m))
    (for-each (lambda (h)
		(format port "#include <rscheme/~a.h>\n" h))
	      (public-h-files bcx))))

(define (write-root-declarations (bcx <build-context>) port)
 (let ((m (building bcx)))
  (center-* port "Root declarations for module `~a'" (link-name m))))

(define (write-private-interface (bcx <build-context>) port roots-file)
 (let ((m (building bcx)))
  (center-* port "Private Interface for Module `~a'" (link-name m))
  (format port 
	  "#include \"~a\"\n"
	  (file-within-dir (public-interface-file bcx)))
  (format port "#include <rscheme/scheme.h>\n")
  (for-each (lambda (h)
	      (if (eq? (string-ref h 0) #\<)
		  (format port "#include ~a\n" h)
		  (format port "#include \"~a\"\n" h)))
	    (extern-h-files bcx))
  (for-each (lambda (h)
	      (format port "#include \"~a.h\"\n" h))
	    (h-files bcx))
  (format port
	  "#include \"~a.h\"\n"
	  roots-file)
  (if (pair? (module-bytecode-extensions m))
      (begin
	(for-each (lambda (x)
		    (format port "UINT_8 *bc_~a_extension" (cadr x))
		    (format port "( UINT_8 *pc, RS_bc_datum **args );\n"))
		  (module-bytecode-extensions m))))))

(define (write-module-linkage-file (bcx <build-context>) port)
 (let ((m (building bcx)))
  (display-disclaimer port)
  (format port 
	  "#include \"~a\"\n\n"
	  (file-within-dir (private-interface-file bcx)))
  (center-* port "Link file for the `~a' module" (link-name m))
  (newline port)
  ;;
  (if (pair? (module-bytecode-extensions m))
      (begin
	(format port "static struct bcx_descr bcx_tab[] = {\n")
	(for-each 
	 (lambda (x)
	   (format port "  { ~d, bc_~a_extension, ~s, &module_~a },\n" 
		   (car x)
		   (cadr x)
		   (cadr x)
		   (link-name m)))
	 (module-bytecode-extensions m))
	(format port "};\n")))
  ;;
  (newline port)
  (for-each
   (lambda (a-part)
     (format port
	     "extern struct part_descr ~a_part_~a;\n"
	     (link-name m)
	     (link-name a-part)))
   (parts bcx))
  (newline port)
  (format port "static struct part_descr *(parts_table[]) = {\n")
  (for-each
   (lambda (a-part)
     (format port "    &~a_part_~a,\n" (link-name m) (link-name a-part)))
   (parts bcx))
  (format port "    (struct part_descr *)0 };\n")
  (newline port)
  (format port
	  "struct module_descr module_~a = { \"~a\", parts_table,\n"
	  (link-name m)
	  (link-name m))
  (if (root-variable-info bcx)
      (begin
	(format port "\t~a,\n" (car (root-variable-info bcx)))
	(format port "\t~a,\n" (cadr (root-variable-info bcx)))
	(format port "\t(struct root_info *)0,\n\n"))
      (begin
	(format port "\t0 /* num roots */,\n")
	(format port "\t(obj *)0,\n")
	(format port "\t(struct root_info *)0,")))
  ;;
  (if (null? (module-bytecode-extensions m))
      (begin
	(format port "\t(struct bcx_descr *)0, /* bc_extensions */\n")
	(format port "\t0 /* num_bc_extensions */\n"))
      (begin
	(format port "\tbcx_tab, /* bc_extensions */\n")
	(format port "\t~d /* num_bc_extensions */\n"
		(length (module-bytecode-extensions m)))))
  ;;
  (format port "    };\n\n")
  ;;
  (let ((n (link-name m)))
    (format port "struct module_descr *RS_module_~a = &module_~a;\n" n n)
    (format port "struct module_descr *RS_fm_~a( void )\n" n)
    (format port "{ return &module_~a; }\n" n))))


