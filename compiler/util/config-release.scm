#|------------------------------------------------------------*-Scheme-*--|
 | File:    compiler/util/config-release.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.13
 | File mod date:    2002-11-05 21:35:46
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  (rsc)
 |
 `------------------------------------------------------------------------|#

;; a component descriptor is a 4-element vector
;;   [0] = target name (usu. module name) for makefile
;;   [1] = subdir (eg, "lowscm" or "gc")
;;   [2] = ofile base (eg, "low_scheme" or "xgc")
;;   [3] = module name for linkage, or #f if none

(define (make-component-descriptors component-modules)
  (let ((cds (reverse *base-runtime-components*)))
    (for-each
     (lambda (m)
       (if (pair? (proj-names m))
	   ;; doesn't work with multiple sub-projects per module, though
	   (let ((subdir (cdr (assq 'directory (properties m)))))
	     (for-each
	      (lambda (link-name)
		(set! cds (cons (vector (symbol->string (name m))
					subdir
					link-name
					(name m))
				cds)))
	      (link-names m)))))
     component-modules)
    (reverse cds)))


(define (create-config-files component-modules)
  (let ((comps (make-component-descriptors component-modules)))
    ;;
    ;; create the Makefile.in
    ;;
    (call-with-dest-output-file
     "Makefile.in"
     (lambda (port)
       (display "##\n" port)
       (display "## Makefile for RScheme @RSCHEME_BUILD@\n" port)
       (display "##\n" port)

       (display "SHELL=/bin/sh\n" port)
       (display "AR=ar ru\n" port)
       (display "RANLIB=@RANLIB@\n" port)
       (display "INSTALL_DIR=@prefix@\n" port)
       (display "FINAL_INSTALL_DIR=@prefix@\n" port)

       (display "@PREAMBLE@\n" port)
       (display "IFLAGS=-I. -I- -Iinstall/include\n" port)

       (format port "SUBPROJ=~a\n" 
	       (join " \\\n\t" 
		     (map (lambda (comp)
			    (format #f "~a/~a.o"
				    (vector-ref comp 1)
				    (vector-ref comp 2)))
			  comps)))
       (format port "SUBPROJECTS=~a\n" (join " " (map (lambda (comp)
							(vector-ref comp 0))
						      comps)))
       (format port "SUBDIRS=~a\n" (join " " (map (lambda (comp)
						    (vector-ref comp 1))
						  comps)))
       (let ((post (locate-src-resource "buildenv/postambl.mak")))
         (format (current-error-port) "postamble => ~s\n" post)
         (display (file->string (pathname->string post)) port))
       (for-each (lambda (comp)
		   (format port "~a ~a/~a.o::\n"
			   (vector-ref comp 0)
			   (vector-ref comp 1)
			   (vector-ref comp 2))
		   (format port "\tcd ~a && $(MAKE)\n\n" (vector-ref comp 1)))
		 comps)))
    ;;
    ;; create release.cfg
    ;;
    (call-with-dest-output-file
     "release.cfg"
     (lambda (port)
       (format port "RSCHEME_VERSION=~s\n" *rscheme-version*)
       (format port "RSCHEME_BUILD=~s\n" *rscheme-build*)))
    ;;
    ;; create rlseconf.h's template
    ;;
    (call-with-dest-output-header-file
     "install/include/rscheme/rlseconf.cfg"
     (lambda (port)
       (format port "#define RSCHEME_VERSION ~s\n" *rscheme-version*)
       (format port "#define RSCHEME_BUILD ~s\n" *rscheme-build*)))
    ;;
    ;; create stdmodul.h
    ;;
    (call-with-dest-output-header-file
     "install/include/rscheme/stdmodul.h"
     (lambda (port)
       (format port "#include <rscheme/linktype.h>\n")
       (let ((ms '()))
	 (for-each (lambda (comp)
		     (if (vector-ref comp 3)
			 (set! ms (cons (string-append "module_"
						       (remove-specials
							(symbol->string
							 (vector-ref comp 3))))
					ms))))
		   comps)
	 (for-each (lambda (m)
		     (format port "extern struct module_descr ~a;\n" m))
		   ms)
	 (newline port)
	 (format port "#define STD_MODULES_DECL ~a, (struct module_descr *)0\n"
		 (join ", \\\n\t" (map (lambda (m)
					 (string-append "&" m))
				       ms))))))
    ;; 
    ;; create subproj.lst
    ;;
    (call-with-dest-output-file
     "subproj.lst"
     (lambda (port)
       (display (join " " (map (lambda (comp)
				 (vector-ref comp 0))
			       comps))
		port)
       (newline port)))
    (values)))

(define (call-with-dest-output-header-file dest proc)
  (call-with-dest-output-file
   dest
   (lambda (port)
     (let ((guard (apply string (map char-upcase 
				     (string->list 
				      (filename (string->file dest)))))))
       (format port "#ifndef _H_RSCHEME_~a\n" guard)
       (format port "#define _H_RSCHEME_~a\n\n" guard)
       (proc port)
       (format port "#endif /* _H_RSCHEME_~a */\n\n" guard)))))

(define (call-with-dest-output-file dest proc)
  (call-with-output-path 
      (append-path *dist-path* (string->file dest))
    proc))

(define (join seperator list)
  (if (null? list)
      ""
      (let ((l (make-seq)))
	(seq-add! l (car list))
	(for-each (lambda (item)
		    (seq-add! l seperator)
		    (seq-add! l item))
		  (cdr list))
	(apply string-append (seq->list l)))))

