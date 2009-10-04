,(use paths tables)

(define (death (err <condition>) next-handler)
  (display "--- internal error occurred\n" *console-error-port*)
  (display err *console-error-port*)
  (newline *console-error-port*)
  (process-exit 2))

(define (elim-dups lst)
  (let ((t (make-table string=? string->hash)))
    (let loop ((r '())
	       (l lst))
      (if (null? l)
	  (reverse r)
	  (if (table-lookup t (car l))
	      (loop r (cdr l))
	      (begin
		(table-insert! t (car l) #t)
		(loop (cons (car l) r) (cdr l))))))))

(define (check-pkgs-built pkg-o-dir pkg-data)
  (let ((error-cnt 0))
    ;;
    ;; make sure the necessary pkg.o's are present
    ;;
    (for-each
     (lambda (pkg)
       (format #t "pkg... ~s\n" pkg)
       (if (not (file-exists? (append-path
			       pkg-o-dir
			       (string->file (format #f "~a.o" pkg)))))
	   (begin
	     (format *console-error-port*
		     "package `~a' not installed; use rsc to generate it\n"
		     pkg)
	     (set! error-cnt (+ error-cnt 1)))))
     pkg-data)
    (if (not (= error-cnt 0))
	(process-exit 1))))

(define (build-shell (dest-dir <string>)
		     (shell-name <string>) ;; same as (last part of) directory
		     (mx-data <list>))     ;; data collected from MX files
  (let ((pkg-data (apply append (map caddr mx-data)))
	(pkg-o-dir (append-dirs (string->dir (getenv "INSTALL_DIR"))
				(string->dir "lib")))
	(error-cnt 0))
    (format #t "pkg-o-dir => ~a\n" pkg-o-dir)
    (check-pkgs-built pkg-o-dir pkg-data)
    ;;
    (with-output-to-file
	(string-append dest-dir "/shell.c")
      (lambda ()
	(emit-shell-preamble)
	(emit-shell-pkg-setup shell-name mx-data pkg-data)
	(emit-shell-postamble)))
    ;;
    (with-output-to-file
	(string-append dest-dir "/boot.scm")
      (lambda ()
	(emit-boot-file shell-name pkg-data mx-data)))
    ;;
    (with-output-to-file
	(string-append dest-dir "/Makefile")
      (lambda ()
	(emit-makefile-setup shell-name pkg-data mx-data)
	(emit-makefile-preamble)
	(emit-makefile-postamble)))))

;; a restricted case of `remove-specials' from rsc
;; because module names with #\.'s in them get #\_ in
;; the filesystem

(define (remove-specials name)
  (list->string
   (map (lambda (ch)
	  (if (eq? ch #\.)
	      #\_
	      ch))
	(string->list name))))

(define (expand-all lst)
  (let ((pkg-o-dir (append-dirs (string->dir (getenv "INSTALL_DIR"))
				(string->dir "lib"))))
    (if (and (= (length lst) 1)
	     (string=? (car lst) "all"))
	(let ((r '()))
	  (for-each
	   (lambda (f)
	     (if (matches-mx-file-pattern? f)
		 (let* ((mx-data (get-mx-data 
				 (substring f 0 (- (string-length f) 3))))
		       (o-files (map (lambda (u)
				       (append-path
					pkg-o-dir
					(string->file
					 (string-append u ".o"))))
				     (caddr mx-data))))
		   (if (every? file-exists? o-files)
		       (set! r (cons mx-data r))
		       (format #t "one is missing: ~s\n" o-files)))))
	   (faux-scandir
	    (pathname->os-path (string->file "[resource]/modules"))))
	  (topo-sort-deps r))
	(topo-sort-deps (map get-mx-data lst)))))

(define (faux-scandir dir)
  (let ((port (open-input-process (format #f "ls ~a" dir))))
    (let loop ((r '()))
      (let ((l (read-line port)))
	(if (eof-object? l)
	    (begin
	      (close-input-port port)
	      (reverse r))
	    (loop (cons l r)))))))

;;;  eliminate entries that are already present in the 
;;;  running image, and sort by dependencies

(define (topo-sort-deps m-list)
  (with-module mlink
    (let ((mtab (make-symbol-table))
	  (topolist '()))
      ;; figure out what's built in already
      (for-each (lambda (mp)
		  (table-insert! mtab (car mp) 'builtin))
		(installed-modules))
      ;; insert entries from m-list
      (for-each (lambda (mx)
		  (if (not (table-lookup mtab (car mx)))
		      (table-insert! mtab (car mx) (cons #f mx))))
		m-list)
      ;; ensure that a module (by name) is present
      ;; return #f if not, else #t and ensure predecessors
      (define (ensure mname)
	(let ((e (table-lookup mtab mname)))
	  ;(format #t "ensure ~s => ~s\n" mname e)
	  (cond
	   ((eq? e 'builtin)
	    #t)
	   ((pair? e)
	    (if (car e)
		#t
		(let ((mx (cdr e)))
		  (set-car! e #t)
		  (if (every? ensure (list-ref mx 4))
		      (begin
			(set! topolist (cons mname topolist))
			#t)
		      (begin
			(set-car! e #f)
			#f)))))
	   (else
	    (format #t "*** warning: module `~s' not available\n" mname)
	    #f))))
      (for-each ensure (map car m-list))
      (map symbol->string (reverse topolist)))))

(define (matches-mx-file-pattern? (f <string>))
  (and (> (string-length f) 3)
       (string=? (substring f (- (string-length f) 3)) ".mx")))

(define (main args)
  (format #t "inst => ~s\n" (getenv "INSTALL_DIR"))
  (handler-bind (<condition> death)
    (let* ((dir (car args))
	   (name (last-directory (string->dir dir)))
	   (modules (expand-all (cdr args))))
      (format #t "dir => ~s\n" dir)
      (format #t "modules => ~s\n" modules)
      (build-shell dir name (map get-mx-data modules))
      #t)))

(define (mx-path pkg)
  (append-path (string->dir "[resource]/modules")
	       (string->file (string-append (remove-specials pkg)
					    ".mx"))))

(define (mx-data-present? pkg)
  (file-exists? (mx-path pkg)))
   
(define (get-mx-data pkg)
  (call-with-input-file (pathname->os-path (mx-path pkg)) read))

(define (figure-extra-libs pkg)
  (if (equal? "syscalls" pkg)
      (if (memq 'sunos (os-type))
	  '("socket" "nsl")
	  '())
      (figure-extra-libs* pkg)))

(define (figure-extra-libs* pkg)
  (let ((a (assoc pkg '(("db" "db")
			("x11" "gd" "X11")
			("ttywin" "curses")))))
    (if a
	;; for each needed library, make sure we can find it...
	(map
	 (lambda (lib)
	   (if (any? (curry lib-in-place? lib)
		     *standard-lib-places*)
	       lib
	       ;; try to find it elsewhere
	       (let loop ((other *other-lib-places*))
		 (if (null? other)
		     (begin
		       (format *console-error-port*
			       "*** warning: couldn't find lib~a.a\n"
			       lib)
		       #f)
		     (if (lib-in-place? lib (car other))
			 (cons (car other) lib)
			 (loop (cdr other)))))))
	 (cdr a))
	(let ((mx (get-mx-data pkg)))
	  (if mx
	      (list-ref mx 5) ;; pick up module's `other-libs'
	      '())))))

(define (lib-in-place? lib place)
  (or (os-file-exists? (string-append place "/lib" lib ".a"))
      (os-file-exists? (string-append place "/lib" lib ".so"))))

(define *standard-lib-places* '("/lib" "/usr/lib"))
(define *other-lib-places* '("/usr/local/lib"
			     "/usr/local/rwi/lib"
			     "/usr/X11R6/lib"))

;;=======================================================================
;;      _          _ _       
;;  ___| |__   ___| | |  ___ 
;; / __| '_ \ / _ \ | | / __|
;; \__ \ | | |  __/ | || (__ 
;; |___/_| |_|\___|_|_(_)___|
;;
;;=======================================================================

(define (emit-shell-preamble)
  (display
"#include <stdlib.h>
#include <string.h>
#include <rscheme/api.h>
#include <rscheme/osglue.h>
#include <rscheme/stdmodul.h>
#include <rscheme/rlseconf.h>
"))

(define (emit-shell-pkg-setup name mxs pkgs)
  (for-each (lambda (p)
	      (format #t "#include <rscheme/pkgs/~a.h>\n" p))
	    pkgs)
  (display "struct module_descr *(std_modules[]) = {\n")
  (for-each (lambda (p)
	      (format #t "\t&module_~a,\n" p))
	    pkgs)
  (display "STD_MODULES_DECL };\n\n")
  (format #t "#define DEFAULT_IMG \"/resource/~a.fas\"\n" name))

(define (emit-shell-postamble)
  (display 
"int main( int argc, const char **argv )
{
  char temp[1024];

  rs_install_dir = getenv( \"RS_INSTALL_DIR\" );
  if (!rs_install_dir)
      rs_install_dir = INSTALL_DIR;

  sprintf( temp, \"%s\" DEFAULT_IMG, rs_install_dir );
  if (!os_file_exists_p( temp ) && strstr( temp, \".fas\" ))
    {
      /*  if the default image isn't there, look for an `.orig' image
       *  as well.  This allows us to easily replace the default
       *  fasl image with preloaded material
       */
       strcpy( strstr( temp, \".fas\" ), \".orig.fas\" );
    }
  
  return rscheme_std_main( argc, argv, std_modules, temp );
}
"))


;;=======================================================================
;;  __  __       _         __ _ _      
;; |  \/  | __ _| | _____ / _(_) | ___ 
;; | |\/| |/ _` | |/ / _ \ |_| | |/ _ \
;; | |  | | (_| |   <  __/  _| | |  __/
;; |_|  |_|\__,_|_|\_\___|_| |_|_|\___|
;;
;;=======================================================================

(define (emit-makefile-preamble)
  (display 
"all:: $(PRODUCT) $(PRODIMG)

XCFLAGS='-DINSTALL_DIR=\"$(INSTALL_DIR)\"'

include $(INSTALL_DIR)/resource/buildenv/preamble.mak

CFILES=shell.c
INSTALL=cp -p

OFILES=$(CFILES:.c=.o)
"))

(define (emit-boot-file name pkgs mxs)
  (for-each
   (lambda (mx)
     (format #t ";; ~s\n" (car mx))
     (pp `(with-module
	      mlink
	    (with-module
		paths
	      (format #t "--- Linking in: ~a\n" ',(car mx))
	      (link-load-module 
	       ',(car mx) 
	       (string->file ,(format #f "[resource]/modules/~a.mif"
				      (cadr mx)))))))
     (newline))
   mxs))
  
(define (emit-makefile-setup name pkgs mxs)
  (display "MD=$(INSTALL_DIR)/resource/modules\n")
  (display "LB=$(INSTALL_DIR)/lib\n")
  (format #t "PRODUCT=~a\n" name)
  (format #t "PRODIMG=~a.orig.fas\n" name)
  (display "PKGS=")
  (for-each (lambda (p)
	      (format #t "$(LB)/~a.o " p))
	    pkgs)
  (newline)
  (display "LINKM=boot.scm")
  (newline)
  (display "XLDX=")
  (for-each (lambda (lib)
	      (if (pair? lib)
		  (format #t "-L~a -l~a " (car lib) (cdr lib))
		  (if (string? lib)
		      (format #t "-l~a " lib))))
	    (apply append (map figure-extra-libs pkgs)))
  (newline)
  (newline))

(define (emit-makefile-postamble)
  (display
"LIB_RS=$(LB)/librs.a
$(PRODUCT): $(OFILES) $(PKGS) $(LIB_RS)
	$(CC) $(CFLAGS) $(OFILES) $(PKGS) -o $(PRODUCT) $(LDX_FLAGS) $(XLDX)

$(PRODIMG): $(PRODUCT)
	./$(PRODUCT) -image $(INSTALL_DIR)/resource/system.img \\
	      $(LINKM) -c.repl $(PRODIMG)

install:: $(INSTALL_DIR)/bin
	$(INSTALL) $(PRODUCT) $(INSTALL_DIR)/bin
	$(INSTALL) $(PRODIMG) $(INSTALL_DIR)/resource

$(INSTALL_DIR)/bin:
	mkdir $(INSTALL_DIR)/bin

clean::
	rm -f $(OFILES) $(PRODUCT)

depend::
	$(CC) $(CFLAGS) -MM -I. $(CFILES) > depends
"))
