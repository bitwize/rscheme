#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/repl/loadmodule.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.5
 | File mod date:    2003-06-22 18:15:05
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  repl
 |
 `------------------------------------------------------------------------|#

(define (module-name-components->sub-dir-modules lst)
  (let ((d (make <directory-name>
		 rooted-at: #f
		 steps: lst))
	(d2 (make <directory-name>
		  rooted-at: #f
		  steps: (reverse! (cdr (reverse lst))))))
    (list 
     ;;
     ;;  If /some/dir/ is in the search path, and we are trying
     ;;  to load module foo.bar, then look for:  
     ;;
     ;;                  /some/dir/foo/bar/module.mif
     ;;  
     (cons (make <file-name>
                 file-directory: d
                 filename: "module"
                 extension: "mif")
           'mif)
     ;;
     ;;  look for:  
     ;;                  /some/dir/foo/bar/module.scm
     ;;  
     (cons (make <file-name>
                 file-directory: d
                 filename: "module"
                 extension: "scm")
           'scm))))

(define (module-name-components->in-dir-modules (lst <list>))
  (let ((d (make <directory-name>
		 rooted-at: #f
		 steps: lst))
	(d2 (make <directory-name>
		  rooted-at: #f
		  steps: (reverse! (cdr (reverse lst))))))
    (list
     ;;
     ;;  If /some/dir/ is in the search path, and we are trying
     ;;  to load module foo.bar, then look for:  
     ;;
     ;;                  /some/dir/foo/bar.mif
     ;;  
     (cons (make <file-name>
                 file-directory: d2
                 extension: "mif"
                 filename: (last lst))
           'mif))))

(define (module-name->rel-paths (name <symbol>))
  (let ((lst (string-split (symbol->string name) #\.)))
    ;; reject anything that has empty sections, i.e., from "foo..bar"
    (if (any? (lambda ((p <string>))
                (string=? p ""))
              lst)
        #f
        (append
         ;; if the module name does not have any `.', then don't
         ;; consider in-dir modules (mlink's load-compiled-module
         ;; will handle that)
         (if (> (length lst) 1)
             (module-name-components->in-dir-modules lst)
             '())
         (module-name-components->sub-dir-modules lst)))))

(define (find-m-by-style (search <list>) (in <directory-name>))
  (let loop ((s search))
    (if (null? s)
	(values)
	(let* ((p (append-path in (caar s)))
	       (f (pathname->os-path p)))
	  (if (os-file-exists? f)
	      (values f p (cdar s))
	      (loop (cdr s)))))))

(define (hier-find-module name)
  (let ((rels (module-name->rel-paths name)))
    (if rels
	(let loop ((p (module-search-path)))
	  (if (null? p)
	      #f
	      (bind ((sfn fn typ (find-m-by-style rels (car p))))
		(if sfn
		    (values fn typ)
		    (loop (cdr p))))))
	#f)))

(define (load-module-from-source (name <symbol>) (f <file-name>))
  (let* ((e (make-user-initial))
	 (n (load-into e (pathname->os-path f)))
	 (ln  (if (symbol? n) n name))
	 (b (get-loaded-module ln)))
    (or b
	(error "~a: did not define-module ~s" f ln))))

;;; load a module using the foo.bar.baz ==> foo/bar/baz
;;; filesystem naming convention

(define (load-hier-module (name <symbol>) scan-only?)
  (bind ((fn typ (hier-find-module name)))
    (if fn
        (if scan-only?
            fn
            (case typ
              ((mif) (link-load-module name fn))
              ((scm) (load-module-from-source name fn))))
	#f)))

(%early-once-only
 (add-module-finder! load-hier-module))
