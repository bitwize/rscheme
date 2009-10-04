#|------------------------------------------------------------*-Scheme-*--|
 | File:    compiler/modules/findmodl.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.6
 | File mod date:    1998-02-19 21:00:04
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  (rsc)
 |
 `------------------------------------------------------------------------|#


;;
;; this path is suitable for SYSTEM BUILD
;;

(define *library-path* '())

(define (reinit-library-path)
  (set! *library-path*
	(map (lambda (base)
	       (append-dirs base (string->dir "modules")))
	     *dist-resource-path*)))

(reinit-library-path)

;;

(define *loaded-modules* '())

;;

(define (get-ct-module name)
  (if (null? *loaded-modules*)
      (set! *loaded-modules* (builtin-modules)))
  (let ((b (assq name *loaded-modules*)))
    (if b
	(cdr b)
	(install-module name 
			(load-dependencies
			 (load-module name
				      (find-module name)
				      #f))))))

;;

(define (load-dependencies m)
    ;; make an effort to locate all of the required modules
    (for-each (lambda ((im <imported-module>))
		(get-ct-module (name im)))
	      (module-imports m))
    m)

;; 

(define (install-module this-module-name m)
  (let ((extern (make-seq)))
    (link-into this-module-name m *loaded-modules* extern)
    (if (not (null? (seq->list extern)))
	(error "unresolved external modules: ~s\n" (seq->list extern)))
    (set! *loaded-modules* 
	  (cons (cons this-module-name m) *loaded-modules*))
    m))

;; find the .mif file for a module

(define (find-module name)
  (or (plain-find-module name)
      (hier-find-module name)
      (error "could not find module: ~s" name)))

(load "../../modules/paths/locate.scm")

(define (plain-find-module name)  ;; <= 0.7.1 module finder
  (let ((path (search-for-file
	       (string->file
		(form-file-name
		 (remove-specials (symbol->string name))
		 #f))
	       *library-path*
	       '("mx"))))
    (if path
	(let ((info (with-input-from-file (pathname->os-path path) read)))
	  (if (eq? (car info) name)
	      (make <file-name>
		     filename: (cadr info)
		     extension: "mif"
		     file-directory: (file-directory path))
              #f)) ;; found something, but not what we want
        #f)))
