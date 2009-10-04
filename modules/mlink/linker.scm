#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/mlink/linker.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.11
 | File mod date:    2003-06-22 18:15:04
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  mlink
 |
 | Purpose:          link modules into local module space
 `------------------------------------------------------------------------|#

;; a function to link a module into an existing (linked)
;; space of modules
;; note that the degenerate space of linked modules is
;; the empty space, a condition under which we aren't
;; called

(define (make-imported-module (name <symbol>))
  (make <imported-module> 
	name: name
	link-commands: '()
	owner: #f
	actual-module: #f))

(define (get-extern-module extern (name <symbol>))
  (if extern
      (let ((t (assq name (seq->list extern))))
	(if t
	    (cdr t)
	    (let ((t (make-imported-module name)))
	      ;; create a new still-import'ed module
	      (seq-add! extern (cons name t))
	      t)))
      (error "~s: imported module not available" name)))


(define (exec-lc (lc <link-cmd>) m src-name im im-name)
  (if *verbose-link*
      (begin
        (format #t "------executing link command------\n")
        (print lc)))
  (execute-link-cmd lc m src-name im im-name))

(define (link-cmd-exec-order (lc <link-cmd>))
  (let ((c (object-class lc)))
    (cond
     ((eq? c <link-bdgs>) 0)
     ((eq? c <link-value>) 1)
     ((eq? c <link-xform>) 2)
     ((eq? c <link-method>) 3)
     (else
      (error "internal: unknown link-cmd class: ~s" c)))))

(define (link-into (src-name <symbol>) 
                   (m <module>)
                   (space <list>) 
                   extern)
  (let ((transpose (vector (make-dequeue)
			   (make-dequeue)
			   (make-dequeue)
			   (make-dequeue))))
    (for-each
     (lambda ((im <imported-module>))
       ;; NOTE! `resolve-module-reference' is defined in two places
       ;;       Once in rsc (the separately-compiling compiler), which
       ;;       gives a definition which returns #f if the named module
       ;;       is not in `space',
       ;;       and again in runtime.scm (this directory), which causes
       ;;       modules to be loaded on demand by calling get-module
       (let ((t (resolve-module-reference (name im) space)))
         (if t
             ;; it's a locally scoped module -- bind to it
             (let ((im-name (name im))
                   (im-m t))
               (if *verbose-link*
                   (format #t "binding ~s to ~s\n" src-name im-name))
	       (set-actual-module! im im-m)
	       (for-each 
		(lambda (lc)
		  (dequeue-push-back!
		   (vector-ref transpose (link-cmd-exec-order lc))
		   (vector lc im-m im-name))
		  (values))
		(link-commands im)))
           ;; it's not a locally scoped module -- it remains extern
           (begin
             (if *verbose-link*
                 (format #t "~s referencing external module ~s\n"
                         src-name
                         (name im)))
             (let (((xim <imported-module>)
                        (get-extern-module extern (name im))))
               (set-link-commands! xim
                                   (append (link-commands xim)
                                           (link-commands im))))))))
     (module-imports m))
    ;;
    ;;  having collected all the init commands, execute them
    ;;  in transposed order (ie, the link-bdg's from all modules
    ;;  before any link-values)
    ;;
    (vector-for-each
     (lambda (q)
       (let loop ()
	 (if (not (dequeue-empty? q))
	     (let (((cmd <vector>) (dequeue-pop-front! q)))
	       (exec-lc (vector-ref cmd 0)
			m
			src-name
			(vector-ref cmd 1)
			(vector-ref cmd 2))
	       (loop)))))
     transpose)))

 

;; link-into recursively
  
(define (link-into* (m-name <symbol>) 
		    (m <module>) 
		    (space <list>) 
		    (extern <seq>))
  (if (null? space)
      (for-each
       (lambda ((im <imported-module>))
	 (set-link-commands! (get-extern-module extern (name im))
			     (link-commands im)))
       (module-imports m))
      (begin
	(link-into* (caar space) (cdar space) (cdr space) extern)
	(link-into m-name m space extern))))

;; create a new <module> from a list of modules
;; with an export list
;; the structure of the export list is as follows:
;;    export-list ::= ((MODULE-NAME export-spec ...) ...)
;;    export-spec ::= NAME | (NEW-NAME . OLD-NAME)

(define (bind-modules (mspace <list>) (exports <list>))
  (let ((extern (make-seq))
	(modules (map cdr mspace)))
    (link-into* (caar mspace) (cdar mspace) (cdr mspace) extern)
    (make <module>
	  name: 'linked
	  link-names: (apply append (map link-names modules))
	  top-level-envt: #f
	  module-imports: (map cdr (seq->list extern))
	  module-exports: (let ((t (make-symbol-table)))
			    (for-each
			     (lambda (xfm) ;; eXports From Module
			       (process-exports-from-module!
				t
				(car xfm) ;; source module name
				(cdr (assq (car xfm) mspace))
				(cdr xfm)))
			     exports)
			    t)
	  module-classes: (apply append (map module-classes modules))
	  module-generic-functions: (apply append
					   (map module-generic-functions
						modules))
	  init-thunks: (apply append (map init-thunks modules)))))

;; 
;; this function processes the export specifications
;; in the binding of a single module
;;
;;  from-module-name/from-module describe the module
;;  that is contributing the stuff
;;

(define (process-exports-from-module! new-exports-table
				      from-module-name
				      from-module
				      export-specs)
  (let ((xt (module-exports from-module))) ;; source export table
    (for-each
     (lambda (xs)
       (bind ((src-name dst-name
			(if (symbol? xs)
			    (values xs xs)
			    (values (car xs) (cdr xs)))))
	 (if (table-lookup new-exports-table dst-name)
	     (format #t "~s: warning: ~s overrides a previous bdg for ~s\n"
		     from-module-name
		     src-name 
		     dst-name))
	 (let ((src-bdg (table-lookup xt src-name)))
	   (if src-bdg
	       (table-insert! new-exports-table dst-name src-bdg)
	       (format #t "~s: can't re-export ~s (not exported)\n"
		       from-module-name
		       src-name)))))
     export-specs)))

;;

(define (module-loaded? (m-name <symbol>))
  (and (assq m-name (installed-modules))
       #t))

(define (get-binding-from-module (m-name <symbol>) (name <symbol>))
  (table-lookup (module-exports (get-module m-name)) name))
