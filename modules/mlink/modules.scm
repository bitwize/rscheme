#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/mlink/modules.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.13
 | File mod date:    1999-01-13 09:33:26
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  mlink
 |
 | Purpose:          Module class definitions
 `------------------------------------------------------------------------|#

#|
The link-names of a module is the list of names by which its component
module descriptors (struct module_descr) are known.  descriptor is
known.  That is, if a link-name is "foo", then the <module> uses a
section of code with a module descriptor named "module_foo" This is
used to support dynamic linking.  Each link-name is also the name
stored in the module_descr.  If no C code/data is required to support
this module, then link-names is ()
|#

(define-class <module> (<object>)
  (name type: <symbol>)      ;; debug name
  (properties type: <list> init-value: '())
  ;
  (link-names init-value: '() 
	      type: <list>) ;; list of C entries that form the module
  ;; (they get accumulated when linking up)
  ;; () if no C context is required/available
  (proj-names init-value: '() 
	      type: <list>) ;; list of C subprojects that form the module
  top-level-envt          ;; may be #f if not published
  (module-imports init-value: '()
		  type: <list>)  ;; a list of <imported-module>'s
#|
A future implemention should support these, a refinement of the
current solution to CR 208 for lazy binding importing...

  (module-dependencies init-value: '()
		       type: <list>)  ;; a list of <imported-module>'s
  (modules-used init-value: '()
		type: <list>)
|#
  (module-exports init-value: '#uninit 
		  type: <table>)
  (module-shares init-value: '() 
		 type: <list>)  ;; list of shared exports
  (module-permits-writing init-value: '()
			  type: <list>) ;; list of writable exports
  (module-classes init-value: '() 
		  type: <list>)   ;; classes defined by this module
  (module-generic-functions init-value: '() 
			    type: <list>)  ;; GFs built in here
  (module-implicit-methods init-value: '() 
			   type: <list>)   ;; getters & setters
  (module-bytecode-extensions init-value: '() 
			      type: <list>)  ;; bytecode extensions
  (init-thunks init-value: '())       ;; initialization thunks
  (first-init-thunks init-value: '()) ;; first-time initialization thunks
  ;; these hooks are called in-order at "use" time, with
  ;; a single argument which the environment using this module
  (usage-hooks init-value: '() type: <list>))  ;; `use'-time procs

(define-class <imported-module> (<object>)
  owner    ;; the module we belong to
  actual-module ;; the actual module we represent
  (name init-value: '#uninit type: <symbol>)
  ;; a list of <link-cmd> general instances
  ;; which are processed, at link time, in-order
  ;; so <link-bdgs> is first, followed by any <link-value>'s,
  ;; followed by any others
  (link-commands init-value: '() type: <list>))

(mifio-class "<module>" <module>)
(mifio-class "<imported-module>" <imported-module>)
(mifio-class "<symbol-table>" <symbol-table>)

;;;  add a feature id, `fid', to the list of features
;;;  a module advertises to implement

(define (add-module-implements! (m <module>) fid)
  (let* ((a (properties m))
	 (e (assq 'implements a)))
    (if e
	(if (not (memq fid (cdr e)))
	    (set-cdr! e (cons fid (cdr e))))
	(set-properties! m (cons (list 'implements fid) a)))
    (values)))

(define (get-module-implements (m <module>))
  (let ((e (assq 'implements (properties m))))
    (if e
	(cdr e)
	'())))
