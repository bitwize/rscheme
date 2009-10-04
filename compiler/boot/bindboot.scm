#|------------------------------------------------------------*-Scheme-*--|
 | File:    compiler/boot/bindboot.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.8
 | File mod date:    1998-08-29 19:45:08
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  (rsc)
 |
 `------------------------------------------------------------------------|#



(define (build-boot-module modules)
  ;; get-module now binds the module to it's imports
  (let ((ms (map get-ct-module modules)))
    ;;
    (finalize-boot-module
     (make <module>
	   name: '*boot*
	   link-names: (apply append (map link-names ms))
	   top-level-envt: #f
	   module-imports: (map (lambda (name module)
				  (make <imported-module>
					name: name
					owner: #f
					actual-module: module))
				modules
				ms)
	   module-exports: (let ((t (make-symbol-table)))
			     (for-each
			      (lambda (cm) ; component module
				(table-for-each
				 (module-exports cm)
				 (lambda (h k v)
				   (if (instance? v <special-form>)
				       (set-compiler-proc! v #f))
				   (table-insert! t k v))))
			      ms)
			     t)
	   module-classes: (apply append (map module-classes ms))
	   module-generic-functions: (apply append
					    (map module-generic-functions ms))
	   module-implicit-methods: (apply append
					   (map module-implicit-methods ms))
	   first-init-thunks: (apply append (map first-init-thunks ms))
	   init-thunks: (apply append (map init-thunks ms))))))



(define (finalize-boot-module m)
  ;; finalize the classes and gfs
  (format #t "finalizing ~d classes...\n" (length (module-classes m)))
  (for-each (lambda (cl) 
	      (link-time-patch-class cl m))
	    (module-classes m))
  ;;
  (format #t "finalizing ~d generic functions...\n"
	  (length (module-generic-functions m)))
  (for-each (lambda (gf) 
	      (link-time-patch-generic-function gf m))
	    (module-generic-functions m))
  ;;
  (format #t "finalizing ~d methods...\n" 
	  (length (module-implicit-methods m)))
  (for-each (lambda (meth)
	      (link-time-patch-implicit-methods meth m))
	    (module-implicit-methods m))
  ;;
  m)

(define (link-time-patch-generic-function (gf <target-gf1>) (m <module>))
  ;(format #t "finalizing generic function: ~s\n" (gf-name gf))
  (let ((dispatcher (table-lookup (module-exports m) 
				  'generic-function-dispatch)))
    (if dispatcher
	(set-template! gf (value dispatcher))
	(abort 'finalize-generic-function 
	       "no binding for generic-function-dispatch"))))

;; the head of the list is a name identifying the form
;; of the implicit method

(define (link-time-patch-implicit-methods (methods <list>) (m <module>))
  (let ((bdg (table-lookup (module-exports m) (car methods))))
    (if bdg
	(let ((template (value bdg)))
	  (for-each (lambda ((meth <target-method>))
		      (set-template! meth template))
		    (cdr methods)))
	(abort 'finalize-implicit-methods
	       "no binding for ~s" (car methods)))))

(define (link-time-patch-class (c <<target-class>>) (m <module>))
  (set-class-precedence-list! c (tclass-precedence-list c)))

