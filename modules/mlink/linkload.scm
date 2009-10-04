#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/mlink/linkload.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.17
 | File mod date:    2006-10-01 11:30:27
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  mlink
 |
 | Purpose:          load and bind modules
 `------------------------------------------------------------------------|#

;;
;;  dynamically bind a module
;;

(define *boot-modules* '()) ;; filled in at boot image linkage time
(define *space* #f)

(define (installed-modules)
  (or *space* *boot-modules*))

(define (install-module! name module)
  (set! *space* (cons (cons name module) (installed-modules))))

;;
;;  `link-load-module' is only used to load modules for execution
;;  if you are doing different things (like the to-C-compiler), then
;;  you would use load-module directly, because you need to manage
;;  your own module namespace and not finalize/patch the loaded modules
;;

(define (link-load-module name path)
  (let ((m (load-module name path #t)))
    (link-into name m (installed-modules) #f)
    (for-each finalize-class (module-classes m))
    (for-each finalize-generic-function (module-generic-functions m))
    (for-each (lambda (method-set)
		(patch-implicit-methods (cdr method-set)
					(car method-set)))
	      (module-implicit-methods m))
    (install-module! name m)
    ;;
    ;; note that we execute `first-init-thunks' immediately,
    ;; but queue the regular `init-thunks' for use when the
    ;; system is restarted
    ;;
    (for-each (lambda (thunk)
		(thunk))
	      (first-init-thunks m))
    (set-first-init-thunks! m '()) ; "nil out the pointer"
    ;;
    (let ((startup-thunks (vector-ref (rscheme-global-ref 0) 3)))
      (append! startup-thunks (init-thunks m)))
    m))

;; most bindings issue a warning and use the new one

(define-method resolve-import-conflict ((self <binding>)
					(new-bdg <binding>)
					(name <symbol>)
					(tle <top-level-contour>))
  (if (not (eq? self new-bdg))
      (format #t "warning: ~s already bound to ~s\n" name self))
  (table-insert! (table tle) name new-bdg))

(define-method resolve-import-conflict ((self <top-level-var>)
					(new-bdg <binding>)
					(name <symbol>)
					(tle <top-level-contour>))
  (if (instance? new-bdg <top-level-var>)
      (if (write-prot self)
          (if (not (eq? (value self) (value new-bdg)))
              (begin
                (format #t "warning: TLV ~s already bound (rebinding to new one)\n" 
                        name)
                (table-insert! (table tle) name new-bdg)))
          (begin
            (if (not (or (eq? (value self) '#unbound)
                         (eq? (value self) (value new-bdg))))
                (format #t "warning: TLV ~s already bound to ~s\n" 
                        name (value self)))
            (set-value! self (value new-bdg))))
      (begin
	(if (not (eq? self new-bdg))
	    (format #t "warning: ~s already bound to ~s = ~s\n" 
		    name self (value self)))
	(table-insert! (table tle) name new-bdg))))

;;

(define-method make-unshared-binding ((self <binding>))
  ;; by default, don't copy -- only variables are copied
  self)

(define-method make-unshared-binding ((self <top-level-var>))
  (let ((c (clone self)))
    (set-write-prot! c #f)
    c))

;;

(define (use-module-in mname (m <module>) envt 
                       #optional (shared? default: #t))
  (let ((t (table envt)))
    (set-dirty?! envt #t)
    (table-for-each
     (module-exports m)
     (lambda (h k v)
       (let ((b (table-lookup t k))
             (v (if shared?
                    v
                    (make-unshared-binding v))))
	 (if b
	     (resolve-import-conflict b v k envt)
	     (begin
	       (table-install! t h k v)
	       (values))))))
    ;; merge the `implements' list if the given
    ;; environment has an associated owner that is
    ;; a module
    (if (instance? (owner envt) <module>)
	(let ((dest (owner envt)))
	  (for-each
	   (lambda (fid)
	     (add-module-implements! dest fid))
	   (get-module-implements m))))
    ;;
    (for-each
     (lambda (hook)
       (hook envt))
     (usage-hooks m))
    (values)))

(define (use-in name envt)
  (if (eq? name 'all)
      (for-each (lambda (ent)
		  (use-module-in (car ent) (cdr ent) envt))
		(installed-modules))
      (use-module-in name (get-module name) envt)))

(define (use-unshared-in name envt)
  (use-module-in name (get-module name) envt #f))
