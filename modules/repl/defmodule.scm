#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/repl/defmodule.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.7
 | File mod date:    2004-02-24 09:38:23
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  repl
 |
 `------------------------------------------------------------------------|#


(define (com-esc-name esc-form)
  (if (null? esc-form)
      '&module
      (if (and (pair? esc-form)
	       (symbol? (car esc-form))
	       (null? (cdr esc-form)))
	  (car esc-form)
	  (error "invalid module escape clause: ~s" esc-form))))

(define (compile-module-form esc-form content-forms)
  (let ((m (make-module)))
    (add-eval-bindings! (top-level-envt m))
    (set-module-exports! m (make-symbol-table))
    (let ((ic (compile-within-module esc-form content-forms m)))
      (if (instance? ic <icode>)
	  ((wrap-tl-expr ic)))
      m)))

(define (compile-within-module esc-form content-forms m)
  (let ((esc-name (com-esc-name esc-form))
	(escaper (lambda (escape-sub-forms lxe dye mode)
		   (for-each
		    (lambda (f)
		      (do-module-escape m f lxe))
		    escape-sub-forms)
		   (make-no-values mode))))
    ;;
    (compile-with-ad-hoc-sf content-forms
			    esc-name
			    escaper
			    #t ;; required to be at top
			    (top-level-envt m)
			    (top-level-envt m)
			    'top)))


;;;
;;;    (module (&esc) defn defn ...)
;;;

(define (compile/module sf form lxe dye mode)
  (make-const (compile-module-form (cadr form) (cddr form)) mode))

;;;
;;;    (define-module foo (&esc) 
;;;       defn
;;;       defn
;;;       ...)
;;;

(define (compile-tl-define-module form lxe dye)
  (let* ((esc (caddr form))
	 (cont (cdddr form))
	 (name (cadr form))
	 (m (compile-module-form esc cont)))
    (install-module! name m)
    name))

;;;
;;;    (define-module-extend foo (&esc)
;;;       defn
;;;       defn
;;;       ...)
;;;

(define (compile-tl-module-extend form lxe dye)
  (compile-within-module (caddr form)
			 (cdddr form)
			 (get-module (cadr form))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (do-module-escape (m <module>) form envt)
  (if (pair? form)
      (case (car form)
	;;
        ((load)
         (load-1 (cadr form) envt))
	;;
	((implements)
	 (for-each
	  (lambda (fid)
	    (add-module-implements! m fid))
	  (cdr form)))
	;;
        ((export)
         (for-each
	  (let ((x (module-exports m)))
	    (lambda (n)
	      (table-insert! x n (lookup envt n))))
          (cdr form)))
        ((import use)
         (if (and (pair? (cdr form))
                  (eq? (cadr form) ':no-shared-bindings))
             (for-each
              (lambda (n)
                (use-unshared-in n (top-level-envt m)))
              (cddr form))
             (for-each
              (lambda (n)
                (use-in n (top-level-envt m)))
              (cdr form))))
        (else
         (error "invalid module control: ~s" form)))
      (error "invalid module control form: ~s" form)))

