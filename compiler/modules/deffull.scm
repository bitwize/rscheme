#|------------------------------------------------------------*-Scheme-*--|
 | File:    compiler/modules/deffull.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.4
 | File mod date:    1997-11-29 23:10:28
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  (rsc)
 |
 `------------------------------------------------------------------------|#


;;
;;  define the full-procedure binding for a primop
;; 
#|
    (define-full-bdg (car (arg <pair>))
      (car arg))
|#

;; compile a top-level "define" construct

(define (compile-tl-define-full-bdg tl-def tl-envt dyn-envt)
  (let ((name (caadr tl-def))
	(formals (cdadr tl-def))
	(body (cddr tl-def)))
    (if *tl-report*
	(format #t "compiling primop's full-procedure binding: ~s\n" name))
    (let ((bdg (lookup tl-envt name)))
      (if (or (instance? (actual-bdg bdg) <primop>)
	      (instance? (actual-bdg bdg) <macro>))
	  (compile-the-full-bdg bdg name formals body tl-envt)
	  ;;
	  (error/syntax "~s: ~s is not bound to a primop or macro!" 
			(car tl-def) name)))))

;;
;; this is kind of strange.  What we actually do is
;; create a NEW primop or macro, replacing the current one in
;; this envt, which looks the same, except it has our
;; full procedure bdg
;;
;; note that this means that later modules should use
;; this module to get the right primop.  Otherwise, they'll
;; get the non-first-classable primop
;;

(define (compile-the-full-bdg bdg name formals body tl-envt)
  ;;
  (let ((tlv (make <top-level-var>
		   name: name
		   value: '#uninit
		   write-prot: #t))
	(new (clone (actual-bdg bdg))))
    ;; bind it in right away, so the full-procedure can
    ;; refer it it's own first-class value
    (bind! tl-envt new)
    (if (instance? new <primop>)
	(set-full-procedure-bdg! new tlv)
	(set-else-bdg! new tlv))
    ;;
    ;; compile the code...
    ;;
    (let* ((cc (make-code-ctx (list (list 'function-scope
					  name))))
	   (ic (compile/procedure name formals body tl-envt tl-envt))
	   (asm (procedure->aml ic '() cc))
	   (proc (make <target-closure> 
		       environment: '() 
		       template: (gen-template asm cc))))
      (set-value! tlv proc)
      #f))) ;; no initialization code
