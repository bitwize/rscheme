#|------------------------------------------------------------*-Scheme-*--|
 | File:    compiler/c.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.24
 | File mod date:    2004-01-12 14:29:48
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  (rsc)
 |
 `------------------------------------------------------------------------|#

;;; forward decls

(define-generic-function c-name)

;;; Modular Compiler

,(use paths tables)

;;; load mlink facility

;;; first, provide a definition for `resolve-module-reference'
;;  that will not automatically bring in dependent modules.

(define (resolve-module-reference (target-name <symbol>) (space <list>))
  (let ((b (assq target-name space)))
    (if b
        (cdr b)
        #f)))

(load "../modules/mlink/seq.scm")
(load "../modules/mlink/mifio.scm")
(load "../modules/mlink/modules.scm")
(load "../modules/mlink/linkcmds.scm")
(load "../modules/mlink/patch.scm")
(load "../modules/mlink/linker.scm")
(load "../modules/mlink/linkload.scm")
(load "../modules/mlink/mpath.scm")

(mifio-class "<byte-coded>" <byte-coded>)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  load our own definitions of some classes and stuff

(load "target.scm")

(define (vector->values v)
  (list->values (vector->list v)))

(define (add-leadin! foo bar)
  ; ignore this %early-once-only in compiler/cmplproc.scm
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  load the compiler

(load "../modules/compiler/compiler.scm")

(define-class <definer> (<special-form>))

(add-mifio-class "<definer>" <definer>)
(add-mifio-class "<substitution>" <substitution>)
(add-mifio-class "<compile-point>" <compile-point>)



;;----------------------------------------------------------------

;; collect initialization procedures all together
 
(define $init-thunks '())

(define-syntax (on-startup . body)
  (set! $init-thunks (append $init-thunks
			     (list (lambda () (begin . body))))))

(load "misc.scm")

;; some other stuff

(define-generic-function link-name)

;;----------------------------------------------------------------
;;  bytecode generation

(load "../modules/codegen/genaml.scm")
(load "../modules/codegen/loops.scm")
(load "../modules/codegen/lexaddr.scm")
(load "../modules/codegen/aml2bc.scm")
(load "../modules/codegen/support.scm")
(load "../modules/codegen/rearrnge.scm")
(load "../modules/codegen/parassmt.scm")

;;----------------------------------------------------------------
;; C-code generation

(load "cgen/assem.scm")
(load "cgen/estsize.scm")
(load "cgen/wrvinsn.scm")

;;----------------------------------------------------------------

;; these should be thread variables if the compiler is to run in a thread

(define *current-module* #f)
(define *build-context* #f)


;;; note:  using the bytecode strategy causes an additional
;;;        word in the template to appear after the code-properties
;;;        but before the literals (it's the bytecode bvec itself)
;;;        [but why say that here?]

(load "modules/imported.scm")
(load "config.scm")

(load "configv.scm")
(load "util/resource.scm")

(load "modules/bldctx.scm")
(load "cgen/parts.scm")
(load "modules/mcfload.scm")
(load "modules/document.scm")
(load "modules/use.scm")
(load "modules/findmodl.scm")
(load "modules/findhier.scm")

(load "cgen/bldfiles.scm")

(load "util/util.scm")
(load "toplevel/builtin.scm")

(load "toplevel/compileclass.scm")

(load "boot/makeinit.scm")
(load "boot/bootable.scm")
(load "util/config-release.scm")

(load "util/genbcode.scm")
(load "amlstubs.scm")

(add-special-form-compiler! top-level-compiler->proc)
(add-special-form-compiler! objsys-compiler->proc)

(define (configure-basis-from-modules m-names)
  (set-compiler-basis-generator! 
   (lambda ()
     (let* ((envt (make-top-level-contour))
            (tbl (table envt)))
       (for-each
        (lambda (mn)
          (let* ((rawm (get-ct-module mn))
                 (src (module->imported-module rawm))
                 (t (table (top-level-envt rawm))))
            ;;
            (for-each
             (lambda (binding-name)
               (if (not (table-lookup tbl binding-name))
                   (let ((b (lookup-from-imported-module src 
                                                         binding-name
                                                         *current-module*)))
                     (table-insert! tbl binding-name b))))
             (key-sequence t))))
        m-names)
       ;;
       envt))))


(for-each 
 (lambda (thunk) (thunk))
 $init-thunks)

