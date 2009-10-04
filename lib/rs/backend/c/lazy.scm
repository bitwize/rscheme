#|------------------------------------------------------------*-Scheme-*--|
 | File:	    rs/backend/c/lazy.scm
 |
 |          Copyright (C)1998 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |	    See <http://www.rscheme.org/> for the latest info.
 |
 | File version:     1.2
 | File mod date:    2005-02-18 15:58:16
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  rs.backend.c
 |
 | Purpose:          Support lazy flushing of code to a ldso
 `------------------------------------------------------------------------|#

(define *code-accumulation* '())

(define-syntax (ifdebug expr) (values))

(define (accumulate-code! cd)
  (ifdebug
   (format #t "will flush later: ~s ~s\n" (function-scope cd) (template cd)))
  (set! *code-accumulation* (append *code-accumulation* (list cd)))
  (values))

(define (flush-lazy-and-call trap-args trap-proc)
  (if (with-module repl *compile-verbose*)
      (format #t "flushing all ~d procs due to trap in: ~s\n"
	      (length *code-accumulation*)
	      trap-proc))
  (flush-all-code)
  (ifdebug
   (format #t "continuing trapped code: ~s\n" trap-proc))
  (apply trap-proc trap-args))

(set-flush-and-call-proc! flush-lazy-and-call) ;; install hook from corelib

(define (flush-all-code)
  (ifdebug
   (format #t "flushing ~d procs\n" (length *code-accumulation*)))
  (let ((lst *code-accumulation*))
    (set! *code-accumulation* '())
    (if (pair? lst)
	(compile-and-load lst))))

(with-module
    repl
  (add-image-save-hook! flush-all-code))

;;;  this was the identity procedure, but now that `lazy-flush-stub'
;;;  is loaded, we can manufacture trampoline templates

(define (make-trampoline-template real-template)
  (let ((tramp (clone lazy-flush-trampoline)))
    (gvec-set! tramp 2 (gvec-ref real-template 2))
    (gvec-set! tramp 4 real-template)
    (ifdebug
     (format #t "trampoline ~s\n\t\tfor: ~s\n" tramp real-template))
    tramp))

(define (default-policy-ccode? asm pt fs)
  (cond
   ((not (vector? pt))                   #f)
   ((not (> (vector-length pt) 0))       #f)
   ((not (eq? (vector-ref pt 0) 'file))  #f)
   ((equal? fs '(eval))                  #f)
   ((and (pair? fs)
	 (eq? (car fs) 'rewriter))       #f)
   ((not
     (every? has-ccode-impl? 
	     (collect-primop-refs asm))) #f)
   (else
    #t)))

(define (has-ccode-impl? primop)
  (and (assq 'ccode (translations primop)) #t))

(define (lazy-aml->template* asm cc)
  (let* ((src-pt (fluid-ref *source-point*))
	 (props (append (code-ctx-properties cc)
			`((location ,(vector-ref src-pt 2)
				    ,(vector-ref src-pt 1)))))
	 (the-template (make-gvec* <template> 0 'deferred
				   props
				   (code-ctx-literals cc))))
    (accumulate-code!
     (make <code-descriptor>
	   template: the-template
	   properties: props
	   strategy: 'ccode
	   code: asm))
    ;;
    (make-trampoline-template the-template)))

(define (lazy-aml->template asm cc)
  (if (every? has-ccode-impl? (collect-primop-refs asm))
      (lazy-aml->template* asm cc)
      (default-aml->template asm cc)))

(add-codegen-implementation! 'lazy lazy-aml->template)

;;;
  
(define (cload-into envt . files)
  (thread-let ((*codegen-policy* 'ccode))
    (with-module repl
      (apply load-into envt files))))

(define-macro (cload . files)
  `(cload-into *self* ,@files))

