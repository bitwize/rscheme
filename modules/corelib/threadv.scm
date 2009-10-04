#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/corelib/threadv.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.6
 | File mod date:    2003-06-22 18:15:03
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  corelib
 |
 `------------------------------------------------------------------------|#

;;;
;;;  the intended use of the direct thread variables
;;;  is for:
;;;    direct: 1 => *input-port*
;;;    direct: 2 => *output-port*
;;;    direct: 3 => *hander-chain*
;;;

(define-syntax update-thread-state!
  (syntax-form (basis)
    (values))
  (syntax-form (basis ('*input-port* expr) . rest)
    (gvec-set! basis 1 expr)
    (update-thread-state! basis . rest))
  (syntax-form (basis ('*output-port* expr) . rest)
    (gvec-set! basis 2 expr)
    (update-thread-state! basis . rest))
  (syntax-form (basis ('*handler-chain* expr) . rest)
    (gvec-set! basis 3 expr)
    (update-thread-state! basis . rest))
  (syntax-form (basis (name expr) . rest)
    (gvec-set! basis 0 (make-gvec <vector> 
                                  (mquote name) 
                                  expr
                                  (gvec-ref basis 0)))
    (update-thread-state! basis . rest)))

(define (make-empty-thread-vars)
  (make-gvec <vector> $thread-var-end #f #f #f))

(%early-once-only
(define-constant $thread-var-end '#())
(define *thread-var-prototype* (make-empty-thread-vars))
(define *direct-names* '#(*input-port* *output-port* *handler-chain*))
(define *thread-var-init-values* $thread-var-end)
)
(define (thread-var-initial-state)
  (clone *thread-var-prototype*))

(define (set-thread-var-initial-state! state)
  (set! *thread-var-prototype* state))
  
(define (add-thread-var! name init)
  (set! *thread-var-init-values*
	(make-gvec <vector> 
		   name
		   init
		   *thread-var-init-values*)))

(define (set-direct-thread-init! index init)
  (vector-set! *thread-var-prototype* index init))

(define-syntax (direct-thread-var-ref index)
  (gvec-ref (get-thread-state-reg) index))

;;;

(define-syntax (indirect-thread-var-apply name proc)
  (let lloop (((p <vector>) (gvec-ref (get-thread-state-reg) 0)))
    (if (eq? p $thread-var-end)
        (let sloop (((p <vector>) *thread-var-init-values*))
          (if (eq? p $thread-var-end)
              (error "indirect thread var not found: ~s" name)
              (if (eq? (gvec-ref p 0) name)
                  (proc p)
                  (sloop (gvec-ref p 2)))))
        (if (eq? (gvec-ref p 0) name)
            (proc p)
            (lloop (gvec-ref p 2))))))

(define (indirect-thread-var-ref name)
  (indirect-thread-var-apply name (lambda (p) (gvec-ref p 1)))) 

(define (indirect-thread-var-set! name val)
  (indirect-thread-var-apply name 
     (lambda (p) 
        (let ((o (gvec-ref p 1)))
           (gvec-set! p 1 val)
           o))))

;;;

(define-macro define-thread-var 
  (macro-rules ()
   ((_ name)
    `(define-thread-var ,name #f))
   ;
   ((_ name init direct: k)
    `(begin
       ; direct vars are immutable, but they can be init'ed
       (%early-once-only
	(set-direct-thread-init! ,k ,init))
       (define-syntax ,name
	 (else
	  (direct-thread-var-ref ,k)))))
   ;
   ((_ name init :indirect)
    `(begin
       (%early-once-only (add-thread-var! ',name ,init))
       (define-syntax ,name
	 (setter-form (val)
	  (indirect-thread-var-set! ',name val))
	 (else
	  (indirect-thread-var-ref ',name)))))
   ;
   ((_ name init)
    (let* ((direct-spot (memq name '(*input-port*
				     *output-port*
				     *handler-chain*))))
      (if direct-spot
	  `(define-thread-var ,name ,init
	     direct: ,(- 4 (length direct-spot)))
	  `(define-thread-var ,name ,init :indirect))))))
   
;;;

(define-macro (thread-let bdgs . body)
  (if (null? bdgs)
      `(let () ,@body)
      (let ((n-save (gensym))
	    (n (gensym)))
	(define (make-new-thread-state bdgs saved-ts)
	  `(make-gvec 
	    <vector>
	    ,(accum-indir-changes bdgs saved-ts)
	    ,@(map (lambda (i)
		     (let ((b (assq (vector-ref '#(*input-port*
						   *output-port*
						   *handler-chain*)
						i)
				    bdgs)))
		       (if b
			   (cadr b)
			   `(gvec-ref ,saved-ts ,(+ i 1)))))
		   (range 3))))
	(define (accum-indir-changes bdgs saved-ts)
	  (if (null? bdgs)
	      `(gvec-ref ,saved-ts 0)
	      (if (memq (caar bdgs) '(*input-port* 
				      *output-port*
				      *handler-chain*))
		  (accum-indir-changes (cdr bdgs) saved-ts)
		  `(make-gvec <vector>
			      ',(caar bdgs)
			      ,(cadar bdgs)
			      ,(accum-indir-changes (cdr bdgs) saved-ts)))))
	;
	`(dynamic-call-thunk
	  #f
	  #f
	  (lambda () ,@body)
	  (get-dynamic-state-reg)
	  (let ((,n-save (get-thread-state-reg)))
	    ,(make-new-thread-state bdgs n-save))))))
