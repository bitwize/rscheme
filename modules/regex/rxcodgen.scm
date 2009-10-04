#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/regex/rxcodgen.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.5
 | File mod date:    1997-11-29 23:10:30
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  regex
 |
 | Purpose:          regex interpreter code generator (from regex icode)
 `------------------------------------------------------------------------|#

;;
;; flat code generation from the PDA-STATE graph
;;


(define (flatten-machine start-state)
  (let (((ptr <fixnum>) 0)
	((buffer <byte-vector>) (bvec-alloc <byte-vector> 100)))
    ;;
    (define (grow)
      (let* (((old-len <fixnum>) (bvec-length buffer))
	     (n (bvec-alloc <byte-vector> (mul2 old-len))))
	(bvec-copy n 0 buffer 0 old-len)
	(set! buffer n)))
    ;;
    (define (pda-emit (val <fixnum>))
      ;(format #t " emit ~04x: ~02x\n" ptr val)
      (if (fixnum>=? ptr (bvec-length buffer))
	  (grow))
      (bvec-write-unsigned-8 buffer ptr val)
      (set! ptr (add1 ptr)))
    ;;
    (define (pda-emit-addr (addr <fixnum>))
      ;(format #t " emit ~04x: ~04x\n" ptr addr)
      (if (fixnum>=? (add1 ptr) (bvec-length buffer))
	  (grow))
      (bvec-write-unsigned-8 buffer ptr (logical-shift-right addr 8))
      (bvec-write-unsigned-8 buffer (add1 ptr) (bitwise-and addr #xFF))
      (set! ptr (add1 (add1 ptr))))
    ;;
    (define (pda-fixup-emit (p <fixnum>) (a <fixnum>))
      ;(format #t " patch ~04x: ~04x\n" p a)
      (bvec-write-unsigned-8 buffer p (logical-shift-right a 8))
      (bvec-write-unsigned-8 buffer (add1 p) (bitwise-and a #xFF))
      (values))
    ;;
    ;;   the main recursive loop
    ;;
    (define (pda-code-gen state)
      (let ((d (data state))
	    (fix '()))
	;;
	(set-addr! state ptr)
	;;
	(if (not (eq? (opcode state) $nop))
	    (begin
	      ;;
	      ;; write out the opcode
	      ;;
	      (pda-emit (opcode state))
	      ;;
	      ;; write out the data
	      ;;
	      (cond
	       ((vector? d)
		(for-each (lambda (i)
			    (pda-emit (vector-ref d i)))
			  (range (vector-length d))))
	       ((list? d)
		(let loop ((d d))
		  (if (pair? d)
		      (begin
			(if (fixnum? (car d))
			    (pda-emit (car d))
			    (if (instance? (car d) <pda-state>)
				(if (addr (car d))
				    (pda-emit-addr (addr (car d)))
				    (begin
				      (set! fix (cons (cons ptr (car d)) fix))
				      (pda-emit-addr #x5555)))
				(error "internal: bad data list element: ~s" 
				       (car d))))
			(loop (cdr d))))))
	       (else
		(error "internal: bad data type: ~s" d)))))
	;;
	;; write out the next item, if it makes sense
	;;
	(if (not (eq? (opcode state) $accept))
	    (let ((n (next state)))
	      (if n
		  (if (addr n)
		      ;; 
		      ;; it's already been written out,
		      ;; so emit a jump to it
		      ;;
		      (begin
			(pda-emit $jump)
			(pda-emit-addr (addr n)))
		      ;;
		      ;; it's not been written out.  put it right here
		      ;;
		      (pda-code-gen n))
		  ;;
		  ;; there is no following state.. reject
		  ;;
		  (pda-emit $reject))))
	;;
	;; patch up any fixups
	;;
	;; (1) write out anything that hasn't been written out yet
	;;     (ie, wasn't reached from our "next" ptr)
	;;
	(for-each (lambda (fx)
		    (if (not (addr (cdr fx)))
			(pda-code-gen (cdr fx))))
		  fix)
	;;
	;; (2) patch it all up
	;;
	(for-each (lambda (fx)
		    (pda-fixup-emit (car fx) (addr (cdr fx))))
		  fix)))
    ;;
    (pda-code-gen start-state)
    (let (((n <byte-vector>) (bvec-alloc <byte-vector> ptr)))
      (bvec-copy n 0 buffer 0 ptr)
      n)))

;;


(define (make-regex-proc name 
			 machine
			 reg-space
			 anchor?
			 as-strings?)
  (make <closure>
	template: regex-interp
	environment: (make-gvec <binding-envt> 
				'() 
				name
				machine
				reg-space
				anchor?
				as-strings?)))

;; 

(define (compile-and-make-proc rexpr as-str?)
  (bind ((first regs (compile-reg-expr rexpr)))
    (values
     (make-regex-proc rexpr
		      (flatten-machine first)
		      (* 2 (length regs))
		      (should-anchor? first)
		      as-str?)
     regs)))


(define (reg-expr->proc rexpr)
  (compile-and-make-proc rexpr #t))

(define (reg-expr->offsets-proc rexpr)
  (compile-and-make-proc rexpr #f))
