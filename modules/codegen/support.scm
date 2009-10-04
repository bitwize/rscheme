#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/codegen/support.scm
 |
 |          Copyright (C)1997, 2001 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.12
 | File mod date:    2003-07-17 11:42:36
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  codegen
 |
 | Purpose:          bytecode code generator support library
 `------------------------------------------------------------------------|#

;; bytecode code generator support library

(define-thread-var *current-bc-accum*)

(define-class <byte-code-accum> (<object>)
  (label-table init-value: '() :sealed)
  (patch-table init-value: '() :sealed)
  (current-pc type: <fixnum> init-value: 0 :sealed)
  (current-buffer type: <byte-vector> :sealed))

;;;
;;;  An intersting observation:  Out of 3051 instances of <byte-coded>
;;;  in my current heap image, only 10% are longer than 150, and
;;;  only 1% are longer than 600.
;;;

(define (make-byte-code-accum)
  (make <byte-code-accum>
        current-buffer: (bvec-alloc <byte-vector> 200)))

;;;

(define (get-current-pc) (current-pc *current-bc-accum*))

(define *debug-byte-codes* #f)

(define (emit-byte-code b)
  (let* (((bc <byte-code-accum>) *current-bc-accum*)
         ((k <fixnum>) (current-pc bc)))
    (if *debug-byte-codes*
        (format #t "[~d] bytecode ~d\n" (current-pc bc) b))
    (if (>= k (bvec-length (current-buffer bc)))
        (let ((t (bvec-alloc <byte-vector> 
                             (mul2 (bvec-length (current-buffer bc))))))
          (bvec-copy t 0 (current-buffer bc) 0 k)
          (set-current-buffer! bc t)))
    (bvec-set! (current-buffer bc) k b)
    (set-current-pc! bc (add1 k))))

(define (flush-byte-codes (bc <byte-code-accum>))
  ;; patch up references to labels
  (let (((buf <byte-vector>) (current-buffer bc)))
    (for-each
     (lambda (p)
       (let ((def (get-label (car p) (label-table bc)))
             (i (cdr p)))
         (bvec-set! buf i (logical-shift-right def 8))
         (bvec-set! buf (+ i 1) (bitwise-and def #xFF))
         i))
     (patch-table bc))
    ;;
    (let ((bcp (bvec-alloc <byte-coded> (current-pc bc))))
      (bvec-copy bcp 0 (current-buffer bc) 0 (current-pc bc))
      bcp)))

(define (def-label l)
  (let (((bc <byte-code-accum>) *current-bc-accum*))
    (if *debug-byte-codes*
        (format #t "[~d] definining label: ~s\n" (current-pc bc) l))
    (set-label-table! bc (cons (cons l (current-pc bc)) (label-table bc)))))
    
(define (ref-label l)
  (let (((bc <byte-code-accum>) *current-bc-accum*))
    (if *debug-byte-codes*
        (format #t "[~d] referencing label: ~s\n" (current-pc bc) l))
    (set-patch-table! bc (cons (cons l (current-pc bc)) (patch-table bc)))
    ;; return 0 to be inserted into bc stream for now
    0))

;;;====================================================================

(define (emit-byte-code-check b)
    (assert (and (fixnum? b)
    	         (>= b 0)
		 (< b 256)))
    (emit-byte-code b))

(define (emit-byte-code-16-check b)
    (assert (and (fixnum? b)
    	         (>= b 0)
		 (< b 65536)))
    (emit-byte-code (logical-shift-right b 8))
    (emit-byte-code (bitwise-and b #xFF)))

(define (emit-byte-code-s16-check b)
    (assert (and (fixnum? b)
    	         (>= b -32768)
		 (< b 32768)))
    (emit-byte-code (bitwise-and (logical-shift-right b 8) #xFF))
    (emit-byte-code (bitwise-and b #xFF)))

(define (emit-byte-code-s32 b)
    (emit-byte-code (bitwise-and (logical-shift-right b 24) #xFF))
    (emit-byte-code (bitwise-and (logical-shift-right b 16) #xFF))
    (emit-byte-code (bitwise-and (logical-shift-right b 8) #xFF))
    (emit-byte-code (bitwise-and b #xFF)))

(define (emit-primop primop-bdg num-args)
  (let ((b (assq 'bytecode (translations (actual-bdg primop-bdg)))))
    (if b
	(let ((op (cdr b)))
	  (if (pair? op)
	      ;; extension primop
	      (begin
		(emit-byte-code 254)
		(emit-byte-code-check (car op))
		(emit-byte-code-check (cdr op)))
	      (if (symbol? op)
		  ;; special primop
		  (emit-special-primop op num-args)
		  ;; normal primop
		  (begin
		    (emit-byte-code 255)
		    (emit-byte-code-check op)))))
	(error/internal
	 "primop cannot be rendered in bytecode: ~s"
	 primop-bdg))))


(define (get-label label table)
  (let ((e (assq label table)))
    (if e
	(cdr e)
	(error "get-label: label `~s' somehow not defined"
	       label))))


(define (aml->byte-coded aml)
  (if *debug-byte-codes*
      (begin
	(display (make-string 60 #\-))
	(newline)))
  (let ((ctx (make-byte-code-accum)))
    (thread-let ((*current-bc-accum* ctx))
      (for-each compile-aml-stmt aml)
      (flush-byte-codes ctx))))

(define-syntax (kget table key)
  (let ((e (table-lookup table key)))
    (if e
	e
	(abort "kget: key `~s' not in table `~s'" key (mquote table)))))
