;;;
;;;   Replace a byte-coded procedure with the equivalent dynamically
;;;   compiled C code
;;;

(define (test x)
  (cons x '()))

(define (dynamic-compile (self <template>))
  )

(define (dcg* (name <string>) (bc <byte-coded>))
  (call-with-output-string
   (lambda (port)
     (format port "#include <rscheme.h>\n")
     (format port "#include <rscheme/vinsns.h>\n")
     (format port "\n")
     ;;
     (format port "__asm__( ~s );\n" (fasm 333))
     (format port "static char FUNCTION[] = ~s;\n" name)
     (format port "jump_addr entry_point( void )\n")
     (format port "{\n~a}\n" (bytecode->ccode bc)))))

(define (dcg name (bc <byte-coded>))
  (let* ((p (dcg* name bc))
         (l (string-split p #\newline)))
    (for-each (lambda (i l)
                (format #t "~-3d: ~a\n" i l))
              (range (length l))
              l)
    p))

(define (dgcx! (tem <template>))
  (let* ((a (boxed-fn-addr (os-c-compile "/tmp/rs-sf-last/include" 
                                         (dcg "fxxx" (gvec-ref tem 3))
                                         "bc_333"
                                         cgateway/0)))
         (hi (bvec-read-unsigned-16 a 2))
         (lo (bvec-read-unsigned-16 a 0)))
    (gvec-set! tem 0 (obj-bits-reform hi lo))
    (values)))

(define (tx)
  (dgcx! (template test)))

(define (fasm k)
  (call-with-output-string
   (lambda (port)
     (format port ".align 4\n")
     (format port "   .byte 0xEE\n")
     (format port "   .byte 0xFF\n")
     (format port "   .byte 0xCC\n")
     (format port "   .byte 0xAA\n")
     (format port "   .globl bc_~d\n" k)
     (format port "bc_~d:\n" k)
     (format port "    jmp entry_point\n"))))

;;;

,(use tables)

(define *bytecode-reverse-insn* (make-fixnum-table))

(define-class <generation-context> (<object>)
  (stack init-value: '())
  (max-depth init-value: 0))

(define (gstack-push (ctx <generation-context>) type)
  (let* ((n (length (stack ctx)))
         (l (~ "t~d.~a_val" n type)))
    (if (> (+ n 1) (max-depth ctx))
        (set-max-depth! ctx (+ n 1)))
    (set-stack! ctx (cons (list l type) (stack ctx)))
    l))

(define (bytecode->ccode (bc <byte-coded>))
  (let ((port (open-output-string))
        (ctx (make <generation-context>)))
    ;;
    (define (preamble)
      (call-with-output-string
       (lambda (p)
         (format p "/* max depth ~d */\n" (max-depth ctx))
         (for-each 
          (lambda (i)
            (format p "RS_bc_datum t~d;\n" i))
          (range (max-depth ctx)))
         (newline p))))
    ;;
    (let loop ((i 0))
      (if (eq? i (bvec-length bc))
          (string-append (preamble) (get-output-string port))
          (let ((f (table-lookup *bytecode-reverse-insn* (bvec-ref bc i))))
            (if (not f)
                (error "Bytecode opcode #x~x not supported" 
                       (bvec-ref bc i)))
            (loop (f bc i port ctx)))))))

;;;

(define-macro (declare-opcode (code port ctx) . body)
  `(table-insert!
    *bytecode-reverse-insn*
    ,code
    (lambda (%bc %i ,port (,ctx <generation-context>))
      (let-syntax ((ref (syntax-form (j)
                          (bvec-ref %bc (fixnum+ %i j))))
                   (push (syntax-form (type)
                           (gstack-push ,ctx type)))
                   (insn-size (syntax-form (n)
                                (+ %i n)))
                   (pop (syntax-form (type)
                          (let ((top (car (stack ,ctx))))
                            (assert (eq? (cadr top) type))
                            (set-stack! ,ctx (cdr (stack ,ctx)))
                            (car top)))))
        ,@body))))

(declare-opcode
 (#x82 port ctx)
 (format port "  COUNT_ARGS(1);\n")
 (insn-size 1))

(declare-opcode
 (#x65 port ctx)
 (format port "  ~a = NIL_OBJ;\n" (push 'obj))
 (insn-size 1))

(declare-opcode
 (#x14 port ctx)
 (format port "  REG1 = ~a;\n" (pop 'obj))
 (insn-size 1))

(declare-opcode
 (#xBA port ctx)
 (format port "  ~a = TLREFB(~d);\n" (push 'obj) (+ 1 0))
 (insn-size 1))

(declare-opcode
 (#xB2 port ctx)
 (format port "  APPLYF( ~d, ~a );\n" 2 (pop 'obj))
 (insn-size 1))

; reg-ref
; cons
; reg-set

(define *bc-insn*
  (call-with-input-file
      "~/p/sf/rscheme-base/src/install/resource/compiler/bytecode/bctable.dat" 
    read))

#|
(define (declare1 bci)
  (let ((opcode (car bci))
        (aml (cadr bci))
        (args (caddr bci))
        (len (cadddr bci)))
    ;;
    ;;
    (case (car aml)
      ((tl-ref/bound)
|#     

(define (arg-collectors args)
  (let loop ((a args)
             (i 0)
             (r '()))
    (if (null? a)
        (reverse! r)
        (let ((a1 (car a)))
          (if (symbol? a1)
              
