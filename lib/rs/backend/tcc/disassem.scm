,(use paths
      tables
      sort)

(define (extract-byte-code-u16 (bc <byte-coded>) (i <fixnum>))
  (bitwise-or
   (logical-shift-left (bvec-ref bc i) 8)
   (bvec-ref bc (add1 i))))

(define (extract-byte-code-s16 (bc <byte-coded>) (i <fixnum>))
  (let (((hi <fixnum>) (bvec-ref bc i))
        ((lo <fixnum>) (bvec-ref bc (add1 i))))
    (if (eq? (bitwise-and hi #x80) 0)
        (bitwise-or (logical-shift-left hi 8) lo)
        (- (bitwise-or (logical-shift-left hi 8) lo) #x10000))))

(define-safe-glue (extract-byte-code-s32 (bc <byte-coded>) (i <raw-int>))
{
  UINT_8 *pc = ((UINT_8 *)PTR_TO_DATAPTR( bc )) + i;
  int a;

  a = (pc[0]<<24) + (pc[1]<<16) + (pc[2] << 8) + pc[3];
  REG0 = int2fx( a );
  RETURN1();
})


(define (load-primop-vector)
  (let ((p (string->file "[resource]/compiler/bytecode/potable.dat")))
    (if (file-exists? p)
        (for-each
         (lambda (primop)
           (vector-set! *bc-primops*
                        (cdr (assq 'bytecode (vector-ref primop 3)))
                        primop))
         (with-input-from-file (pathname->os-path p) read)))))

(define *bytecode-disassem* (make-vector 256 #f))
(define *bc-primops* (make-vector 256 #f))
(define *bc-extensions* (make-vector 256 #f))

(define-macro (declare-dynamic-compiler (code bc i port) . body)
  '(values))
  
(define-macro (declare-opcode (code bc i) . body)
  `(vector-set!
    *bytecode-disassem*
    ,code
    (lambda ((,bc <byte-coded>) (,i <fixnum>))
      (let-syntax ((ref (syntax-form (kk)
                          (bvec-ref ,bc (fixnum+ ,i kk))))
                   (insn-size (syntax-form (nn)
                                (fixnum+ ,i nn))))
        ,@body))))

(load "/tmp/a.scm")

(declare-opcode 
 (#xFE bc i)
 ;; Extension primop
 (let* ((extn (ref 1))
        (f (or (vector-ref *bc-extensions* extn)
               (error "bytecode extension #x~02x not loaded" extn))))
   (f bc i)))

(declare-opcode 
 (#xFF bc i)
 (let* ((primop (ref 1))
        (info (or (vector-ref *bc-primops* primop)
                  (error "bytecode primop #x~02x not defined" primop)))
        (name (vector-ref info 0))
        (args (vector-ref info 1))
        (ret (vector-ref info 2)))
   (values `(primop ,name ,args ,ret)
           (insn-size 2))))
   

(define-method dis ((self <byte-coded>))
  (let ((q (make-dequeue))
        (lbl (make-fixnum-table)))
    (let loop ((i 0))
      (if (fixnum>=? i (bvec-length self))
          (values (dequeue-state q) lbl)
          (bind ((f (or (vector-ref *bytecode-disassem* (bvec-ref self i))
                        (error "bytecode opcode #x~02x not handled"
                               (bvec-ref self i))))
                 (insn i2 (f self i)))
            ;;
            (case (car insn)
              ((branch-if-false)
               (table-insert! lbl (cadr insn) '(internal)))
              ((jump)
               (table-insert! lbl (cadr insn) '(jump)))
              ((bjump)
               (table-insert! lbl (caddr insn) (list 'bjump (cadr insn))))
              ((save)
               (table-insert! lbl (caddr insn) (list 'save (cadr insn)))))
            ;;
            (dequeue-push-back! q (cons* i (- i2 i) insn))
            (loop i2))))))

(define-method dis ((self <template>))
  (dis (gvec-ref self 3)))

(define-method dis ((self <function>))
  (dis (gvec-ref self 0)))

(define-method print ((self <template>))
  (bind ((bc (gvec-ref self 3))
         (insn-vec labels (dis bc))
         (symlabel (make-fixnum-table)))
    ;;
    (for-each
     (lambda (j k)
       (table-insert! 
        symlabel 
        k
        (case (car (table-lookup labels k))
          ((internal) (~ "c~d" j))
          ((jump) (~ "cx~d" j))
          ((bjump) (~ "loop~d" j))
          ((save) (~ "save~d" j))
          (else (~ "l~d" j)))))
     (range (table-size labels))
     (sort (key-sequence labels) <))
    ;;
    (table-insert! symlabel 0 "start")
    ;;
    (vector-for-each/i
     (lambda (i item)
       (bind ((aux "")
              (pc (car item))
              (n (cadr item))
              (label skip (if (table-key-present? symlabel pc)
                              (values
                               (table-lookup symlabel pc)
                               (and
                                (not (eq? pc 0))
                                (memq (car (table-lookup labels pc))
                                      '(bjump save))
                                #t))
                              (values #f #f)))
              (hexlines (bytes->hex bc pc n)))
         (if skip
             (format #t "|\n"))
         (if label
             (format #t "~04x ~-10a:\n" pc label))
         (format #t "~04x  ~8a ~30a~a\n"
                 pc
                 (car hexlines)
                 (fixup-labels (cddr item) symlabel)
                 aux)
         (let loop ((f (cdr hexlines))
                    (pc (+ pc 3)))
           (if (pair? f)
               (begin
                 (format #t "~04x  ~a\n" pc (car f))
                 (loop (+ pc 3) (cdr f)))))))
     insn-vec)))

(define (fixup-labels insn sym)
  (case (car insn)
    ((branch-if-false
      jump)
     `(,(car insn)
       ,(table-lookup sym (cadr insn)) 
       ,@(cddr insn)))
    ((bjump save)
     `(,(car insn)
       ,(cadr insn)
       ,(table-lookup sym (caddr insn)) 
       ,@(cdddr insn)))
    (else
     insn)))
            
(define (bytes->hex bc i n)
  (if (<= n 3)
      (list (string-join " "
                         (map (lambda (k)
                                (~ "~02x" (bvec-ref bc (+ i k))))
                              (range n))))
      (cons (car (bytes->hex bc i 3))
            (bytes->hex bc (+ i 3) (- n 3)))))
      
;;;

(define (test x)
  (if (foo x)
      (list x #x112233)
      '(a b)))

