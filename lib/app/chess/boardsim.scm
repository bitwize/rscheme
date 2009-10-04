
(define-class <bit-board> (<object>) :bvec)

(define-syntax uop!
  (syntax-form (op t a k . r)
    (bvec-set! t k (op (bvec-ref a k)))
    (uop! op t a . r))
  (syntax-form (op t a)
    t))

(define (new-bit-board)
  (let ((b (bvec-alloc <bit-board> 8)))
    (uop! (lambda (x) 0) b "12345678" 0 1 2 3 4 5 6 7)))

(define (bbset (self <bit-board>)
               (file <symbol>)
               (rank <fixnum>))
  (let (((b <bit-board>) (clone self)))
    (bvec-set! b 
               (sub1 rank) 
               (bitwise-or
                (bvec-ref b (sub1 rank))
                (logical-shift-right #x80 (vmemq file '#(a b c d e f g h)))))
    b))

(define (bb-is-set? (self <bit-board>)
                    (file <symbol>)
                    (rank <fixnum>))
  (not (eq? (bitwise-and
             (bvec-ref self (sub1 rank))
             (logical-shift-right #x80 (vmemq file '#(a b c d e f g h))))
            0)))

(define-syntax bop!
  (syntax-form (op t a b k . r)
    (bvec-set! t k (op (bvec-ref a k) (bvec-ref b k)))
    (bop! op t a b . r))
  (syntax-form (op t a b)
    t))

(define-syntax (bop op a b)
  (let ((t (bvec-alloc <bit-board> 8)))
    (bop! op t a b 0 1 2 3 4 5 6 7)))

(define-syntax (uop op a)
  (let ((t (bvec-alloc <bit-board> 8)))
    (uop! op t a 0 1 2 3 4 5 6 7)))

(define (bexec (self <bit-board>) (cmd <string>) . args)
  (car (bexec* (make-vector 10) 
               (list->vector args)
               (list self)
               cmd)))

(define (do-repeat (regs <vector>) (args <vector>) stack (cmd <string>))
  (let rloop ((n 0) (stack stack))
    (if (< n 8)
        (rloop (+ n 1) (bexec* regs args stack cmd))
        stack)))

(define (bexec* (regs <vector>) (args <vector>) stack (cmd <string>))
  (define (digit k)
    (- (char->integer (string-ref cmd k))
       (char->integer #\0)))
  ;
  (let loop ((i 0)
             (stack stack))
    ;(format #t "  [~d] ... ~s\n" i stack)
    (if (= i (string-length cmd))
        stack
        (case (string-ref cmd i)
          ((#\newline #\tab #\space)
           (loop (+ i 1) stack))
          ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
           (loop (+ i 1) (cons (vector-ref args (digit i)) stack)))
          ((#\<)
           (loop (+ i 2) (cons (vector-ref regs (digit (+ i 1))) stack)))
          ((#\{)
           (let* ((j (string-search cmd #\} i))
                  (subr (substring cmd (+ i 1) j)))
             (loop (+ j 1) (do-repeat regs args stack subr))))
          ((#\>)
           (vector-set! regs (digit (+ i 1)) (car stack))
           (loop (+ i 2) (cdr stack)))
          ((#\!)
           (loop (+ i 1) (cons (new-bit-board) stack)))
          ((#\=)
           (loop (+ i 1) (cons (car stack) stack)))
          ((#\x)
           (loop (+ i 1) (cons* (cadr stack) (car stack) (cddr stack))))
          ((#\^)
           (loop (+ i 1) (cdr stack)))
          ((#\~)
           (loop (+ i 1) (cons (uop bitwise-not (car stack)) (cdr stack))))
          ((#\&)
           (loop (+ i 1) (cons (bop bitwise-and (car stack) (cadr stack))
                               (cddr stack))))
          ((#\|)
           (loop (+ i 1) (cons (bop bitwise-or (car stack) (cadr stack))
                               (cddr stack))))
          ((#\e) (loop (+ i 1) (cons (east (car stack)) (cdr stack))))
          ((#\w) (loop (+ i 1) (cons (west (car stack)) (cdr stack))))
          ((#\n) (loop (+ i 1) (cons (north (car stack)) (cdr stack))))
          ((#\s) (loop (+ i 1) (cons (south (car stack)) (cdr stack))))
          ((#\S)
           (loop (+ i 3)
                 (cons (bbset (car stack)
                              (string->symbol
                               (string (string-ref cmd (+ i 1))))
                              (digit (+ i 2)))
                       (cdr stack))))
          (else
           (error "bexec: bad command ~s" (string-ref cmd i)))))))

(define (east (a <bit-board>))
  (let ((t (bvec-alloc <bit-board> 8)))
    (bvec-set! t 0 (logical-shift-right (bvec-ref a 0) 1))
    (bvec-set! t 1 (logical-shift-right (bvec-ref a 1) 1))
    (bvec-set! t 2 (logical-shift-right (bvec-ref a 2) 1))
    (bvec-set! t 3 (logical-shift-right (bvec-ref a 3) 1))
    (bvec-set! t 4 (logical-shift-right (bvec-ref a 4) 1))
    (bvec-set! t 5 (logical-shift-right (bvec-ref a 5) 1))
    (bvec-set! t 6 (logical-shift-right (bvec-ref a 6) 1))
    (bvec-set! t 7 (logical-shift-right (bvec-ref a 7) 1))
    t))

(define (west (a <bit-board>))
  (let ((t (bvec-alloc <bit-board> 8)))
    (bvec-set! t 0 (bitwise-and #xFF (logical-shift-left (bvec-ref a 0) 1)))
    (bvec-set! t 1 (bitwise-and #xFF (logical-shift-left (bvec-ref a 1) 1)))
    (bvec-set! t 2 (bitwise-and #xFF (logical-shift-left (bvec-ref a 2) 1)))
    (bvec-set! t 3 (bitwise-and #xFF (logical-shift-left (bvec-ref a 3) 1)))
    (bvec-set! t 4 (bitwise-and #xFF (logical-shift-left (bvec-ref a 4) 1)))
    (bvec-set! t 5 (bitwise-and #xFF (logical-shift-left (bvec-ref a 5) 1)))
    (bvec-set! t 6 (bitwise-and #xFF (logical-shift-left (bvec-ref a 6) 1)))
    (bvec-set! t 7 (bitwise-and #xFF (logical-shift-left (bvec-ref a 7) 1)))
    t))

(define (north (a <bit-board>))
  (let ((t (bvec-alloc <bit-board> 8)))
    (bvec-set! t 0 0)
    (bvec-set! t 1 (bvec-ref a 0))
    (bvec-set! t 2 (bvec-ref a 1))
    (bvec-set! t 3 (bvec-ref a 2))
    (bvec-set! t 4 (bvec-ref a 3))
    (bvec-set! t 5 (bvec-ref a 4))
    (bvec-set! t 6 (bvec-ref a 5))
    (bvec-set! t 7 (bvec-ref a 6))
    t))

(define (south (a <bit-board>))
  (let ((t (bvec-alloc <bit-board> 8)))
    (bvec-set! t 0 (bvec-ref a 1))
    (bvec-set! t 1 (bvec-ref a 2))
    (bvec-set! t 2 (bvec-ref a 3))
    (bvec-set! t 3 (bvec-ref a 4))
    (bvec-set! t 4 (bvec-ref a 5))
    (bvec-set! t 5 (bvec-ref a 6))
    (bvec-set! t 6 (bvec-ref a 7))
    (bvec-set! t 7 0)
    t))

#|
(define-safe-glue (bexec (self <bit-board>) (cmd <string>) #rest)
{
  unsigned long long tmp, stack[10], regset[10], *sp;
  int r, f, i, ni = string_length( cmd );
  char *p = string_text( cmd );
  obj t;
  int argc = arg_count_reg - 2;

  sp = &stack[10];
  *--sp = *(unsigned long long *)PTR_TO_DATAPTR( self );
  printf( "init => %016llx\n", *sp );

  for (i=0; i<ni; i++) {
    printf( "  OP '%c', stack depth %d\n", p[i], &stack[10] - sp );
    switch (p[i]) {

      case '\t':
      case '\n':
      case ' ':
        break;  /* skip whitespace */

      default:
        scheme_error( "bexec: bad opcode 0x~02x", 1, int2fx( p[i] ) );
        break;

      case '0': case '1': case '2':
      case '3': case '4': case '5':
      case '6': case '7': case '8':
      case '9':
        if ((p[i] - '0') >= argc) {
          scheme_error( "bexec: arg[~a] out of range", 1, MAKE_ASCII_CHAR(p[i]) );
        }
        t = reg_ref( (p[i] - '0' ) + 2 );
        *--sp = *(unsigned long long *)PTR_TO_DATAPTR(t);
        printf( "push(arg[%d]) => %016llx\n", p[i]-'0', *sp );
        break;

      case 'S':         /* set */
        r = p[++i];
        f = p[++i];
        if ((r < 1) || (r > 8)) {
          scheme_error( "bexec: SET rank ~d out of range 1..8", 1, int2fx(r) );
        }
        if ((f < 1) || (f > 8)) {
          scheme_error( "bexec: SET file ~d out of range 1..8", 1, int2fx(f) );
        }
        *sp |= ((unsigned long long)(0x80 >> (f-1))) << ((r-1)*8);
        printf( "set(%c,%d) => %016llx\n", "?abcdefgh"[f], r, *sp );
        break;

      case '!':         /* new */
        *--sp = 0;
        break;

      case '=':         /* dup */
        sp[-1] = sp[0];
        sp--;
        break;

      case '<':         /* load */
        *--sp = regset[p[++i]-'0'];
        break;

      case '>':         /* store */
        regset[p[++i]-'0'] = *sp++;
        break;

      case 'x':         /* exch */
        tmp = sp[0];
        sp[0] = sp[1];
        sp[1] = tmp;
        break;

      case '&':         /* and */
        sp[1] = sp[0] & sp[1];
        sp++;
        break;

      case '|':         /* or */
        sp[1] = sp[0] | sp[1];
        sp++;
        break;

      case '~':         /* not */
        sp[0] = ~sp[0];
        break;

      case 'w':         /* west */
        sp[0] = (sp[0] & 0x7F7F7F7F7F7F7F7FULL) << 1;
        break;

      case 'e':         /* east */
        sp[0] = (sp[0] & 0xFEFEFEFEFEFEFEFEULL) >> 1;
        break;

      case 'n':
        sp[0] = (sp[0] << 8);
        break;

      case 's':
        sp[0] = (sp[0] >> 8);
        break;
    }
  }
  printf( "stack = %d items, top = %016llx\n", &stack[10] - sp, *sp );
  REG0 = bvec_alloc( sizeof(unsigned long long), CLASSOF_PTR(self) );
  *((unsigned long long *)PTR_TO_DATAPTR( REG0 )) = *sp;
  RETURN1();
})

|#
