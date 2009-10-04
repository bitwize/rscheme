
;;; create a stream for md5, which is implemented as 
;;; a procedure which returns either a block of 64 bytes
;;; or #f for end-of-stream

(define (make-md5-stream (src <input-port>) #optional (len default: 0))
  (let ((at-last #f))
    (lambda ()
      (if at-last
	  (if (string? at-last)
	      (let ((l at-last))
		(set! at-last #t)
                l)
	      #f)
	  (handler-case
	   (let ((block (read-string src 64)))
	     (set! len (+ len 64))
             block)
	   ((<partial-read> condition: pr)
	    (let ((lenstr (make-string 8 #\nul))
		  ((block <string>) (partially-read pr)))
	      (set! len (+ len (string-length block)))
	      (let ((lenbits (* len 8)))
		(let-syntax ((setb (syntax-form (i)
				     (bvec-set! 
				      lenstr
				      i
				      (bitwise-and
				       #xFF
				       (logical-shift-right
					lenbits
					(* i 8)))))))
		  (setb 0) (setb 1)
		  (setb 2) (setb 3)
                  ;;these are broken for <fixnum>'s
                  ;;(setb 4) (setb 5)
                  ;;(setb 6) (setb 7)
		  ))
              ;;
              ;;(format #t "total length = ~d bits\n" len)
              ;;(print lenstr)
	      ;;
	      (let ((mlen (modulo len 64)))
		(if (>= mlen 56)
		    ;; can't fit padding + length in this block
		    (begin
		      (set! at-last (string-append 
				     (make-string 56 #\nul)
				     lenstr))
		      (string-append block 
				     "\200"
				     (make-string (- 63 mlen) #\nul)))
		    ;; can fit padding + length in this block
		    (begin
		      (set! at-last #t)
		      (string-append block
				     "\200"
				     (make-string (- 55 mlen) #\nul)
				     lenstr)))))))))))

;;;
;;;  we treat this as a little-endian representation
;;;  because that's what MD5 likes (silly people)
;;;

(define-class <md5-state> (<object>) :bvec)

(define-glue (md5-state->string (state <md5-state>))
{
  unsigned *s = (unsigned *)PTR_TO_DATAPTR( state );
  obj str;
  char *strp;
  int i, j;

  str = bvec_alloc( 33, string_class );
  strp = (char *)PTR_TO_DATAPTR( str );

  for (i=0; i<4; i++)
    {
      unsigned v = s[i];

      for (j=0; j<4; j++)
        {
          sprintf( strp, "%02x", v & 0xFF );
	  v >>= 8;
	  strp += 2;
        }
    }
  REG0 = str;
  RETURN1();
})

(define-glue (init-md5-state (state <md5-state>))
{
  UINT_32 A, B, C, D;

  A = 0x67452301;
  B = 0xefcdab89;
  C = 0x98badcfe;
  D = 0x10325476;

  ((unsigned *)PTR_TO_DATAPTR( state ))[0] = A;
  ((unsigned *)PTR_TO_DATAPTR( state ))[1] = B;
  ((unsigned *)PTR_TO_DATAPTR( state ))[2] = C;
  ((unsigned *)PTR_TO_DATAPTR( state ))[3] = D;
  RETURN0();
})

(define (make-md5-state)
  (let ((s (bvec-alloc <md5-state> 16)))
    (init-md5-state s)
    s))


(define-macro (define-md5-glue (name . args))
  ;;
  (define *pre*
    '{
  UINT_32 tmp, A, B, C, D;
  UINT_8 *x = PTR_TO_DATAPTR( block );

  /*
  printf( ">" );
  for (A=0; A<64; A++)
    printf( " %02x", x[A] );
  printf( "\n" );
  */

  A = ((unsigned *)PTR_TO_DATAPTR( state ))[0];
  B = ((unsigned *)PTR_TO_DATAPTR( state ))[1];
  C = ((unsigned *)PTR_TO_DATAPTR( state ))[2];
  D = ((unsigned *)PTR_TO_DATAPTR( state ))[3];

  /*printf( "[ %08x %08x %08x %08x\n", A, B, C, D );*/

#define ROR(x,s) (((x<<s)&(0xFFFFFFFFUL)) + ((x>>(32-s))))

#define F(x,y,z) (((x)&(y))|((~(x))&(z)))
#define G(x,y,z) (((x)&(z))|((y)&(~(z))))
#define H(x,y,z) ((x)^(y)^(z))
#define I(x,y,z) ((y)^((x)|(~(z))))
#define block(i) (x[i*4]+(x[i*4+1]<<8)+(x[i*4+2]<<16)+(x[i*4+3]<<24))
  })

  (define *post*
    '{
  ((unsigned *)PTR_TO_DATAPTR( state ))[0] += A;
  ((unsigned *)PTR_TO_DATAPTR( state ))[1] += B;
  ((unsigned *)PTR_TO_DATAPTR( state ))[2] += C;
  ((unsigned *)PTR_TO_DATAPTR( state ))[3] += D;

  /*printf( "  %08x %08x %08x %08x ]\n", A, B, C, D );*/


#undef ROR
#undef F
#undef G
#undef H
#undef I
#undef block

  RETURN0();
  })
  ;;
  (define *T*
    (list->vector
     (map 
      (lambda (i)
	(let ((n (* 4294967296.0 (abs (sin (+ i 1))))))
	  (string->number (sprintf-float "%.0f" 20 (floor n)))))
      (range 64))))
  ;;
  (define (gen-update-md5-glue)
    (with-output-to-string
      (lambda ()
	(display *pre*)
	(for-each (lambda (op-desc)
		    (apply gen-round-from-op op-desc))
		  *round-ops*)
	(display *post*))))
  
  (define (gen-round-from-op f a b c d k s i)
    (format #t "  tmp = block(~d);\n" k)
    ;(format #t "  printf( \"~a X[~d] = 0x%08lx\\n\", tmp );\n" f k)
    (format #t "  tmp = ~a + ~a(~a,~a,~a) + tmp + 0x~x;\n"
	    a f b c d (vector-ref *T* (- i 1)))
    ;(format #t "  printf( \"      tmp = 0x%08lx\\n\", tmp );\n")
    (format #t "  ~a = ~a + ROR(tmp,~d);\n" a b s))
  
;;; a `round op' is of the form (f a b c d k s i)
;;; and the operation is:
;;;
;;;    (set! a (+ b (rotate-left (+ a (f b c d) (X k) (T i)) s)))
;;;
;;; where (X k) is word k of the block
;;;
  
  (define *round-ops*
    '((F A B C D  0  7  1)
      (F D A B C  1 12  2)
      (F C D A B  2 17  3)
      (F B C D A  3 22  4)
      
      (F A B C D  4  7  5)
      (F D A B C  5 12  6)
      (F C D A B  6 17  7)
      (F B C D A  7 22  8)

      (F A B C D  8  7  9)
      (F D A B C  9 12 10)
      (F C D A B 10 17 11)
      (F B C D A 11 22 12)

      (F A B C D 12  7 13)
      (F D A B C 13 12 14)
      (F C D A B 14 17 15)
      (F B C D A 15 22 16)

      (G A B C D  1  5 17)
      (G D A B C  6  9 18)
      (G C D A B 11 14 19)
      (G B C D A  0 20 20)

      (G A B C D  5  5 21)
      (G D A B C 10  9 22)
      (G C D A B 15 14 23)
      (G B C D A  4 20 24)

      (G A B C D  9  5 25)
      (G D A B C 14  9 26)
      (G C D A B  3 14 27)
      (G B C D A  8 20 28)

      (G A B C D 13  5 29)
      (G D A B C  2  9 30)
      (G C D A B  7 14 31)
      (G B C D A 12 20 32)

      (H A B C D  5  4 33)
      (H D A B C  8 11 34)
      (H C D A B 11 16 35)
      (H B C D A 14 23 36)

      (H A B C D  1  4 37)
      (H D A B C  4 11 38)
      (H C D A B  7 16 39)
      (H B C D A 10 23 40)

      (H A B C D 13  4 41)
      (H D A B C  0 11 42)
      (H C D A B  3 16 43)
      (H B C D A  6 23 44)

      (H A B C D  9  4 45)
      (H D A B C 12 11 46)
      (H C D A B 15 16 47)
      (H B C D A  2 23 48)


      (I A B C D  0  6 49)
      (I D A B C  7 10 50)
      (I C D A B 14 15 51)
      (I B C D A  5 21 52)

      (I A B C D 12  6 53)
      (I D A B C  3 10 54)
      (I C D A B 10 15 55)
      (I B C D A  1 21 56)

      (I A B C D  8  6 57)
      (I D A B C 15 10 58)
      (I C D A B  6 15 59)
      (I B C D A 13 21 60)

      (I A B C D  4  6 61)
      (I D A B C 11 10 62)
      (I C D A B  2 15 63)
      (I B C D A  9 21 64)))

  `(define-safe-glue (,name ,@args)
     ,(make <curly-braced>
	text: (gen-update-md5-glue))))


(define-md5-glue (update-md5-state (state <md5-state>) (block <string>)))

(define-method to-string ((self <md5-state>))
  (md5-state->string self))

;;;

(define (md5-digest x)
  (md5-state->string (md5-binary-digest x)))

(define-method md5-binary-digest ((self <string>))
  (md5-binary-digest (open-input-string self)))

(define-method md5-binary-digest ((self <input-port>))
  (let ((strm (make-md5-stream self))
	(state (make-md5-state)))
    (let loop ()
      (let ((block (strm)))
	(if block
	    (begin
	      (update-md5-state state block)
	      (loop))
	    state)))))

(define (md5-prefix (self <string>) (pad <char>))
  (let ((s (make-md5-state)))
    (let loop ((i 0))
      (if (< (+ i 64) (string-length self))
          (begin
            (update-md5-state s (substring self i (+ i 64)))
            (loop (+ i 64)))
          (begin
            (if (< i (string-length self))
                (update-md5-state
                 s
                 (string-append
                  (substring self i)
                  (make-string (- (string-length self) i) pad))))
            s)))))


;;;

(define-class <md5-accumulator> (<object>)
  (state type: <md5-state>)
  (buffer type: <string>)
  (total-length type: <fixnum> init-value: 0))

(define (make-md5-accum)
  (make <md5-accumulator>
        state: (make-md5-state)
        buffer: (make-string 64)))

(define-method accum->md5 ((self <string>) (accum <md5-accumulator>))
  (accum->md5 (open-input-string self) accum))

(define-method accum->md5 ((self <input-port>) (accum <md5-accumulator>))
  (let (((buf <string>) (buffer accum))
        ((s <md5-state>) (state accum)))
    ;;
    (let loop ((j (bitwise-and (total-length accum) 63))
               (n (total-length accum)))
      (let* ((m (- 64 j))
             (chunk (input-port-read-max self m)))
        (if (string? chunk)
            (if (= (string-length chunk) m)
                (begin
                  (if (= j 0)
                      (update-md5-state s chunk)
                      (begin
                        (bvec-copy buf j chunk 0 m)
                        (update-md5-state s buf)))
                  (loop 0 (+ n m)))
                (begin
                  (assert (< (string-length chunk) m))
                  (bvec-copy buf j chunk 0 (string-length chunk))
                  (set-total-length! accum (+ n (string-length chunk)))))
            (set-total-length! accum n))))))
    
(define (md5-finalize (self <md5-accumulator>))
  (let* ((n (total-length self))
         (j (bitwise-and n 63))
         (buf (buffer self))
         (s (clone (state self))))
    ;;
    (if (>= j 56)
        ;; can't fit padding + length in this block
        (begin
          (update-md5-state s (string-append (substring buf 0 j)
                                             "\200"
                                             (make-string (- 63 j) #\nul)))
          (update-md5-state s (string-append (make-string 56 #\nul)
                                             (length->md5-len-string n))))
        ;; can fit it in this block...
        (update-md5-state s (string-append (substring buf 0 j)
                                           "\200"
                                           (make-string (- 55 j) #\nul)
                                           (length->md5-len-string n))))
    (md5-state->string s)))
        

(define (length->md5-len-string (n <integer>))
  (let ((lenstr (make-string 8 #\nul)))
    (let-syntax ((setbr (syntax-form (i b)
                          (bvec-set! lenstr
                                     i
                                     (bitwise-and #xFF (logical-shift-right n b)))))
                 (setbl (syntax-form (i b)
                          (bvec-set! lenstr
                                     i
                                     (bitwise-and #xFF (logical-shift-left n b))))))
      ;;
      (setbl 0 3) 
      (setbr 1 5)
      (setbr 2 13)
      (setbr 3 21)
      (if (not (fixnum? n))
          (begin
            (setbr 4 29)
            (setbr 5 37)
            (setbr 6 45)
            (setbr 7 53)))
      lenstr)))

#|
(define (tp)
  (for-each
   (lambda (i)
     (let* ((s (list->string (map (lambda (j)
                                    (integer->char (+ (random 26) 65)))
                                  (range i))))
            (o (md5-digest s))                  ; old
            (n (let ((m (make-md5-accum)))      ; new
                 (accum->md5 s m)
                 (md5-finalize m))))
       ;;
       (format #t "~d ~s ~s ~s\n" i s o n)
       (assert (string=? o n))))
   (range 500)))
|#
