;;;
;;;   SHA-1 implementation for RScheme (RFC 3174)
;;;   
;;;   Copyright (C) 2004 Donovan Kolbly
;;;   Licensed for Free Use
;;;

(define-class <sha1-state> (<object>) :bvec)

(define (sha1-message-trailer (n <integer>))
  (let* ((trailer (pack-string u64/b: (* 8 n)))
         (total-len (+ 1 (bitwise-or (+ n 8) 63)))
         (unfilled (- total-len n 9)))
    (string-append "\x80"
                   (make-string unfilled #\x00)
                   trailer)))

(define (usef t)
  (cond
   ((<= t 19) "F0")
   ((<= t 39) "F1")
   ((<= t 59) "F2")
   ((<= t 79) "F1")))

(define (usek t)
  (cond
   ((<= t 19) "K0")
   ((<= t 39) "K1")
   ((<= t 59) "K2")
   ((<= t 79) "K3")))

(define *sha-preamble*
#"C
unsigned char *inp;
unsigned long A, B, C, D, E, TEMP;
UINT_32 *H;
unsigned long W0,W1,W2,W3,W4,W5,W6,W7,W8,W9,W10,W11,W12,W13,W14,W15,W16,W17,W18,W19,W20,W21,W22,W23,W24,W25,W26,W27,W28,W29,W30,W31,W32,W33,W34,W35,W36,W37,W38,W39,W40,W41,W42,W43,W44,W45,W46,W47,W48,W49,W50,W51,W52,W53,W54,W55,W56,W57,W58,W59,W60,W61,W62,W63,W64,W65,W66,W67,W68,W69,W70,W71,W72,W73,W74,W75,W76,W77,W78,W79;

#define F0(b,c,d)       (((b)&(c))|((~(b)&(d))))
#define F1(b,c,d)       ((b)^(c)^(d))
#define F2(b,c,d)       (((b)&(c))|((b)&(d))|((c)&(d)))

#define K0              (0x5A827999)
#define K1              (0x6ED9EBA1)
#define K2              (0x8F1BBCDC)
#define K3              (0xCA62C1D6)

#define S(n,x) ((((x)<<n)&(0xFFFFFFFFUL)) | (((x)>>(32-n))))

  inp = PTR_TO_DATAPTR( input ) + offset;
  H = (UINT_32 *)PTR_TO_DATAPTR(state);
#"C
)

(define-safe-glue (make-sha-state)
  literals: ((& <sha1-state>))
{
  obj s = bvec_alloc( 20, TLREF(0) );
  unsigned *H = (unsigned *)PTR_TO_DATAPTR( s );
  H[0] = 0x67452301;
  H[1] = 0xEFCDAB89;
  H[2] = 0x98BADCFE;
  H[3] = 0x10325476;
  H[4] = 0xC3D2E1F0;
  REG0 = s;
  RETURN1();
})

(define *sha-postamble*
#"C
  RETURN0();
#undef F0
#undef F1
#undef F2
#undef K0
#undef K1
#undef K2
#undef K3
#undef S
#"C)

(define (process-block #optional (p default: (current-output-port)))
  ;;
  (define (compute-w t)
    (if (< t 16)
        ;;
        ;;  Step (a)
        ;;
        (begin
          (format p "  W~d = (inp[~d] << 24) + (inp[~d] << 16) +"
                  t 
                  (* t 4)
                  (+ (* t 4) 1))
          (format p " (inp[~d] << 8) + inp[~d];\n"
                  (+ (* t 4) 2)
                  (+ (* t 4) 3)))
        ;;
        ;;  Step (b)
        ;;
        (format p "  W~d = S(1,( W~d ^ W~d ^ W~d ^ W~d));\n"
                t
                (- t 3)
                (- t 8)
                (- t 14)
                (- t 16))))
  ;;
  (let ((a "A")
        (b "B")
        (c "C")
        (d "D")
        (e "E")
        (temp "TEMP"))
    (format p "  A = H[0];\n")
    (format p "  B = H[1];\n")
    (format p "  C = H[2];\n")
    (format p "  D = H[3];\n")
    (format p "  E = H[4];\n")
    (for-each
     (lambda (t)
       (compute-w t)
       (format p "  ~a = S(5,~a) + ~a(~a,~a,~a) + ~a + W~d + ~a;\n"
               temp
               a
               (usef t)
               b c d 
               e
               t
               (usek t))
       (let ((x e))
         (set! e d)
         (set! d c)
         (format p "  ~a = S(30,~a);\n" b b)
         (set! c b)
         (set! b a)
         (set! a temp)
         (set! temp x)))
     (range 80))
    ;;
    (format p "  H[0] += ~a;\n" a)
    (format p "  H[1] += ~a;\n" b)
    (format p "  H[2] += ~a;\n" c)
    (format p "  H[3] += ~a;\n" d)
    (format p "  H[4] += ~a;\n" e)))

(define (gen-sha-code)
  (let ((p (open-output-string)))
    (write-string p *sha-preamble*)
    (process-block p)
    (write-string p *sha-postamble*)
    (get-output-string p)))

(define-macro (define-sha-glue (name . args))
  `(define-safe-glue (,name ,@args)
     ,(make <curly-braced>
        text: (gen-sha-code))))

(define-sha-glue (SHA-block (input <string>) 
                            (offset <raw-int>) 
                            (state <sha1-state>)))



(define-safe-glue (sha1-state->binary-string (state <sha1-state>))
{
  UINT_32 *H = (UINT_32 *)PTR_TO_DATAPTR( state );
  obj str = bvec_alloc( 21, string_class );
  char *strp;
  unsigned i;

  strp = string_text( str );

  for (i=0; i<5; i++) {
    UINT_32 h = H[i];

    *strp++ = (h >> 24) & 0xFF;
    *strp++ = (h >> 16) & 0xFF;
    *strp++ = (h >>  8) & 0xFF;
    *strp++ = (h      ) & 0xFF;
  }
  REG0 = str;
  RETURN1();
})

(define-safe-glue (sha1-state->string (state <sha1-state>))
{
  UINT_32 *H = (UINT_32 *)PTR_TO_DATAPTR( state );
  obj str = bvec_alloc( 41, string_class );
  static const char *hex = "0123456789abcdef";
  char *strp;
  unsigned i;

  strp = string_text( str );

  for (i=0; i<5; i++) {
    UINT_32 h = H[i];

    *strp++ = hex[ (h >> 28) & 0xF ];
    *strp++ = hex[ (h >> 24) & 0xF ];
    *strp++ = hex[ (h >> 20) & 0xF ];
    *strp++ = hex[ (h >> 16) & 0xF ];
    *strp++ = hex[ (h >> 12) & 0xF ];
    *strp++ = hex[ (h >>  8) & 0xF ];
    *strp++ = hex[ (h >>  4) & 0xF ];
    *strp++ = hex[ (h >>  0) & 0xF ];
  }
  REG0 = str;
  RETURN1();
})

;;;

(define-method sha1-digest* ((self <string>))
  (let ((n (string-length self))
        (s (make-sha-state)))
    (let loop ((i 0))
      (if (<= (+ i 64) n)
          (begin
            (SHA-block self i s)
            (loop (+ i 64)))
          (let ((last-blocks (string-append (substring self i)
                                            (sha1-message-trailer n))))
            (case (string-length last-blocks)
              ((64) 
               (SHA-block last-blocks 0 s))
              ((128)
               (SHA-block last-blocks 0 s)
               (SHA-block last-blocks 64 s))
              (else
               (error "SHA padding didn't work out!?")))
            s)))))

(define-method sha1-digest* ((self <input-port>))
  (let ((s (make-sha-state)))
    (let loop ((n 0))
      (if (handler-case
           (let ((block (read-string self 64)))
             (SHA-block block 0 s)
             #t)
           ((<partial-read> condition: pr)
            (let* (((block <string>) (partially-read pr))
                   (n (+ n (string-length block)))
                   (last-blocks (string-append block
                                               (sha1-message-trailer n))))
              (case (string-length last-blocks)
                ((64) 
                 (SHA-block last-blocks 0 s))
                ((128)
                 (SHA-block last-blocks 0 s)
                 (SHA-block last-blocks 64 s))
                (else
                 (error "SHA padding didn't work out!?")))
              #f)))
          (loop (+ n 64))
          s))))

(define-method sha1-digest ((source <object>))
  (sha1-state->string (sha1-digest* source)))

(define-method sha1-binary-digest ((source <object>))
  (sha1-state->binary-string (sha1-digest* source)))

