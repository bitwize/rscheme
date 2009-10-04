;;;
;;;   SHA-256 implementation for RScheme (FIPS 180-2)
;;;
;;;   Copyright (C) 2005 Donovan Kolbly
;;;   Licensed for Free Use
;;;

(define-class <sha256-state> (<object>) :bvec)

(define-safe-glue (make-sha256-state)
  literals: ((& <sha256-state>))
{
  obj s = bvec_alloc( sizeof( UINT_32 ) * 8, TLREFB(0) );
  UINT_32 *H = (UINT_32 *)PTR_TO_DATAPTR( s );

  H[0] = 0x6a09e667;
  H[1] = 0xbb67ae85;
  H[2] = 0x3c6ef372;
  H[3] = 0xa54ff53a;
  H[4] = 0x510e527f;
  H[5] = 0x9b05688c;
  H[6] = 0x1f83d9ab;
  H[7] = 0x5be0cd19;
  REG0 = s;
  RETURN1();
})

(define-safe-glue (SHA256-block (input <string>)
                                (offset <raw-int>)
                                (state <sha256-state>))
  properties: ((other-c-files "sha256.c"))
{
  extern void update_sha256( obj i, int o, obj s );
  
  update_sha256( input, offset, state );
  RETURN0();
})


(define-safe-glue (sha256-state->binary-string (state <sha256-state>))
{
  UINT_32 *H = (UINT_32 *)PTR_TO_DATAPTR( state );
  obj str = bvec_alloc( 8*4+1, string_class );
  char *strp;
  unsigned i;

  strp = string_text( str );

  for (i=0; i<8; i++) {
    UINT_32 h = H[i];

    *strp++ = (h >> 24) & 0xFF;
    *strp++ = (h >> 16) & 0xFF;
    *strp++ = (h >>  8) & 0xFF;
    *strp++ = (h      ) & 0xFF;
  }
  REG0 = str;
  RETURN1();
})

(define-safe-glue (sha256-state->string (state <sha256-state>))
{
  UINT_32 *H = (UINT_32 *)PTR_TO_DATAPTR( state );
  obj str = bvec_alloc( 1+(2*8*4), string_class );
  static const char *hex = "0123456789abcdef";
  char *strp;
  unsigned i;

  strp = string_text( str );

  for (i=0; i<8; i++) {
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

#|

   fips180-message-trailer

   Build a message trailer that includes the 0x80 octet,
   a sequence of NUL octets, and an 8-byte big-endian representation
   of the message size in bits.

   This trailer, when appended to the last (partial) block of the
   message, will produce either one or two full 64-byte blocks.
|#


(define-safe-glue (fips180-message-trailer (j <integer>))
{
  UINT_32 total_len, n;
  UINT_8 *ptr;
  obj trailer;
  unsigned i, nunfilled;

  n = basic_raw_uint( j );

  total_len = 1 + ((n+8)|63);
  nunfilled = total_len - n - 9;

  trailer = bvec_alloc( (1 + nunfilled + 8 + 1), string_class );
  ptr = (UINT_8 *)PTR_TO_DATAPTR( trailer );
  *ptr++ = 0x80;
  for (i=0; i<nunfilled; i++) {
    *ptr++ = 0;
  }

  *ptr++ = 0;              /* byte[0] MSB */
  *ptr++ = 0;              /* byte[1]     */
  *ptr++ = 0;              /* byte[2]     */
  *ptr++ = (n >> (5+24));  /* byte[3]     */
  *ptr++ = (n >> (5+16));  /* byte[4]     */
  *ptr++ = (n >> (5+8));   /* byte[5]     */
  *ptr++ = (n >> (5));     /* byte[6]     */
  *ptr++ = (n << (8-5));   /* byte[7] LSB */

  REG0 = trailer;
  RETURN1();
})

(define-method fips180-digest* ((self <string>) s accum)
  (let ((n (string-length self)))
    (let loop ((i 0))
      (if (<= (+ i 64) n)
          (begin
            (accum self i s)
            (loop (+ i 64)))
          (let ((last-blocks (string-append (substring self i)
                                            (fips180-message-trailer n))))
            (case (string-length last-blocks)
              ((64) 
               (accum last-blocks 0 s))
              ((128)
               (accum last-blocks 0 s)
               (accum last-blocks 64 s))
              (else
               (error "SHA padding didn't work out!?")))
            s)))))

(define-method fips180-digest* ((self <input-port>) s accum)
  (let loop ((n 0))
    (if (handler-case
         (let ((block (read-string self 64)))
           (accum block 0 s)
           #t)
         ((<partial-read> condition: pr)
          (let* (((block <string>) (partially-read pr))
                 (n (+ n (string-length block)))
                 (last-blocks (string-append block
                                             (fips180-message-trailer n))))
            (case (string-length last-blocks)
              ((64) 
               (accum last-blocks 0 s))
              ((128)
               (accum last-blocks 0 s)
               (accum last-blocks 64 s))
              (else
               (error "SHA padding didn't work out!?")))
            #f)))
        (loop (+ n 64))
        s)))

#|

;; FIPS 180-2, Appendix B, Section 1 "SHA-256 Example (One-Block Message)"

(define *appxb1*
  (string-append "abc\200"
                 (make-string (- 64 4 1) #\x00)
                 (string #\x18)))
(define s (make-sha256-state))
(SHA256-block *appxb1* 0 s)


;; FIPS 180-2, Appendix B, Section 2 "SHA-256 Example (Multi-Block Message)"

(assert
 (string=? "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
           (sha256-state->string
            (fips180-digest*
             "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
             (make-sha256-state)
             SHA256-block))))

|#

(define-method sha256-digest ((source <object>))
  (sha256-state->string
   (fips180-digest* source (make-sha256-state) SHA256-block)))

(define-method sha256-binary-digest ((source <object>))
  (sha256-state->binary-string
   (fips180-digest* source (make-sha256-state) SHA256-block)))

