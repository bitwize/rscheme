(define (prepare-message-schedule p)
  ;; 6.2.2.
  ;; Step 1. Prepare the message schedule, W[t]
  (for-each
   (lambda (t)
     (if (< t 16)
         (begin
           (format p "  W[~d] = (inp[~d] << 24) + (inp[~d] << 16) +"
                   t 
                   (* t 4)
                   (+ (* t 4) 1))
           (format p " (inp[~d] << 8) + inp[~d];\n"
                   (+ (* t 4) 2)
                   (+ (* t 4) 3)))
         (format p "  W[~d] = sigma1(W[~d]) + W[~d] + sigma0(W[~d]) + W[~d];\n"
                 t
                 (- t 2)
                 (- t 7)
                 (- t 15)
                 (- t 16))))
   (range 64)))


(define *sha256-preamble*
#"C

#define ROTR(n,x) (((x)>>n) | ((x)<<(32-n)))
#define SHR(n,x)  ((x)>>n)

#define Ch(x,y,z)  (((x)&(y))^((~(x))&(z)))
#define Maj(x,y,z) (((x)&(y))^((x)&(z))^((y)&(z)))

#define SIGMA0(x)  (ROTR(2,x) ^ ROTR(13,x) ^ ROTR(22,x))
#define SIGMA1(x)  (ROTR(6,x) ^ ROTR(11,x) ^ ROTR(25,x))

#define sigma0(x)  (ROTR(7,x) ^ ROTR(18,x) ^ SHR(3,x))
#define sigma1(x)  (ROTR(17,x) ^ ROTR(19,x) ^ SHR(10,x))

  unsigned char *inp;
  unsigned t;

  UINT_32 a, b, c, d, e, f, g, h, T1, T2;
  UINT_32 *H;
  UINT_32 W[64];

  inp = PTR_TO_DATAPTR( input ) + offset;
  H = (UINT_32 *)PTR_TO_DATAPTR( state );

  /* 6.2.2. 
     Step 2. Initialize the eight working variables with the (i-1)'s hash value:
   */

  a = H[0];
  b = H[1];
  c = H[2];
  d = H[3];
  e = H[4];
  f = H[5];
  g = H[6];
  h = H[7];

#"C)

(define *sha256-postamble*
#"C

  RETURN0();
#undef ROTR
#undef SHR
#undef Ch
#undef Maj
#undef SIGMA0
#undef SIGMA1
#undef sigma0
#undef sigma1

#"C)

(define *constant*
  '#("0x428a2f98" "0x71374491" "0xb5c0fbcf" "0xe9b5dba5"
     "0x3956c25b" "0x59f111f1" "0x923f82a4" "0xab1c5ed5"
     "0xd807aa98" "0x12835b01" "0x243185be" "0x550c7dc3"
     "0x72be5d74" "0x80deb1fe" "0x9bdc06a7" "0xc19bf174"
     "0xe49b69c1" "0xefbe4786" "0x0fc19dc6" "0x240ca1cc"
     "0x2de92c6f" "0x4a7484aa" "0x5cb0a9dc" "0x76f988da"
     "0x983e5152" "0xa831c66d" "0xb00327c8" "0xbf597fc7"
     "0xc6e00bf3" "0xd5a79147" "0x06ca6351" "0x14292967"
     "0x27b70a85" "0x2e1b2138" "0x4d2c6dfc" "0x53380d13"
     "0x650a7354" "0x766a0abb" "0x81c2c92e" "0x92722c85"
     "0xa2bfe8a1" "0xa81a664b" "0xc24b8b70" "0xc76c51a3"
     "0xd192e819" "0xd6990624" "0xf40e3585" "0x106aa070"
     "0x19a4c116" "0x1e376c08" "0x2748774c" "0x34b0bcb5"
     "0x391c0cb3" "0x4ed8aa4a" "0x5b9cca4f" "0x682e6ff3"
     "0x748f82ee" "0x78a5636f" "0x84c87814" "0x8cc70208"
     "0x90befffa" "0xa4506ceb" "0xbef9a3f7" "0xc67178f2"))

(define (crunch-variables port)
  (for-each
   (lambda (t)
     (format port "T1 = h + SIGMA1(e) + Ch(e,f,g) + ~a + W[~d];\n"
             (vector-ref *constant* t)
             t)
     (format port "T2 = SIGMA0(a) + Maj(a,b,c);\n")
     (format port "h = g;\n")
     (format port "g = f;\n")
     (format port "f = e;\n")
     (format port "e = d + T1;\n")
     (format port "d = c;\n")
     (format port "c = b;\n")
     (format port "b = a;\n")
     (format port "a = T1 + T2;\n"))
   (range 64)))

(define (output-intermediate-hash port)
  (for-each
   (lambda (k)
     (format port "  H[~d] += ~a;\n" k (string-ref "abcdefgh" k)))
   (range 8)))
             

(define (gen-sha256-code)
  (call-with-output-string
   (lambda (port)
     (write-string port *sha256-preamble*)
     (prepare-message-schedule port)
     (crunch-variables port)
     (output-intermediate-hash port)
     (write-string port *sha256-postamble*))))

(define-macro (define-sha256-glue (name . args))
  `(define-safe-glue (,name ,@args)
     ,(make <curly-braced>
        text: (gen-sha256-code))))

(define-sha256-glue (SHA256-block (input <string>)
                                  (offset <raw-int>)
                                  (state <sha256-state>)))
