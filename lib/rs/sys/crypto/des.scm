,(use rs.net.sha1
      rs.util.pack)

(define-class <des-key> (<object>) :bvec)

(define-class <des-key-schedule> (<object>) :bvec)

(define-class <des-3key-schedule> (<object>))

(define-macro (define-des-glue args . body)
  `(define-safe-glue ,args
     properties: ((other-h-files "<openssl/des.h>")
                  (other-libs "ssl"))
     type-handler: (<des-key>
                    (direct-instance? <des-key>)
                    ("DES_cblock *~a"
                     "(DES_cblock *)PTR_TO_DATAPTR(~a)"))
     type-handler: (<des-key-schedule>
                    (direct-instance? <des-key-schedule>)
                    ("DES_key_schedule *~a"
                     "(DES_key_schedule *)PTR_TO_DATAPTR(~a)"))
     type-handler: (<des-3key-schedule>
                    (direct-instance? <des-3key-schedule>)
                    ("DES_key_schedule *~a"
                     "(DES_key_schedule *)PTR_TO_DATAPTR(gvec_ref(~a,SLOT(0)))"
                     "~a_A")
                    ("DES_key_schedule *~a"
                     "(DES_key_schedule *)PTR_TO_DATAPTR(gvec_ref(~a,SLOT(1)))"
                     "~a_B")
                    ("DES_key_schedule *~a"
                     "(DES_key_schedule *)PTR_TO_DATAPTR(gvec_ref(~a,SLOT(2)))"
                     "~a_C"))
                    
     ,@body))

(define-des-glue (set-odd-key-parity! (key <des-key>))
{
   DES_set_odd_parity( key );
   RETURN0();
})

(define-des-glue (make-key-schedule (key <des-key>))
  literals: ((& <des-key-schedule>))
{
  obj ks;
  int rc;

  ks = bvec_alloc( sizeof( DES_key_schedule ), TLREF(0) );
  
  rc = DES_set_key_checked( key, 
                            (DES_key_schedule *)PTR_TO_DATAPTR( ks ) );
  if (rc < 0) {
    if (rc == -2) {
      scheme_error( "DES_set_key_checked: weak key: ~s", 1, key );
    } else if (rc == -1) {
      scheme_error( "DES_set_key_checked: parity error: ~s", 1, key );
    } else {
      scheme_error( "DES_set_key_checked: error ~d", 1, int2fx( rc ) );
    }
  }
  REG0 = ks;
  RETURN1();
})

(define-des-glue (encrypt3 (ksv <des-3key-schedule>)
                           (src <string>)
                           (s_offset <raw-int>)
                           (dst <string>)
                           (d_offset <raw-int>))
{
  DES_ecb3_encrypt( (DES_cblock *)((char *)PTR_TO_DATAPTR(src) + s_offset),
                    (DES_cblock *)((char *)PTR_TO_DATAPTR(dst) + d_offset),
                    ksv_A,
                    ksv_B,
                    ksv_C,
                    1 );
  RETURN0();
})

(define-des-glue (decrypt3 (ksv <des-3key-schedule>)
                           (src <string>)
                           (s_offset <raw-int>)
                           (dst <string>)
                           (d_offset <raw-int>))
{
  DES_ecb3_encrypt( (DES_cblock *)((char *)PTR_TO_DATAPTR(src) + s_offset),
                    (DES_cblock *)((char *)PTR_TO_DATAPTR(dst) + d_offset),
                    ksv_A,
                    ksv_B,
                    ksv_C,
                    0 );
  RETURN0();
})

(define (hash-string->des-key str)
  (let ((k (bvec-alloc <des-key> 8))
        (h (sha1-binary-digest str)))
    ;;
    (bvec-set! k 0 (bvec-ref h 0))
    (bvec-set! k 1 (bvec-ref h 1))
    (bvec-set! k 2 (bvec-ref h 2))
    (bvec-set! k 3 (bvec-ref h 3))
    (bvec-set! k 4 (bvec-ref h 4))
    (bvec-set! k 5 (bvec-ref h 5))
    (bvec-set! k 6 (bvec-ref h 6))
    (bvec-set! k 7 (bvec-ref h 7))
    ;;
    (set-odd-key-parity! k)
    k))

(define (hash-string->des3-key-schedule str)
  (make-gvec
   <des-3key-schedule>
   (make-key-schedule (hash-string->des-key str))
   (make-key-schedule (hash-string->des-key (string-append str "&" str)))
   (make-key-schedule (hash-string->des-key (string-append "12+" str)))))
  

(define (t)
  (hash-string->des3-key-schedule "foo"))

(define (e)
  (let ((o (make-string 8)))
    (encrypt3 (t) "Hello, I" 0 o 0)
    o))

(define (d in)
  (let ((o (make-string 8)))
    (decrypt3 (t) in 0 o 0)
    o))

(define (block-padding len bs)
  (let ((p (modulo (- bs (modulo len bs)) bs)))
    (make-string p #\x00)))

(define (check-trailer-encapsulate (self <string>))
  (string-append self (sha1-binary-digest self)))

(define (check-trailer-strip (self <string>))
  (if (< (string-length self) 20)
      (error "String too short")
      (let* ((n (- (string-length self) 20))
             (h (substring self n))
             (p (substring self 0 n)))
        (if (string=? (sha1-binary-digest p) h)
            (values)
            (error "Internal hash mismatch")))))

(define (pad-for-blocking (self <string>) block-size)
  (let* ((len (string-length self))
         (cleartext (string-append self
                                   (block-padding (+ len 8) block-size)
                                   "\0\0\0\0"
                                   (pack-string u32/b: len))))
    cleartext))

(define-method des3-encrypt ((self <string>) (ks <des-3key-schedule>))
  (let* ((work (pad-for-blocking self 8))
         (n (string-length work)))
    ;;
    (let loop ((i 0))
      (if (< i n)
          (begin
            (encrypt3 ks work i work i)
            (loop (+ i 8)))
          work))))     ; was cleartext; now ciphertext

(define-method des3-decrypt ((self <string>) (ks <des-3key-schedule>))
  (let ((n (string-length self)))
    ;;
    (let loop ((i 0))
      (if (< i n)
          (begin
            (decrypt3 ks self i self i)
            (loop (+ i 8)))
          (with-unpack
           (substring self (- n 4))
           (u32/b: len)
           (substring self 0 len))))))
