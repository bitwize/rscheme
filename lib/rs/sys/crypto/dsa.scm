#|
  Notes on configuring and using the josephson junction RNG
  on intel 810 (i810) linux boxes:

        # insmod i810_rng
        # mknod /dev/intel_rng c 10 183

        occasionally (and frequently):
                # dd if=/dev/intel_rng bs=32 count=1 > /dev/random

        reading 256 bytes from the rng takes ~1 second

|#

,(use syscalls
      rs.net.sha1
      rs.sys.crypto
      rs.net.pem)


(define (b->s (n <bignum>))
  (pem-encode (bignum->octet-string n)))

(define (s->b (s <string>))
  (octet-string->bignum (pem-decode s)))


(define-class <dsa-public-key> (<object>)
  (name type: <string>)
  (dsa-p type: <bignum>)
  (dsa-q type: <bignum>)
  (dsa-g type: <bignum>)
  (dsa-y type: <bignum>)
  (dsa-seed type: <bignum>)
  (dsa-counter type: <fixnum>))

(define-class <dsa-private-key> (<object>) :abstract
  (public type: <dsa-public-key>))

(define-class <dsa-encrypted-private-key> (<dsa-private-key>)
  (dsa-x-enc type: <string> init-value: ""))

(define-thread-var *passphrase-getter* #f)

(define (make-input-passphrase-filter name)
  (if *passphrase-getter*
      (let ((ks (hash-string->des3-key-schedule
                 (*passphrase-getter* 'input name))))
        (lambda (s)
          (check-trailer-strip (des3-decrypt s ks))))
      identity))

(define (make-output-passphrase-filter name)
  (if *passphrase-getter*
      (let ((ks (hash-string->des3-key-schedule
                 (*passphrase-getter* 'output name))))
        (lambda (s)
          (des3-encrypt (check-trailer-encapsulate s) ks)))
      identity))

(define-method name ((self <dsa-private-key>))
  (name (public self)))

(define-method dsa-x ((self <dsa-encrypted-private-key>))
  (octet-string->bignum ((make-input-passphrase-filter (name self)) 
                         (dsa-x-enc self))))

(define-method initialize ((self <dsa-encrypted-private-key>) 
                           #key (dsa-x default: #f))
  (if dsa-x
      (set-dsa-x-enc! self ((make-output-passphrase-filter (name self))
                            (bignum->octet-string dsa-x))))
  self)


(define (make-random-number high1? minbits variance)
  (let ((n (+ minbits (random variance))))   ; use PRNG for length variation
    (values (make-random-number* high1? n) n)))

(define (make-random-number* high1? n)
  (let* ((s (get-random-string (ceiling (/ n 8))))
         (low-mask (vector-ref '#(#xFF #x01 #x03 #x07
                                  #x0F #x1F #x3F #x7F)
                               (modulo n 8)))
         (high-bit (vector-ref '#(128 1 2 4 8 16 32 64) (modulo n 8))))
    ;;
    (bvec-set! s 0 (bitwise-and
                    (if high1? 
                        (bitwise-or (bvec-ref s 0) high-bit)
                        (bvec-ref s 0))
                    low-mask))
    ;;
    (values (octet-string->bignum s) n)))

(define (rntest high? n v)
  (for-each
   (lambda (i)
     (format #t "~4d => ~-60b\n" i (make-random-number high? n v)))
   (range 50)))

(define (sha msg)
  (string->bignum (sha1-digest msg) 16))

(define (bignum-sha n)
  (string->bignum (sha1-digest (number->string n 16)) 16))

(define (compute-q s g)
  ;; with certainty >= 1-(2^-160), s+1 will be less than 2^g
  (let ((u (bitwise-xor (bignum-sha s)
                        (bignum-sha (+ s 1)))))
    (bitwise-or
     u
     ;; this is #b1000...0001, where the number is 160 bits long
     730750818665451459101842416358141509827966271489)))

(define (get-random-string len)
  (let ((fd (fd-open "/dev/random" (make-fd-open-mode 'read) 0))
        ((s <string>) (make-string len)))
    (let loop ((i 0))
      (if (< i len)
          (let ((j (fd-read fd s i (- len i))))
            (loop (+ i j)))
          (begin
            (fd-close fd)
            s)))))

  

(define (make-random-byte-stream)
  (let ((fd (fd-open "/dev/random" (make-fd-open-mode 'read) 0))
        ((s <string>) (make-string 1)))
    (lambda (mode)
      (case mode
        ((next)
         (fd-read fd s 0 1)
         (bvec-ref s 0))
        ((close)
         (fd-close fd)
         (set! fd -1))))))

  
;;;

(define (progress fmt . args)
  (let ((p (current-output-port)))
    (apply format p fmt args)
    (flush-output-port p)))

(define (compute-prime-q)
  (let loop ()
    (progress ".")
    (bind ((S g (make-random-number #t 161 64))
           (q (compute-q S g)))
      (if (bignum-probably-prime? q)
          (values S g q)
          (loop)))))

(define (build-w n b S N g)
  (let loop ((k 0)
             (w 0))
    (let ((v (bignum-sha (mod2exp (+ S N k) g))))
      (if (= k n)
          (+ w (logical-shift-left (mod2exp v b) (* 160 k)))
          (loop (+ k 1)
                (+ w (logical-shift-left v (* 160 k))))))))
          

(define (dsa-prime-generate numbits)
  (bind ((b (modulo (- numbits 1) 160))
         (n (quotient (- numbits 1) 160))
         (S g q (compute-prime-q))
         (min-x (logical-shift-left 1 (- numbits 1))))
    (let loop ((C 0)
               (N 2))
      (if (= C 4096)
          (dsa-prime-generate numbits)
          (begin
            (progress "+")
            (let* ((x (+ (build-w n b S N g) min-x))
                   (p (- x (- (modulo x (* q 2)) 1))))
              (if (and (>= p min-x)
                       (bignum-probably-prime? p))
                  (begin
                    (progress "[done]\n")
                    (values p q S C))
                  (loop (+ C 1) (+ N n 1)))))))))
                      


(define (make-dsa-key-pair #key name (bits default: 832))
  (bind ((p q S C (dsa-prime-generate bits))
         (g h (pick-h p q))
         (x (pick-x q))
         (y (exp-modulo g x p)))
    (make <dsa-encrypted-private-key>
          public: (make <dsa-public-key>
                        name: name
                        dsa-p: p
                        dsa-q: q
                        dsa-g: g
                        dsa-y: y
                        dsa-seed: S
                        dsa-counter: C)
          dsa-x: x)))
    

   
(define (pick-h p q)
  (let loop ()
    (let* ((h (modulo (make-random-number #t 1024 16) (- p 1)))
           (g (exp-modulo h (quotient (- p 1) q) p)))
      (if (> g 1)
          (values g h)
          (loop)))))

(define (pick-x q)
  (let loop ()
    ;(progress "x")
    (let ((x (make-random-number #f 160 1)))
      (if (< x q)
          x
          (loop)))))

;;;

(define (dsa-sign (key <dsa-private-key>) (hash <bignum>))
  (let* ((pk (public key))
         (k (pick-x (dsa-q pk)))
         (r (modulo (exp-modulo (dsa-g pk) k (dsa-p pk)) (dsa-q pk)))
         ;;
         (s (modulo
             (* (invert-modulo k (dsa-q pk))
                (modulo (+ hash (* (dsa-x key) r)) (dsa-q pk)))
             (dsa-q pk))))
    ;;
    (list s r)))

#|

(define kk (make-dsa-key-pair name: "2183e196-f957-48ff-b9f9-f012393764ed"))
(define ss (dsa-sign kk (bignum-sha 123456789123456789123456789)))

|#

(define (dsa-valid? (key <dsa-public-key>) (hash <bignum>) sig)
  (let* ((q (dsa-q key))
         (p (dsa-p key))
         (w (invert-modulo (car sig) q))
         (u1 (modulo (* hash w) q))
         (u2 (modulo (* (cadr sig) w) q))
         (v (modulo (modulo (* (exp-modulo (dsa-g key) u1 p)
                               (exp-modulo (dsa-y key) u2 p))
                            p)
                    q)))
    (= v (cadr sig))))


  
(define (test-case-runs k)
  (for-each
   (lambda (i)
     (progress ".")
     (let* ((h (sha (~ "This is a ~d test, ~a" i (time))))
            (sig (dsa-sign k h)))
       (assert (dsa-valid? (public k) h sig))))
   (range 72))
  (progress "[ok]\n"))

(define (test-cases)
  (let loop ()
    (let ((k (make-dsa-key-pair name: "Test-Cases"
                                bits: (+ 512 (* 8 (random 64))))))
      (test-case-runs k)
      (test-case-runs k)
      (test-case-runs k)
      (test-case-runs k)
      (test-case-runs k)
      (test-case-runs k)
      (test-case-runs k)
      (test-case-runs k)
      (loop))))

;;;

