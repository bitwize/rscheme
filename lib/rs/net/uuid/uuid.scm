,(use syscalls 
      unixm
      rs.net.md5)

(define-class <uuid> (<object>) :bvec)

(define *seq* "d519eda0-931d-44ae-83ae-dbb95642ebef")

(define (make-uuid)
  (let* ((u (bvec-alloc <uuid> 16))
         (stuff (string-append "XXXXXXXX"
                               (number->string (random))
                               ","
                               *seq*
                               ","
                               (number->string (random))
                               "."
                               (number->string (getpid))
                               ","
                               (machine-bits->string u)
                               ","
                               (let ((x (or (stat "/dev/tty")
                                            (stat "/tmp"))))
                                 (if x
                                     (time->string (stat-mtime x))
                                     "?"))
                               ","
                               (machine-bits->string (cons u u))))
         (t (time)))
    ;;
    (bvec-copy stuff 0 t 0 (bvec-length t))
    (let ((x (md5-binary-digest stuff)))
      (set! *seq* (to-string x))
      (bvec-copy u 0 x 0 16)
      ;; c.f. http://www.webdav.org/specs/draft-leach-uuids-guids-01.txt
      (bvec-set! u 8 (bitwise-or #x80 (bitwise-and (bvec-ref u 8) #b00111111)))
      (bvec-set! u 6 (bitwise-or #x40 (bitwise-and (bvec-ref u 6) #x0F)))
      u)))

(define-method to-string ((self <uuid>))
  (string-append
   (~ "~02x" (bvec-ref self 0))
   (~ "~02x" (bvec-ref self 1))
   (~ "~02x" (bvec-ref self 2))
   (~ "~02x" (bvec-ref self 3))
   "-"
   (~ "~02x" (bvec-ref self 4))
   (~ "~02x" (bvec-ref self 5))
   "-"
   (~ "~02x" (bvec-ref self 6))
   (~ "~02x" (bvec-ref self 7))
   "-"
   (~ "~02x" (bvec-ref self 8))
   (~ "~02x" (bvec-ref self 9))
   "-"
   (~ "~02x" (bvec-ref self 10))
   (~ "~02x" (bvec-ref self 11))
   (~ "~02x" (bvec-ref self 12))
   (~ "~02x" (bvec-ref self 13))
   (~ "~02x" (bvec-ref self 14))
   (~ "~02x" (bvec-ref self 15))))
   
#|
         0 1 2 3  4 5  6 7  8 9  A B C D E F
        LLLLLLLL-MMMM-HHHH-XXSS-NNNNNNNNNNNN

L = time_low
M = time_mid
H = time_high_and_version
X = clock_seq_and_reserved
S = clock_seq_low
N = node

|#
