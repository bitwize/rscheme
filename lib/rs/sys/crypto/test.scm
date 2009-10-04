,(use rs.sys.crypto
      rs.net.sha1
      syscalls)

(define (t)
  (exp-modulo (string->bignum "1570" 10)
              (string->bignum "1019" 10)
              (string->bignum "3337" 10)))


