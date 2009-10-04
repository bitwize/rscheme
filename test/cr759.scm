
(define (big-string)
  (let ((p (open-output-string)))
    (for-each
     (lambda (i)
       (write (range i) p)
       (newline p))
     (range 1000))
    (close-output-port p)))

(define (zip str)
  (format #t "writing ~d bytes\n" (string-length str))
  (let ((o (open-output-process "gzip -c > /tmp/str.gz")))
    (write-string o str)
    (close-output-port o)
    (string-length str)))

;; in 0.7.3, this doesn't work (CR 743)
;; in b0, it SEGVs (CR 759)

(check 1895106 (zip (big-string)))

(with-module syscalls (unlink "/tmp/str.gz"))
