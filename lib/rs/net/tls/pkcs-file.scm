,(use regex
      rs.net.pem)

(define (pem-read-certificate file)
  (pem-read-object file "CERTIFICATE"))

(define pem-file-delimter (reg-expr->proc 
                           '(seq "-----" 
                                 (* #\-)
                                 (save (or "BEGIN" "END"))
                                 #\space
                                 (save (+ (or alpha digit #\space)))
                                 "-----"
                                 (* #\-))))

(define (pem-read-object file type)
  (call-with-input-file
      file
    (lambda (port)
      (let loop ((r '())
                 (in #f))
        (let ((l (read-line port)))
          (if (string? l)
              (bind ((s e be label (pem-file-delimter l)))
                (cond
                 ((and (not in)
                       s
                       (string=? be "BEGIN")
                       (string=? label type))
                  (loop '() #t))
                 ((and in s (string=? be "END"))
                  (string->byte-vector
                   (pem-decode (apply string-append (reverse! r)))))
                 (in
                  (loop (cons l r) #t))
                 (else
                  (loop '() #f))))
              (error "~a: pem file misformatted" file)))))))

