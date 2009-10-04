
(define-class <rfc822-message> (<object>)
  (properties init-value: '#())
  (sender type: <rfc822-address>)
  (recipients type: <list>)             ; list of <rfc822-address>
  (headers type: <list>)
  (contents type: <string>))

(define-method display-object ((self <rfc822-message>) port)
  (for-each
   (lambda ((h <pair>))
     (write-string port (car h))
     (write-string port ": ")
     (write-string port (cadr h))
     (write-string port "\n")
     (for-each
      (lambda (follow)
        (write-string port "\t")
        (write-string port follow)
        (write-string port "\n"))
      (cddr h)))
   (headers self))
  (write-string port "\n")
  (write-string port (contents self)))


(define rfc822-contn (reg-expr->proc '(prefix (+ space))))
(define rfc822-hdr (reg-expr->proc '(prefix (seq
                                             (save (+ (not (or space #\:))))
                                             #\:
                                             (+ space)))))

(define (read-headers port)
  (let loop ((h '()))
    (let ((line (read-line port)))
      (if (or (eof-object? line) (string=? line ""))
          (reverse! h)
          (bind ((s e hdr (rfc822-hdr line)))
            (if s
                (loop (cons (list hdr (substring line e)) h))
                (bind ((s e (rfc822-contn line)))
                  (if s
                      (if (null? h)
                          (error "Header continuation before first header!")
                          (loop (cons (append (car h)
                                              (list (substring line e)))
                                      (cdr h))))
                      (error "Invalid header line")))))))))

#|
(define (crack-headers (v <vector>))
  (let ((n (vector-length v)))
    ;;
    (let loop ((i 0)
               (h '()))
      (if (< i n)
          (if (string=? (vector-ref v i) "")
              (values (reverse! h) (subvector v (+ i 1)))
              (bind ((line (vector-ref v i))
                     (s e hdr (rfc822-hdr line)))
                (if s
                    (loop (+ i 1)
                          (cons (list hdr (substring line e)) h))
                    (bind ((s e (rfc822-contn line)))
                      (if s
                          (if (null? h)
                              (error "header continuation before first header")
                              (loop (+ i 1)
                                    (cons (append (car h) 
                                                  (list (substring line e)))
                                          (cdr h))))
                          (error "invalid header line"))))))
          (values (reverse! h) '#())))))
|#


(define-method parse-rfc822-message ((self <string>)
                                     (envelope-sender <rfc822-address>)
                                     (envelope-recipients <list>))
  ;;
  (assert (every? (lambda (i)
                    (instance? i <rfc822-address>))
                  envelope-recipients))
  ;;
  (bind ((p (open-input-string self))
         (hdrs (read-headers p)))
    (make <rfc822-message>
          sender: envelope-sender
          recipients: envelope-recipients
          headers: hdrs
          contents: (read-string p))))

(define (read-mailbox-body port)
  (call-with-output-string
   (lambda (accum)
     (let loop ()
       (let* ((p (port-position port))
              (l (read-line port)))
         (cond
          ((eof-object? l)
           (values))
          ((mailbox-from-line l)
           (set-port-position! port p))
          (else
           (write-string accum l)
           (newline accum)
           (loop))))))))

(define mailbox-from-line (reg-expr->proc
                           '(entire
                             (seq "From "
                                  (save (seq (+ (not (or space #\@)))
                                             #\@
                                             (+ (not (or space #\@)))))
                                  (+ #\space)
                                  (save (seq (not #\space)
                                             (+ any)))))))

                                        
(define (mailbox-read-rfc822 port)
  (bind ((hdr (read-line port)))
    (if (string? hdr)
        (bind ((s e sender date (mailbox-from-line hdr)))
          (if s
              (let ((h (read-headers port)))
                (make <rfc822-message>
                      sender: (parse-email-address sender)
                      recipients: '()
                      headers: h
                      contents: (read-mailbox-body port)))
              (error "Mailbox has bad From header")))
        #f)))
