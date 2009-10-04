,(use rs.net.sha1
      rs.net.pem)

;(define $to "dkolbly@yahoo.com")
(define $to "dkolbly@gmail.com")
;(define $to "sa-test@sendmail.net")
;(define $to "dk@crynwr.com")
;(define $to "BoJoBB@www.skylist.net")
;(define $to "dktest@temporary.com")

(define (signed-email (self <rfc822-message>) 
                      #key 
                      private-keyfile
                      domain
                      selector)
  (call-with-output-file
      "/tmp/t1"
    (lambda (p)
      ;;
      (for-each
       (lambda (h)
         (format p "~a: ~a\r\n" (car h) (cdr h)))
       (headers self))
      ;;
      (write-string p "\r\n")
    ;;
      (for-each
       (lambda (l)
         (write-string p l)
         (write-string p "\r\n"))
       (string-split (contents self) #\newline))))
  ;;
  (set-headers! self
                (cons
                 (rfc822-wrapped
                  "DomainKey-Signature"
                  (~ "a=rsa-sha1; h=~a; c=nofws; b=~a; d=rscheme.org; q=dns; s=~a"
                     (string-join ": " (map car (headers self)))
                     (compute-signature/nofws self private-keyfile)
                     selector))
                 (headers self)))
  self)

(define (compute-signature/nofws (self <rfc822-message>) private-keyfile)
  (bind ((out in p (port->run->port "openssl" "dgst"
                                    "-sign"
                                    private-keyfile
                                    "-sha1"))
         (fws (reg-expr->proc '(+ (or #\tab #\nl #\cr #\space)))))
    ;;
    ;; write the headers in order, with folding whitespace removed
    ;;
    (call-with-output-file
        "/tmp/t1.nofws"
      (lambda (q)
        (for-each
         (lambda (h)
           (let ((l (format #f "~a:~a\r\n" 
                            (car h)
                            (string-join "" (string-split (cdr h) fws)))))
             (format #t "SIGN ~s\n" l)
             (write-string q l)
             (write-string out l)))
         (headers self))
        ;;
        (format #t "SIGN CRLF\n")
        (write-string out "\r\n")
        (write-string q "\r\n")
        ;;
        (for-each
         (lambda (l)
           (write-string out l)
           (write-string q l)
           (format #t "SIGN ~s+CRLF\n" l)
           (write-string out "\r\n")
           (write-string q "\r\n"))
         (reverse
          (pop-empty
           (reverse
            (map (lambda (l)
                   (string-join "" (string-split l fws)))
                 (string-split (contents self) #\newline))))))))
        ;;
    (close-output-port out)
    (chunkify (pem-encode (port->string in)))))

(define (compute-signature/simple (self <rfc822-message>) private-keyfile)
  (bind ((out in p (port->run->port "openssl" "dgst"
                                    "-sign"
                                    private-keyfile
                                    "-sha1"))
         (fws (reg-expr->proc '(+ (or #\tab #\nl #\cr #\space)))))
    ;;
    ;; write the headers in order, with folding whitespace removed
    ;;
    (for-each
     (lambda (h)
       (let ((l (format #f "~a: ~a\r\n" (car h) (cdr h))))
         (format #t "SIGN ~s\n" l)
         (write-string out l)))
     (headers self))
    ;;
    (format #t "SIGN CRLF\n")
    (write-string out "\r\n")
    ;;
    (for-each (lambda ((l <string>))
                (format #t "SIGN ~s+CRLF\n" l)
                (write-string out l)
                (write-string out "\r\n"))
              (string-split (contents self) #\newline))
    (close-output-port out)
    (chunkify (pem-encode (port->string in)))))

(define (chunkify l)
  (string-join
   #\space
   (string-split/including l (lambda (s i)
                               (if (> (+ i 32) (string-length s))
                                   #f
                                   (values (+ i 16) (+ i 32)))))))

(define (pop-empty l)
  (if (and (pair? l) (string=? (car l) ""))
      (pop-empty (cdr l))
      l))
    
(define (tdk)
  (make <rfc822-message>
        sender: (parse-email-address "confirm@rscheme.org")
        recipients: (list (parse-email-address $to))
        headers: (list
                  (cons "Date" (time->rfc-822 (time)))
                  (cons "Subject" "Testing DomainKeys #1")
                  (cons "From" "Confirmation Dude <confirm@rscheme.org>")
                  (cons "Message-Id"
                        (~ "<~a@rscheme.org>"
                           (sha1-digest
                            (time->rfc-822 (time)))))
                  (cons "To" $to))
        contents: (call-with-output-string
                   (lambda (p)
                     (format p "This is a test of my domain-keys implementation.\n")
                     (format p "If this comes back OK, I'll be sorta happy.\n")))))
                     
(define (rfc822-wrapped hdr text)
  (cons hdr
        (call-with-output-string
         (lambda (port)
           (let loop ((i (+ 3 (string-length hdr)))
                      (sofar 0)
                      (words (string-split text #\space)))
             (if (null? words)
                 (values)
                 (let ((n (string-length (car words))))
                   (if (and (> sofar 0)
                            (> (+ i n) 72))
                       (begin
                         (write-string port "\r\n        ")
                         (loop 8 0 words))
                       (begin
                         (if (> sofar 0)
                             (write-char #\space port))
                         (write-string port (car words))
                         (loop (+ i n) (+ sofar n) (cdr words)))))))))))

(define (t)
  (signed-email 
   (tdk)
   domain: "rscheme.org"
   selector: "confirm"
   private-keyfile: "/u/donovan/p/rscheme-web/state/static/domainkey.private"))

(define (all-the-way)
  (let ((p (open-smtp host: 
                      ;"209.246.26.21"
                      ;"68.142.225.40"
                      ;"192.203.178.2"
                      "67.67.204.57"
                      )))
    (send-one-email p (t))
    (close p)))

