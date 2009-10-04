,(use rs.net.pem
      rs.net.md5)

(define (process-auth/DIGEST-MD5 (self <smtp-client-connection>) bail)
  (let ((realm (smtp-realm *smtp-server*))
        (ack #f)
        (rsp '())
        (user #f)
        (auser #f)
        (secret #f)
        (expect #f))
    ;;
    (send-response self '(334) 
                   (vector (make-digest-md5-challenge (name realm))))
    ;;
    (set! ack (read-line (input-port self)))
    (format #t "ACK: ~s\n" ack)
    (if (string=? ack "*")
        (bail '(501 5 7 0) "no AUTH data supplied"))
    ;;
    (handler-case
     (set! rsp (decode-sasl-packet ack))
     ((<condition> condition: e)
      (bail '(501 5 5 2) "could not parse auth request")))
    ;;
    (print rsp)
    ;;
    (if (not (smtp-realm *smtp-server*))
        (bail '(535 5 7 0) "go away (KX)"))
    ;;
    (set! user (pq (cadr (assoc "username" rsp))))
    ;;
    (let* ((ack #f)
           (auser (chap-authenticate 
                   realm
                   user
                   (pq (cadr (assoc "response" rsp)))
                   (lambda (secret)
                     (set! ack (compute-response-value secret
                                                       (name realm)
                                                       rsp
                                                       #t))
                     (compute-response-value secret (name realm) rsp #f)))))
      ;;
      (if (not auser)
          (bail '(535 5 7 0) "go away (JK)"))
      ;;
      (let ((rspauth (pem-encode (string-append "rspauth=" ack))))
        ;;
        (send-response self '(334) (vector rspauth))
        ;; skip the zero-length client answer
        (read-line (input-port self))
        ;;
        ;; discard unauth'd knowledge of client
        ;; (per RFC 2554 section 4)
        (clear-envelope! self)
        (set-auth-user! self auser)
        (respond self '(235 2 7 0) "ok")))))

(define (process-auth/CRAM-MD5 (self <smtp-client-connection>) bail)
  (let ((n (string-append "<rs.net.smtpd.1.0."
                          (make-nonce)
                          "@"
                          (name *smtp-server*)
                          ">")))
    ;(format #t "NONCE ~s\n" n)
    (send-response self '(334) (vector (pem-encode n)))
    (let ((ack (read-line (input-port self))))
      (format #t "<<< ~a\n" ack)
      ;(format #t "REPLY ~s\n" (pem-decode ack))
      ;(format #t "expect: ~s\n" (hmac "foobar" n))
      (let* ((rply (pem-decode ack))
             (brk (string-search rply #\space))
             (username (and brk (substring rply 0 brk)))
             (auth (and brk (substring rply (+ brk 1))))
             (auser (and auth (chap-authenticate
                               (smtp-realm *smtp-server*)
                               username
                               auth
                               (lambda (secret)
                                 (hmac secret n))))))
        ;;
        (if (not auser)
            (bail '(535 5 7 0) "go away (CX)"))
        ;;
        (clear-envelope! self)
        (set-auth-user! self auser)
        (respond self '(235 2 7 0) "ok")))))

;;; [RFC 2104] Section 2

(define (string-xor str k)
  (let ((b (make-string (string-length str))))
    (let loop ((i 0))
      (if (< i (string-length str))
          (begin
            (bvec-set! b i (bitwise-xor (bvec-ref str i) k))
            (loop (+ i 1)))
          b))))

(define (hmac key text)
  ;;
  (let* ((L 16)
         (B 64)
         (Hb md5-binary-digest)
         (Ha md5-digest)
         (k (if (> (string-length key) B)
                (string-append (bvec->string (Hb key))
                               (make-string (- B L) #\x00))
                (string-append key
                               (make-string (- B (string-length key)) 
                                            #\x00)))))
    (Ha (string-append
         (string-xor k #x5C)
         (bvec->string
          (Hb (string-append
               (string-xor k #x36)
               text)))))))


(define (process-auth (self <smtp-client-connection>) parm)
  (call-with-current-continuation
   (lambda (return)
     (define (bail . args)
       (apply respond self args)
       (return))
     ;;
     (cond
      ((string=? parm "DIGEST-MD5") 
       (process-auth/DIGEST-MD5 self bail))
      ((string=? parm "CRAM-MD5") 
       (process-auth/CRAM-MD5 self bail))
      (else
       (bail '(504 5 5 1) "unsupported authentication type"))))))

(register-extended-smtp-feature
 "AUTH"
 "AUTH CRAM-MD5 DIGEST-MD5"
 (list (list "AUTH" (& process-auth))))

(define (make-nonce)
  (to-string (make-uuid)))

(define (make-digest-md5-challenge realm)
  (let ((nonce (make-nonce)))
    (values
     (pem-encode
      (string-join
       #\,
       (map (lambda (p)
              (~ "~a=~a" (car p) (cadr p)))
            `((realm ,(string-append "\"" realm "\""))
              (nonce ,(string-append "\"" nonce "\""))
              (algorithm "md5-sess")
              (charset "utf-8")))))
     nonce)))

(define (kda k s)
  (md5-digest (~ "~a:~a" k s)))

(define (pq x)
  (if (char=? (string-ref x 0) #\")
      (substring x 1 (- (string-length x) 1))
      x))

(define (a1 secret realm resp)
  (string-append
   (bvec->string
    (md5-binary-digest (string-append (pq (cadr (assoc "username" resp)))
                                      ":"
                                      realm
                                      ":"
                                      secret)))
   ":"
   (pq (cadr (assoc "nonce" resp)))
   ":"
   (pq (cadr (assoc "cnonce" resp)))))


(define (compute-response-value secret realm resp rsp?)
  ;;
  (define (a2)
    (string-append (if rsp? "" "AUTHENTICATE")
                   ":"
                   (pq (cadr (assoc "digest-uri" resp)))))
  ;;
  (kda
   (md5-digest (a1 secret realm resp))
   (string-append
    (pq (cadr (assoc "nonce" resp)))
    ":"
    (cadr (assoc "nc" resp))
    ":"
    (pq (cadr (assoc "cnonce" resp)))
    ":"
    (pq (cadr (assoc "qop" resp)))
    ":"
    (md5-digest (a2)))))

(define (decode-sasl-packet enc)
  (map (lambda (p)
         (let ((x (string-search p #\=)))
           (list (substring p 0 x)
                 (substring p (+ x 1)))))
       (string-split (pem-decode enc) #\,)))

(define $rep
  (decode-sasl-packet
   (string-append
    "Y2hhcnNldD11dGYtOCx1c2VybmFtZT0iY2hyaXMiLHJlYWxtPSJlbHdvb2"
    "QuaW5ub3NvZnQuY29tIixub25jZT0iT0E2TUc5dEVRR20yaGgiLG5jPTAw"
    "MDAwMDAxLGNub25jZT0iT0E2TUhYaDZWcVRyUmsiLGRpZ2VzdC11cmk9Im"
    "ltYXAvZWx3b29kLmlubm9zb2Z0LmNvbSIscmVzcG9uc2U9ZDM4OGRhZDkw"
    "ZDRiYmQ3NjBhMTUyMzIxZjIxNDNhZjcscW9wPWF1dGg=")))
#|
"cmVhbG09ImVsd29vZC5pbm5vc29mdC5jb20iLG5vbmNlPSJPQTZNRzl0"
    "RVFHbTJoaCIscW9wPSJhdXRoIixhbGdvcml0aG09bWQ1LXNlc3MsY2hh"
    "cnNldD11dGYtOA==")))

        "username=\"donovan\",realm=\"star.westgate.xynthesis.com\",nonce=\"xxxxxxxxxx\",cnonce=\"VYgqHsDRkMw=\",nc=00000001,qop=\"auth\",digest-uri=\"smtp/localhost\",response=\"b582bd61dfe71a312e6e14e65977e3f3\",charset=\"utf-8\""
|#
