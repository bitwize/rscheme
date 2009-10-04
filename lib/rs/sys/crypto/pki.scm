,(use util.xml
      util.xpath
      tables
      rs.util.properties)

;;;

(define (replace-xml-signature-block sxml sig)
  (define (scan x)
    (if (pair? x)
        (if (eq? (car x) 'InlineSignature)
            sig
            (cons (car x) (map scan (cdr x))))
        x))
  (scan sxml))


(define (dsa-sign-xml key sxml)
  (let* ((t (time->string (time) "%Y-%m-%dT%H:%M:%SZ" #f))
         (sigid (to-string (make-uuid)))
         (hash (sha (string-append
                     t
                     "|"
                     (name (public key))
                     "|"
                     (sxml->string (replace-xml-signature-block
                                    sxml
                                    '(InlineSignature))))))
         (sig (dsa-sign key hash)))
    ;;
    (replace-xml-signature-block
     sxml
     `(InlineSignature
       (@ (Id ,sigid)
          (time ,t))
       ;;
       (SignedInfo
        (Reference
         (DigestValue ,(b->s hash))))
       ;;
       (SignatureValue
        (S ,(b->s (car sig)))
        (R ,(b->s (cadr sig))))
       ;;
       (KeyInfo
        (@ (Id ,(name (public key))))
        (KeyValue
         (DSAKeyValue
          (P ,(b->s (dsa-p (public key))))
          (Q ,(b->s (dsa-q (public key))))
          (G ,(b->s (dsa-g (public key))))
          (Y ,(b->s (dsa-y (public key)))))))))))

(define (dsa-verify-xml key sxml)
  (let* ((sig (car (xpath () sxml "//InlineSignature")))
         (payload (replace-xml-signature-block sxml '(InlineSignature)))
         (keyid (xpath-str sig "KeyInfo/@Id"))
         (tstamp (xpath-str sig "@time"))
         (s (s->b (xpath-str sig "SignatureValue/S")))
         (r (s->b (xpath-str sig "SignatureValue/R")))
         (hash (sha (string-append
                     tstamp
                     "|"
                     keyid
                     "|"
                     (sxml->string payload)))))
    (dsa-valid? key hash (list s r))))

(define (pubkeyxml (self <dsa-public-key>))
  `(DSAKeyValue
    (P ,(b->s (dsa-p self)))
    (Q ,(b->s (dsa-q self)))
    (G ,(b->s (dsa-g self)))
    (Y ,(b->s (dsa-y self)))
    (seed ,(b->s (dsa-seed self)))
    (pgenCounter ,(number->string (dsa-counter self)))))
  
(define-method get-dsa-x-enc ((self <dsa-private-key>) gpp)
  (pem-encode
   (des3-encrypt
    (bignum->octet-string (dsa-x self))
    (hash-string->des3-key-schedule (gpp)))))


(define-method get-dsa-x-enc ((self <dsa-encrypted-private-key>) gpp)
  (dsa-x-enc self))


(define-method to-sxml ((self <dsa-private-key>))
  `(KeyInfo
    (@ (Id ,(name (public self))))
    ;;
    (DSAPrivateKeyValue
     (X (@ (encrypt "des3")) 
        ,(dsa-x-enc self)))
    ,(pubkeyxml (public self))))
  
(define-method to-sxml ((self <dsa-public-key>))
  `(KeyInfo
    (@ (Id ,(name self)))
    ,(pubkeyxml self)))

(define (import-sxml-publickey* name dsakeyvalue)
  (make <dsa-public-key>
        name: name
        dsa-p: (s->b (xpath-str dsakeyvalue "P"))
        dsa-q: (s->b (xpath-str dsakeyvalue "Q"))
        dsa-g: (s->b (xpath-str dsakeyvalue "G"))
        dsa-y: (s->b (xpath-str dsakeyvalue "Y"))
        dsa-seed: (s->b (xpath-str dsakeyvalue "seed"))
        dsa-counter: (string->number (xpath-str dsakeyvalue "pgenCounter"))))

(define (import-sxml-publickey keyinfo)
  (import-sxml-publickey* (xpath-str keyinfo "@Id") 
                          (car (xpath () keyinfo "DSAKeyValue"))))

(define (import-sxml-privatekey keyinfo)
  (let ((pub (import-sxml-publickey* (xpath-str keyinfo "@Id") 
                                     (car (xpath () keyinfo "DSAKeyValue")))))
    (if (string=? (xpath-str keyinfo "DSAPrivateKeyValue/X/@encrypt") "des3")
        (make <dsa-encrypted-private-key>
              public: pub
              dsa-x-enc: (pem-decode (xpath-str keyinfo "DSAPrivateKeyValue/X")))
        (make <dsa-encrypted-private-key>
              public: pub
              dsa-x: (s->b (xpath-str keyinfo "DSAPrivateKeyValue/X"))))))

(define *testkey-xml*
#"testkey
<KeyInfo Id="2183e196-f957-48ff-b9f9-f012393764ed">
  <DSAPrivateKeyValue>
    <X>R5M6HQceHbCIt7Z96BDKBrZTCMo=</X>
  </DSAPrivateKeyValue>
  <DSAKeyValue>
    <P>+uU3J1xhpXFIMIk04AiOrM1561Zo9BnOqcgvvUh6xcQmt2bfoT9OFSEADkIwIQgL5eP8hUHRttLnFbrIuWtOsDLO89qYFmSzPNKd09b1yIfyPHLIaPfcDcxxhb63VhEd38fZIote/08=</P>
    <Q>x8geU8gT32u4gwDyWDZ3HQAFwLM=</Q>
    <G>LQkH11QzTo/FB4zMfjOVplq1HzKzbU5kMH1UKdS3xLpw0T3BVptZGa15h5juWB8sD59pJAlftovbCY9uj4HI4ZKKgusbrptdHPUSFj9XqGEL4rgH7lol3dzpnJeT3zAW/RIzvqUAbBM=</G>
    <Y>dh9TeytYA2cCaGg2LLsux6/Bg03qAxRBy0tvAQTmf6qQ7Lu5E1WcSwXl9Uh91bmKL5ORaSL2xYfHe0GKLUQ3q2anJFIraUD0XlvV0FA1WMILIj7Nj3n6W+6Yu/5Ffj+6XwIc8tjoQcY=</Y>
    <seed>BYqG36V0ZNiJ5ublnCA/tY1OVX8FEdHUIw==</seed>
    <pgenCounter>845</pgenCounter>
  </DSAKeyValue>
</KeyInfo>
#"testkey)

(define *testkey* (import-sxml-privatekey (cadr (string->sxml *testkey-xml*))))

(define (make-uuid)
  "beffc1f8-9da3-4801-b2e8-76886c2d3bb9")

(define *test-xml-1* '(foo (p "This is a test")
                           (p "This is another test")
                           (InlineSignature)
                           (p "And here is an appendix")))

(define *test-xml-1s*
  '(foo
    (p "This is a test")
    (p "This is another test")
    (InlineSignature
     (@
      (Id "beffc1f8-9da3-4801-b2e8-76886c2d3bb9")
      (time "2004-09-01T22:57:57Z"))
     (SignedInfo (Reference (DigestValue "yAZp7dOVgp5k4KaJ8IBQuxcSrLc=")))
     (SignatureValue
      (S "v6f8TO8HgDS/lIGRxBoZ79Uc2vI=")
      (R "F8IaMNYUWOkDayneAIS+0PQIBfA="))
     (KeyInfo
      (@ (Id "2183e196-f957-48ff-b9f9-f012393764ed"))
      (KeyValue
       (DSAKeyValue
        (P
         "+uU3J1xhpXFIMIk04AiOrM1561Zo9BnOqcgvvUh6xcQmt2bfoT9OFSEADkIwIQgL5eP8hUHRttLnFbrIuWtOsDLO89qYFmSzPNKd09b1yIfyPHLIaPfcDcxxhb63VhEd38fZIote/08=")
        (Q "x8geU8gT32u4gwDyWDZ3HQAFwLM=")
        (G
         "LQkH11QzTo/FB4zMfjOVplq1HzKzbU5kMH1UKdS3xLpw0T3BVptZGa15h5juWB8sD59pJAlftovbCY9uj4HI4ZKKgusbrptdHPUSFj9XqGEL4rgH7lol3dzpnJeT3zAW/RIzvqUAbBM=")
        (Y
         "dh9TeytYA2cCaGg2LLsux6/Bg03qAxRBy0tvAQTmf6qQ7Lu5E1WcSwXl9Uh91bmKL5ORaSL2xYfHe0GKLUQ3q2anJFIraUD0XlvV0FA1WMILIj7Nj3n6W+6Yu/5Ffj+6XwIc8tjoQcY=")))))
    (p  "And here is an appendix")))


;;;

(define-class <certificate-db-entry> (<object>)
  (properties type: <vector> init-value: '#())
  (name type: <string>)
  (certificate type: <list>))

(define (cert-db-insert! cert)
  (let* ((id (xpath-str cert "Subject/@Id"))
         (e (make <certificate-db-entry>
                  name: id
                  certificate: cert)))
    (if (table-lookup *cert-db* id)
        (error "Duplicate entry: ~s" id))
    (table-insert! *cert-db* id e)
    ;;
    (set-property! e 'public-key
                   (import-sxml-publickey 
                    (car (xpath () cert "Subject/KeyInfo"))))
    (values)))

(define *cert-db* (make-string-table))

;;;

(define (load-cert-db)
  (for-each
   (lambda (f)
     (if (and (not (member f '("." "..")))
              (not (char=? (string-ref f (- (string-length f) 1)) #\~)))
         (cert-db-insert! 
          (cadr (string->sxml (file->string (~ "certdb/~a" f)))))))
   (scandir "certdb")))

(define (save-cert-db)
  (table-for-each
   *cert-db*
   (lambda (h k v)
     (call-with-output-file
         (~ "certdb/~a" k)
       (lambda (port)
         (write-sxml (pretty-printify-xml (certificate v)) port)
         (newline port))))))


        
(define (self-signed? cert)
  (string=? (xpath-str cert "Subject/@Id")
            (xpath-str cert "Issuer/@Id")))

;;;
;;;  Returns the chain of signatories, regardless of any
;;;  trust calculation
;;;

(define (chain-of-issuers id)
  (let ((entry (or (table-lookup *cert-db* id)
                   (error "No knowledge of ~s" id))))
    (if (self-signed? (certificate entry))
        (list entry)
        (cons entry
              (chain-of-issuers (xpath-str (certificate entry) "Issuer/@Id"))))))

;;;
;;;  Verifies that certs are, in fact, signed by who they are claimed
;;;  to be signed by (as computed by chain-of-issuers)
;;;

(define (verify-signatures chain)
  (if (null? (cdr chain))
      ;; verify self-signature
      (let ((ss (car chain)))
        (or (dsa-verify-xml (get-property ss 'public-key)
                            (certificate ss))
            (error "Self-signature verification failure")))
      (let ((issuer (cadr chain))
            (subject (car chain)))
        (if (dsa-verify-xml (get-property issuer 'public-key)
                            (certificate subject))
            (verify-signatures (cdr chain))
            (error "Issued signature verification failure")))))

;;;

(define (passphrase-prompter type name)
  (case type
    ((input)
     (let ((p (current-output-port)))
       (format p "Enter passphrase for <~a> : " name)
       (flush-output-port p)
       (read-line)))
    ((output)
     (let ((p (current-output-port)))
       (let loop ()
         (format p "Enter new passphrase for <~a> : " name)
         (flush-output-port p)
         (let ((a (read-line)))
           (format p "           ~aagain, to verify : "
                   (make-string (string-length name) #\space))
           (let ((b (read-line)))
             (if (string=? a b)
                 a
                 (begin
                   (format p "*** mismatch; try again\n")
                   (loop))))))))))

     
