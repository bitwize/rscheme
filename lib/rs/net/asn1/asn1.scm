
(define-method to-string ((self <substring>))
  (with-module
      precore
    (substring (basis self)
               (offset self)
               (+ (offset self) (length self)))))

(define-method to-substring ((self <substring>))
  self)

(define-method to-substring ((self <string>))
  (make <substring>
        basis: self
        offset: 0
        length: (string-length self)
        string-length: (string-length self)))

(define-method write-object ((self <substring>) port)
  (write-object (to-string self) port))

(define xx
  (pem-decode
   (string-join
    ""
    '("MIHsAmkAn2/rqoYWaic3Q56hZ0iLGIPbL0WVHTVgrR7G2xA39+XmFTweiMAX1FGy"
      "Uw6yVemDWgnpQL71wPp758Sn+9n/VlN6W6BNQeOl5FW/PVIzDNJWf4ft6xdCtvux"
      "ZKxYeynfXeXvHKvdClkCFQDGb28rwgxT4NRMkCgWFa6POIJp7QJobou91JxW+WIa"
      "KNY8ybjlitwzQVyq1h39XR7WSzHg4p1h1gp9d8uJ6CGjOZPlyr2z3KsXXeR/opUu"
      "djqIezici2/vq99ECCEUI+oc5p6UkmVhK4nRt+Nm/fbJKEajOaEKJAEv8OmDl8w="))))

(define (t #optional (s default: xx))
  (to-substring s))

(define-macro (define-asn-glue args . body)
  `(define-safe-glue ,args
     properties: ((other-h-files "ber.h")
                  (other-libs "gmp")
                  (other-c-files "ber_parse_rec.c"
                                 "ber_encode_rec.c"))
     type-handler: (<substring>
                    (direct-instance? <substring>)
                    ("unsigned char *~a"
                     "(unsigned char *)PTR_TO_DATAPTR(gvec_ref(~a,SLOT(0)))"
                     "~a_base")
                    ("unsigned ~a"
                     "fx2int(gvec_ref(~a,SLOT(1)))"
                     "~a_offset")
                    ("unsigned ~a"
                     "fx2int(gvec_ref(~a,SLOT(2)))"
                     "~a_length"))
     ,@body))

(define-class <asn-parse-error> (<condition>)
  code
  place
  arg)

(define-class <asn-field> (<object>) :abstract)


(define-class <asn-other-field> (<asn-field>)
  tag
  contents)

(define-class <asn-printable-string> (<asn-field>)
  contents)

(define-class <asn-utctime> (<asn-field>)
  contents)

(define-class <asn-bit-string> (<asn-field>)
  (skipbits type: <fixnum>)
  (contents type: <substring>))

(define-class <asn-set> (<asn-field>)
  (membership type: <vector>))

(define-class <asn-oid> (<asn-field>)
  (hash-code type: <fixnum> init-value: -1)
  (oid type: <vector>)
  (name init-value: #f))

(define-method initialize ((self <asn-oid>))
  (if (eq? (hash-code self) -1)
      (bind ((len hash (ber-encode-oid (make-dequeue) (oid self))))
        (set-hash-code! self hash)))
  self)

(define-method equal? ((a <asn-oid>) (b <asn-oid>))
  (equal? (oid a) (oid b)))

(define *symbolic-oid-table* (make-table))

(define-method to-string ((self <asn-oid>))
  (if (name self)
      (to-string (name self))
      (cond
       ((built-up-name self)
        => (lambda (n)
             (set-name! self n)
             (to-string n)))
       (else
        (string-join #\. (map number->string (vector->list (oid self))))))))

(define (built-up-name (self <asn-oid>))
  (cond
   ((table-lookup *symbolic-oid-table* self) => name)
   ((parent-oid self)
    => (lambda (p)
         (string-append (to-string p)
                        "."
                        (number->string 
                         (vector-ref 
                          (oid self)
                          (- (vector-length (oid self)) 1))))))
   (else #f)))

(define-method write-object ((self <asn-oid>) port)
  (format port "#[<asn-oid> ~a" 
          (string-join #\. (map number->string (vector->list (oid self)))))
  ;;
  (if (name self)
      (format port " :~a" (name self))
      (cond
       ((built-up-name self)
        => (lambda (n)
             (format port " =~a" n)))))
  ;;
  (format port "]"))

;;;  This allows us to evaluate
;;;             (oid 1 3 6)             => #[<asn-oid> 1.3.6 :dod]
;;;

(define-method oid ((self <fixnum>) #rest r)
  (let ((o (make <asn-oid>
                 oid: (list->vector (cons self r)))))
    (or (table-lookup *symbolic-oid-table* o) o)))


(define-method write-object ((self <asn-printable-string>) port)
  (format port "#[<asn-printable-string> ~s]" (to-string (contents self))))

(define-method write-object ((self <asn-utctime>) port)
  (format port "#[<asn-utctime> ~s]" (to-string (contents self))))

(define-method string-length ((self <asn-bit-string>))
  (- (* 8 (string-length (contents self))) (skipbits self)))

(define-method write-object ((self <asn-bit-string>) port)
  (format port "#[<asn-bit-string> (~d bits)]" (string-length self)))


(define-method write-object ((self <asn-set>) port)
  (format port "#[<asn-set>")
  (vector-for-each
   (lambda (m)
     (write-char #\space port)
     (write-object m port))
   (membership self))
  (format port "]"))

(define $asn-classes (vector #|  0 |# <asn-parse-error>
                             #|  1 |# <asn-other-field>
                             #|  2 |# <asn-oid>
                             #|  3 |# <asn-set>
                             #|  4 |# <asn-printable-string>
                             #|  5 |# <asn-utctime>
                             #|  6 |# <asn-bit-string>
                      ))

;(define *symbolic-oid-table* (make-table))

;;;


(define-method write-object ((self <asn-other-field>) port)
  (format port "#[<asn-other> ~02x (~d bytes)]"
          (tag self)
          (string-length (contents self))))

(define-method display-object ((self <asn-parse-error>) port)
  (format port "*** ASN.1 parse error [~04d]\n" (code self))
  (format port "*** at: ~s\n" (place self))
  (if (arg self)
      (format port "*** info: ~s\n" (arg self))))

(define-asn-glue (ber-encode-header accum (tag <raw-int>) (len <raw-int>))
{
  REG0 = int2fx( ber_encode_header( accum, tag, len ) );
  RETURN1();
})

(define-asn-glue (ber-encode-fixnum accum (data <fixnum>))
{
  REG0 = int2fx( ber_encode_fx( accum, data ) );
  RETURN1();
})

(define-asn-glue (ber-encode-oid accum (data <vector>))
{
  obj h;
  REG0 = int2fx( ber_encode_oid( accum, data, &h ) );
  REG1 = h;
  RETURN(2);
})

(define-asn-glue (ber-encode-string accum (s <string>))
{
  REG0 = int2fx( ber_encode_str( accum, s ) );
  RETURN1();
})

(define-method ber-encode* ((self <fixnum>) accum)
  (ber-encode-fixnum accum self))

(define-method ber-encode* ((self <string>) accum)
  (ber-encode-string accum self))

(define-method ber-encode* ((self <vector>) accum)
  (if (and (> (vector-length self) 0)
           (eq? (vector-ref self 0) 'oid))
      (error "not implemented")
      (let ((q (make-dequeue)))
        (let loop ((i 0)
                   (n 0))
          (if (< i (vector-length self))
              (loop (+ i 1)
                    (+ n (ber-encode* (vector-ref self i) q)))
              (let ((n (+ n (ber-encode-header q #x30 n))))
                (dequeue-push-back! accum (dequeue-state q))
                n))))))

(define (ber-encode item)
  (let ((q (make-dequeue)))
    (ber-encode* item q)
    (let ((p (open-output-string)))
      ;;
      (define (flatten s)
        (if (string? s)
            (write-string p s)
            (vector-for-each flatten s)))
      ;;
      (flatten (dequeue-state q))
      (get-output-string p))))




(define-asn-glue (ber-parse (src <substring>))
  literals: ((& $asn-classes))
{
  unsigned char *p = src_base + src_offset;
  unsigned char *lim;
  obj orig = raw_src;

  lim = p + src_length;
  REG0 = ber_parse_rec( src_base,
                        &p,
                        lim,
                        raw_src,
                        TLREFB(0) );
  if (p < lim) {
    REG1 = make4( CLASSOF_PTR( orig ),
                  gvec_ref( orig, SLOT(0) ),
                  int2fx( p - src_base ),
                  int2fx( lim - p ),
                  int2fx( lim - p ) );
    RETURN(2);
  } else {
    RETURN1();
  }
})

(define *snmp-packet*
  (list->string
   (map integer->char
        '(48 41 2 1 0 4 6 112 117 98 108 105 99 160 28 2 4 73 66 189
             164 2 1 0 2 1 0 48 14 48 12 6 8 43 6 1 2 1 1 1 0 5 0))))

(define (ber-parse-sequence (src <substring>))
  (let ((q (make-dequeue)))
    (let loop ((src src))
      (bind ((item rest (ber-parse src)))
        (dequeue-push-back! q item)
        (if rest
            (loop rest)
            (dequeue-state q))))))


;;;  0x31  Set
;;;  0x17  UTCTime

;;;
;;;  BER is somewhat flexible on the writer's side; there are usually many
;;;      ways to encode a value in BER, e.g., the integer 1 can be
;;;      coded as (04 01 01) or (04 02 00 01)
;;;
;;;  DER is not flexible; there is exactly one way to encode each
;;;      value (this is the basis of hash validation; a unique
;;;      representation means two computations that produce the same
;;;      result must arrive at the same encoding, and hence the
;;;      hash of the encoding will be equal)

(define (indent n)
  (make-string (* n 3) #\space))

(define-method print-decoding ((self <vector>) depth)
  (format #t "~aSEQUENCE\n" (indent depth))
  (vector-for-each
   (rcurry print-decoding (+ depth 1))
   self))

(define-method print-decoding ((self <string>) depth)
  (format #t "~aOCTETSTRING          :~#*@30s\n" (indent depth) self))

(define-method print-decoding ((self <substring>) depth)
  (format #t "~aOCTETSTRING          :~#*@30s\n" (indent depth) (to-string self)))

(define-method print-decoding ((self <asn-printable-string>) depth)
  (format #t "~aPRINTABLESTRING      :~#*@30s\n" 
          (indent depth) 
          (to-string (contents self))))

(define-method print-decoding ((self <asn-utctime>) depth)
  (format #t "~aUTCTIME              :~a\n" 
          (indent depth) 
          (to-string (contents self))))

(define-method print-decoding ((self <integer>) depth)
  (format #t "~aINTEGER              :~s\n" (indent depth) self))

(define-method print-decoding ((self <asn-oid>) depth)
  (format #t "~aOBJECT               :~a\n"
          (indent depth)
          (to-string self)))

(define-method print-decoding ((self <asn-bit-string>) depth)
  (format #t "~aBIT STRING           (~d bits)\n" 
          (indent depth) 
          (string-length self)))

(define-method print-decoding ((self <asn-set>) depth)
  (format #t "~aSET\n" (indent depth))
  (vector-for-each
   (rcurry print-decoding (+ depth 1))
   (membership self)))

(define-method print-decoding ((self <boolean>) depth)
  (if self
      (format #t "~aTRUE?\n" (indent depth))
      (format #t "~aNULL\n" (indent depth))))

(define-method print-decoding ((self <object>) depth)
  (format #t "~aOTHER ~s\n" (indent depth) self))

  

;;;

(define (parent-oid (self <asn-oid>))
  (if (<= (vector-length (oid self)) 2)
      #f
      (bind ((vparent (subvector (oid self) 0 (- (vector-length (oid self)) 1)))
             (len hash (ber-encode-oid (make-dequeue) vparent))
             (p (make <asn-oid>
                      hash-code: hash
                      oid: vparent)))
        (or (table-lookup *symbolic-oid-table* p) p))))
            


(for-each
 (let ((spaces (with-module regex (reg-expr->proc '(+ space)))))
   (lambda (entry)
     (bind ((parts (string-split entry spaces))
            (vec (list->vector 
                  (map string->number (string-split (car parts) #\.))))
            (a (make <asn-oid>
                     oid: vec
                     name: (cadr parts))))
       (table-insert! *symbolic-oid-table* a a))))
 '("1.2                                 iso.member-body"
   "1.2.840                             us"
   "1.2.840.113539                      rsadsi"
   "1.2.840.113539.1                    pkcs"
   "1.2.840.113549.1.1                  pkcs-1"
   "1.2.840.113549.1.1.1                rsaEncryption"
   "1.2.840.113549.1.1.4                md5WithRSAEncryption"
   "1.2.840.113549.1.1.5                sha1WithRSAEncryption"

   ;; X.500 attribute types
   "2.5                                 joint-iso-itu-t.ds"
   "2.5.4                               attributeType"
   "2.5.4.0                             objectClass"
   "2.5.4.1                             aliasedEntryName"
   "2.5.4.2                             knowldgeinformation"
   "2.5.4.3                             commonName"
   "2.5.4.4                             surname"
   "2.5.4.5                             serialNumber"
   "2.5.4.6                             countryName"
   "2.5.4.7                             localityName"
   "2.5.4.8                             stateOrProvinceName"
   "2.5.4.9                             streetAddress"
   "2.5.4.10                            organizationName"
   "2.5.4.11                            organizationalUnitName"
   "2.5.4.12                            title"
   "2.5.4.13                            description"
   "2.5.4.14                            searchGuide"
   "2.5.4.15                            businessCategory"
   "2.5.4.16                            postalAddress"
   "2.5.4.17                            postalCode"
   "2.5.4.18                            postOfficeBox"
   "2.5.4.19                            physicalDeliveryOfficeName"
   "2.5.4.20                            telephoneNumber"
   "2.5.4.21                            telexNumber"
   "2.5.4.22                            teletexTerminalIdentifier"
   "2.5.4.23                            facsimileTelephoneNumber"
   "2.5.4.24                            x121Address"
   "2.5.4.25                            internationalISDNNumber"
   "2.5.4.26                            registeredAddress"
   "2.5.4.27                            destinationIndicator"
   "2.5.4.28                            preferredDeliveryMethod"
   "2.5.4.29                            presentationAddress"
   "2.5.4.30                            supportedApplicationContext"
   "2.5.4.31                            member"
   "2.5.4.32                            owner"
   "2.5.4.33                            roleOccupant"
   "2.5.4.34                            seeAlso"
   "2.5.4.35                            userPassword"
   "2.5.4.36                            userCertificate"
   "2.5.4.37                            cACertificate"
   "2.5.4.38                            authorityRevocationList"
   "2.5.4.39                            certificateRevocationList"
   "2.5.4.40                            crossCertificatePair"
   "2.5.4.41                            name"
   "2.5.4.42                            givenName"
   "2.5.4.43                            initials"
   "2.5.4.44                            generationQualifier"
   "2.5.4.45                            uniqueIdentifier"
   "2.5.4.46                            dnQualifier"
   "2.5.4.47                            enhancedSearchGuide"
   "2.5.4.48                            protocolInformation"
   "2.5.4.49                            distinguishedName"
   "2.5.4.50                            uniqueMember"
   "2.5.4.51                            houseIdentifier"
   "2.5.4.52                            supportedAlgorithms"
   "2.5.4.53                            deltaRevocationList"
   "2.5.4.58                            attributeCertificate"
   "2.5.4.65                            pseudonym"

   "1.3                                 iso.identified-organization"
   "1.3.6                               dod"
   "1.3.6.1                             internet"
   "1.3.6.1.2                           mgmt"
   "1.3.6.1.2.1                         mib-2"
   "1.3.6.1.2.1.1                       system"

   "1.3.6.1.2.1.1.1                     sysDescr"
   "1.3.6.1.2.1.1.2                     sysObjectID"
   "1.3.6.1.2.1.1.3                     sysUpTime"
   "1.3.6.1.2.1.1.4                     sysContact"
   "1.3.6.1.2.1.1.5                     sysName"
   "1.3.6.1.2.1.1.6                     sysLocation"
   "1.3.6.1.2.1.1.7                     sysServices"
   ))

#| 

  Various Web OID references...

   http://asn1.elibel.tm.fr/cgi-bin/oid/display?oid=1.2.840.113549.1.1&action=display
   http://www.alvestrand.no/objectid/1.2.840.113549.1.1.html
|#


(define (ber-decode str)
  (ber-parse (to-substring str)))
