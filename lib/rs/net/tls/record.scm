,(use rs.sys.threads.manager
      rs.util.pack
      syscalls)

#|
      Client                                               Server
      _____________________                   ___________________
      ClientHello                  -------->
                                                      ServerHello
                                                     Certificate*
                                               ServerKeyExchange*
                                              CertificateRequest*
                                   <--------      ServerHelloDone
      Certificate*
      ClientKeyExchange
      CertificateVerify*
      [ChangeCipherSpec]
      Finished                     -------->
                                               [ChangeCipherSpec]
                                   <--------             Finished
      Application Data             <------->     Application Data

             Fig. 1 - Message flow for a full handshake
|#
  


#|
  [00] 16 >     RECORD          content-type            (= handshake)
  [01] 03 >     RECORD          protocol-major
  [02] 01 >     RECORD          protocol-minor
  [03] 00 5f >  RECORD          fragment-length

  [05] 01 >       Handshake     msg_type                (= client_hello)
  [06] 00 00 5b > Handshake     length  

  [09] 03 >       ClientHello   client_version.major
  [0a] 01 >       ClientHello   client_version.minor
                  ClientHello   random...
  [0b] 3f >       Random        gmt_unix_time
  [0c] 56 >
  [0d] 09 >
  [0e] 4d >

  [0f] 4b >       Random        random_bytes[28]
  [10] f1 >
  [11] b4 >
  [12] 39 >
  [13] 9f >
  [14] 78 >
  [15] 5b >
  [16] ab >
  [17] 2e >
  [18] 6c >
  [19] a1 >
  [1a] 08 >
  [1b] bf >
  [1c] 62 >
  [1d] b3 >
  [1e] aa >
  [1f] d9 >
  [20] d9 >
  [21] b2 >
  [22] 09 >
  [23] 38 >
  [24] 8e >
  [25] 5c >
  [26] f6 >
  [27] 48 >
  [28] a4 >
  [29] ae >
  [2a] d8 >
                  ClientHello   session_id
  [2b] 00 >                                     length

                  ClientHello   cipher_suite
  [2c] 00 34 >                                  length

  [2e] 00 39 >
  [30] 00 38 >
  [32] 00 35 >
  [34] 00 16 >
  [36] 00 13 >
  [38] 00 0a >
  [3a] 00 33 >
  [3c] 00 32 >
  [3e] 00 2f >
  [40] 00 66 >
  [42] 00 05 >
  [44] 00 04 >
  [46] 00 63 >
  [48] 00 62 >
  [4a] 00 61 >
  [4c] 00 15 >
  [4e] 00 12 >
  [50] 00 09 >
  [52] 00 65 >
  [54] 00 64 >
  [56] 00 60 >
  [58] 00 14 >
  [5a] 00 11 >
  [5c] 00 08 >
  [5e] 00 06 >
  [60] 00 03 >
  [62] 01 >       ClientHello   compression_method.length
  [63] 00 >       ClientHello   compression_method[0]
|#

(define-class <tls-record> (<object>)
  (content-type type: <fixnum>)
  (protocol-major type: <fixnum>)
  (protocol-minor type: <fixnum>)
  (fragment type: <byte-vector>))

(define (make-handshake-record type body)
  (make <tls-record>
         content-type: $handshake-content
         protocol-major: 3
         protocol-minor: 1
         fragment: (vflatten
                    (vector
                     (pack-segment*
                      '(struct
                        (message-type type: (uint 1))
                        (message-length type: (uint 3)))
                      `((message-type ,type)
                        (message-length ,(vlength body))))
                     body))))


(define (make-session-id)
  `((length 4) (id #(1 2 3 4))))

(define-method vlength ((self <byte-vector>))
  (bvec-length self))

(define-method vlength ((self <vector>))
  (reduce + 0 (map vlength (vector->list self))))

(define-method vflatten ((self <byte-vector>))
  self)

(define-method vflatten ((self <vector>))
  (string->byte-vector
   (call-with-output-string
    (lambda (port)
      (writev port self)))))

(define (string->byte-vector s)
  (let ((b (bvec-alloc <byte-vector> (string-length s))))
    (bvec-copy b 0 s 0 (string-length s))
    b))

(define $handshake-type/client-hello 2)
(define $handshake-type/server-hello 2)
(define $handshake-type/certificate 11)

(define (make-certificate)
  (let ((cert (pem-read-certificate "sample/server-cert.pem")))
    (make-handshake-record
     $handshake-type/certificate
     (pack-segment*
      *CERTIFICATE*
      `((length ,(+ (bvec-length cert) 3))
        (certificate-list 
         #(((length ,(bvec-length cert))
            (certificate ,cert)))))))))

#|
(define (make-server-key-exch)
  ;;
  (define (gen-p-value)
    123)
  (define (gen-g-value)
    123)
  (define (gen-ys-value)
    123)
  ;;
  (make-handshake-record
   (pack-segment*
    *SERVER-KEY-EXCHANGE*
    '((diffie-hellman
       ((params
         ((DH-p ,(gen-p-value))
          (DH-g ,(gen-g-value))
          (DH-Ys ,(gen-ys-value))))
        (signed-params
         ((md5-hash ,(...))
          (sha-hash ,(...))))))))))
|#

(define (make-server-hello)
  (make-handshake-record
   $handshake-type/server-hello
   (pack-segment*
    *SERVER-HELLO*
    `((protocol-major 3)
      (protocol-minor 1)
      (random
       ((gmt-unix-time ,(unix-time))
        (random-bytes ,(make-random-bytes))))
      (session-id ,(make-session-id))
      (cipher-suite TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA)
      (compression-method null)))))
#|
    (make <tls-record>
          content-type: $handshake-content
          protocol-major: 3
          protocol-minor: 1
          fragment: (vflatten
                     (vector
                      (pack-segment*
                       '(struct
                         (message-type type: (uint 1))
                         (message-length type: (uint 3)))
                       `((message-type 2)
                         (message-length ,(vlength h))))
                      h)))))
|#
(define (make-random-bytes)
  (vector-map (lambda (k)
                (random 256))
              (list->vector (range 28))))

(define (unix-time)
  (inexact->exact (time->epoch-seconds (time))))

   

(define (tls-write-record port (r <tls-record>))
  (format #t "writing:\n")
  (pp (tls-parse r))
  (let (((b <byte-vector>) (fragment r)))
    (write-string port 
                  (pack-string u8: (content-type r)
                               u8: (protocol-major r)
                               u8: (protocol-minor r)
                               u16/b: (bvec-length b)))
    (write-bytes port b 0 (bvec-length b))
    (flush-output-port port)))

(define (read-bvec port len)
  (let* ((s (read-string port len))
         (x (bvec-alloc <byte-vector> (string-length s))))
    (bvec-copy x 0 s 0 (string-length s))
    x))
    

(define (tls-read-fragment port)
#|
  (let loop ((i 0))
    (let ((ch (read-char port)))
      (if (eof-object? ch)
          (values)
          (begin
            (format #t "  [~02x] ~02x >\n" i (char->integer ch))
            (loop (+ i 1))))))
|#
  (bind ((content-type (bvec-read-unsigned-8 (read-bvec port 1) 0)))
    (if (not (memq content-type '(20 21 22 23)))
        (error "incomprehensible content-type: #x~02x" content-type)
        (bind ((protocol-major protocol-minor (with-unpack
                                               (read-bvec port 2)
                                               (u8: major
                                                    u8: minor)
                                               (values major minor)))
               (length (with-unpack (read-bvec port 2) (u16/b: l) l))
               (fragment (read-bvec port length)))
          (make <tls-record>
                content-type: content-type
                protocol-major: protocol-major
                protocol-minor: protocol-minor
                fragment: fragment)))))

(define (tls-parse (self <tls-record>))
  (case (content-type self)
    ((22) (tls-parse-handshake self))
    (else (error "unknown content-type"))))

(define (tls-parse-handshake (self <tls-record>))
  (with-unpack (fragment self)
               (u8: msg-type
                u8: length-high
                u16/b: length-low)
    (format #t "fragment length = ~d\n" (bvec-length (fragment self)))
    (format #t "expected length = ~d\n" (+ (* length-high #x10000) length-low 4))
    (if (not (= (+ (* length-high #x10000) length-low 4)
                (bvec-length (fragment self))))
        (error "length mismatch"))
    (let ((s (make <segment>
                   segment-data: (fragment self)
                   segment-offset: 4
                   segment-length: (+ (* length-high
                                         #x10000)
                                      length-low))))
      (case msg-type
        ((1) (tls-parse-client-hello s))
        ((2) (tls-parse-server-hello s))
        ((11) (unpack-segment s *CERTIFICATE*))
        (else (error "unknown handshake msg-type: ~s" msg-type))))))


(define (tls-parse-server-hello (src <segment>))
  (unpack-segment src *SERVER-HELLO*))

(define (tls-parse-client-hello (src <segment>))
  (unpack-segment src *CLIENT-HELLO*))

(define-constant $change-cipher-spec-content 20)
(define-constant $alert-content 21)
(define-constant $handshake-content #x16)
(define-constant $application-data-content 23)

(define (process-tls-connection sock)
  (format #t "go: ~s\n" sock)
  (handler-case
   (begin
     #|
     (tls-write-record (output-port sock)
                       (make <tls-record>
                             content-type: $handshake-content
                             protocol-major: 3
                             protocol-minor: 1
                             fragment: "foobar"))
     |#
     (let ((f (tls-read-fragment (input-port sock))))
       (print f)
       (let ((x (tls-parse f)))
         (format #t "Parsed: ~#*@60s\n" x)
         (pp x)
         (tls-write-record (output-port sock) (make-server-hello))
         (tls-write-record (output-port sock) (make-certificate))
         ;(tls-write-record (output-port sock) (make-server-key-exch))
         ;(tls-write-record (output-port sock) (make-certificate-request))
         (for-each
          (lambda (t)
            (let ((f (tls-read-fragment (input-port sock))))
              (print f)
              (let ((x (tls-parse f)))
                (format #t "~s parsed: ~#*@60s\n" t x)
                (pp x))))
          '(ClientKeyExchange
            CertificateVerify
            ChangeCipherSpec
            Finished)))))
   ((<condition> condition: c)
    (format #t "TLS error: ~a" c)))
  (close sock))

(define (tls-server port)
  (let ((sock (open-server-socket port)))
    (let loop ()
      (let ((c (accept-client sock)))
        (thread-resume
         (make-thread
          (lambda ()
            (process-tls-connection c))))
        (loop)))))


#|
How to test...

star$ cd /tmp/openssl-0.9.7b/apps
star$ ./openssl s_client -debug -tls1 -CAfile /u/donovan/p/sf/library/dev/rs/net/tls/sample/server-cert.pem -connect localhost:9090 

|#
