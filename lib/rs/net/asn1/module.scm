(define-module rs.net.asn1 ()
  (&module
   (import usual-inlines
           rs.net.pem
           tables
           )
   (load "asn1.scm")

   (export
    ber-encode
    ber-decode
    )))

