(define-module rs.net.pem ()
  (&module (import usual-inlines))
  (&module
    (load "pem.scm")
    (export pem-encode
	    random-pem
	    pem-decode)))
