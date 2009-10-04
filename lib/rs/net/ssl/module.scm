(define-module rs.net.ssl ()
  (&module
   (import usual-inlines)
   (import unixm)
   (import syscalls)
   (import regex)
   (import rs.sys.threads.manager
           rs.util.properties)

   (load "asyncvar.scm")
   (load "sslsocket.scm")
   (export make-sslmgr
           ssl-connect
           mini-munge!
           <ssl-socket>
           has-certificate?
           certificate
           plaintext-in
           plaintext-out
           )))
