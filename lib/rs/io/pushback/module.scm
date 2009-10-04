(define-module rs.io.pushback ()
  (&module
   (import usual-inlines)
   (load "pushback.scm")
   (export <pushback-input-port>
           open-input-pushback-port
           input-port-pushback)))
