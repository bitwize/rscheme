(define-module rs.util.reify ()
  (&module
   (import usual-inlines
           tables
           compiler)
   
   (load "reify.scm")
   
   (export <reified-expression>
           expr-form
           expr-source
           expr-getter
           expr-setter
           reify!)))

  
