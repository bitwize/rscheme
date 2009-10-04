(define-module rs.net.uuid ()
  (&module
   (import usual-inlines
           rs.net.md5)
   (load "uuid.scm")
   
   (export make-uuid
           <uuid>)))

