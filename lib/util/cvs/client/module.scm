(define-module util.cvs.client ()
  (&module
   (import usual-inlines
           rs.util.properties
           rs.sys.threads.manager 
           syscalls 
           tables regex sort)
   ;;
   (load "client.scm")
   (export cvs-open
           cvs-close
           cvs-log
           cvs-checkout)
   ;;
   ))
