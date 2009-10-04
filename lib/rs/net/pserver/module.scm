(define-module rs.net.pserver ()
  (&module
   (import usual-inlines
           rs.sys.threads.manager
           rs.util.msgs
           rs.util.logfile
           rs.util.charset
           rs.util.properties
           rs.net.httpd
           rs.util.text
           sort
           paths
           tables
           syscalls)
   ;;
   (load "pserver.scm")
   (load "auth.scm")
   (load "commands.scm")
   ;;
   ;(import app.sourcebase)
   ;;
   (load "sb.scm")
   ;;
   (export start-cvs-server)))
