(define-module rs.net.smtp ()
  (&module
   (import usual-inlines
           regex
           syscalls
           unixm
           rs.net.rfc822
           rs.util.msgs
           rs.util.properties
           rs.sys.threads.manager)
   ;;
   (load "smtp.scm")
   ;;
   (export open-smtp
           <smtp-connection>
           time->rfc-822
           ;;
           send-one-email
           headers
           recipients
           contents
           sender
)))
