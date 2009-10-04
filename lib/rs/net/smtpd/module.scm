(define-module rs.net.smtpd ()
  (&module
   (import usual-inlines
           rs.sys.threads.manager
           regex
           tables
           syscalls
           rs.net.rfc822
           rs.net.uuid
           rs.util.realm)
   ;;
   (load "boundedline.scm")
   (load "smtpd.scm")
   (load "auth.scm")
   ;;
   (export <smtpd-error>
           <smtp-server> 
           port
           rcpt-domains
           smtp-realm
           smtp-auth-required?
           injector
           set-injector!
           ;;
           run-smtp-server
           ;;
           <smtp-endpoint>
           name
           reject-message
           set-reject-message!
           ;;
           <smtp-domain>
           name
           users
           ;;
           <smtp-message>
           envelope-sender
           envelope-recipients
           content
           ;;
           ;;  This GF will be invoked on the user object
           ;;  returned from the auth realm
           smtp-user-relay-ok?)))

           


