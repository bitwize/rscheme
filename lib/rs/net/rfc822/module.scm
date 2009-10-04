(define-module rs.net.rfc822 ()
  (&module
   (import usual-inlines
           rs.sys.tables
           earley
           regex
           tables
           rs.util.charset
           rs.util.properties)
   ;;
   (load "charset-grammar.scm")
   (load "address.scm")
   ;;
   (export parse-email-address
           <rfc822-address>

           user-name
           domain
           domain-name
           <rfc822-error>
           valid-email-address?)
   ;;
   (load "message.scm")
   (export <rfc822-message>
           parse-rfc822-message
           mailbox-read-rfc822
           sender
           recipients
           headers
           contents)))



   
