(define-module rs.net.telnet ()
  (&module
   ;
   (import usual-inlines
           rs.net.nvt
           rs.util.msgs
           rs.sys.threads.manager))
  ;
  (define-message-table rs.net.telnet 444)
  ;
  (&module
   ;
   (load "telnet.scm")
   (export open-telnet-session
           <telnet-session>

           input-port
           output-port
           telnet-declare-option
           telnet-request-option
           telnet-subnegotiate-option
           telnet-await-subnegotiation
           telnet-await-remote-option-ack

           telnet-get-environment
           telnet-get-terminal-type)))

