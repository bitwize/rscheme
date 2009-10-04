(define-module rs.net.console ()
  (&module
   (import usual-inlines
           rs.sys.threads.manager
           rs.util.msgs
           rs.util.logfile
           rs.util.realm
           rs.net.telnet
           editinp
           syscalls
           mlink
           repl)
   ;;
   (load "console.scm")
   ;;
   (export start-console-server
           authentication)))
