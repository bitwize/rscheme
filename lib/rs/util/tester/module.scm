(define-module rs.util.tester ()
  (&module
   (import usual-inlines
           rs.sys.threads.manager
           editinp
           util.xml
           mlink
           syscalls
           repl
           regex)
   ;;
   (load "tester.scm")
   (load "unitfile.scm")
   ;;
   (export repl-test-case
           repl-test-case->sxml
           load-unit-test)))

