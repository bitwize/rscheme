(define-module rs.util.pstoregc ()
  (&module
   (import rs.lang)
   (import iolib paths)
   (import syscalls unixm regex)
   ;;
   (import rs.sys.threads.manager)
   (import rs.sys.threads.shell)
   (import rs.db.lss rs.db.rstore)
   ;;
   (load "scanmgr.scm")
   (load "simple.scm")
   ;
   (export init-compaction
           did-commit)))

