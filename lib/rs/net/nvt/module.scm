(define-module rs.net.nvt ()
  (&module
   (import usual-inlines)
   (import rs.sys.threads.manager)
   ;;
   (load "nvt-out.scm")
   (load "nvt-in.scm")
   (load "nvt.scm")
   ;;
   (export open-output-nvt
           open-input-nvt
           open-nvt)))
