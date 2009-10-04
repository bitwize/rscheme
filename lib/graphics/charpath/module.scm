(define-module graphics.charpath ()
  (&module
   (import usual-inlines)
   (import rs.sys.threads.manager)
   (import syscalls)
   (import rs.util.properties)
   (import unixm)
   (import tables sort)
   (import graphics.geometry)
   (import graphics.fontmgr)
   (import rs.util.msgs))
  
  ;;
  (define-message-table graphics.charpath 604)
  ;;
  (&module
   ;
   (load "charpath.scm")
   (export text->path
           get-charpath
           get-font-info
           load-font-outlines
           load-some-font-outlines)))
