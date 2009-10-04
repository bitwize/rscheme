(define-module graphics.text.layout ()

  ;; [originally `graphics.para' in app/fiesta/para/]

  (&module
   (import usual-inlines
           graphics.device
           graphics.geometry
           util.xml
           util.xpath
           regex
           util.xpath
           rs.util.charset)

   (import graphics.fontmgr
           graphics.afm
           graphics.styles
           tables
           rs.sys.multimethod
           rs.util.properties
           rs.util.quantity))
  ;;

  (define-unit em)
  (define-unit pt)

  (define (scaled->qty x) (/ x 1000))
  (define (qty->scaled x) (inexact->exact (round (* 1000 x))))
  ;;
  
  (define-syntax (debug . items) (values))
  ;(define-syntax (debug . items) (begin . items))

  (&module
   (load "listiter.scm")
   (load "glue.scm")
   (load "data.scm")
   (load "space.scm")
   (load "config.scm")
   (load "nodes.scm")

   (load "badness.scm")
   (load "breakwidth.scm")

   (load "deactivate.scm")
   (load "trybreak.scm")
   (load "deactivate2.scm")
   (load "trybreak2.scm")
   (load "linebreak.scm")

   (load "input.scm")
   (load "output.scm")

   (load "print.scm")
   (load "render.scm")

   (load "vlist.scm")
   ;;
   (load "layout.scm")
   )
   ;;
   
  ;;
  ;;

  (&module
   (export with-line-width
           line-break
           init-style-glue
           text->hlist
           render-text-layout
           <packed-line>
           ;;
           with-config-parameter
           ;;
           render-vlist
           vlist-set
           break-text-into-vlist
           sxml->glue
           make-vskip
           ;;
           make-text-style
           font
           ;;
           layout-text)))
