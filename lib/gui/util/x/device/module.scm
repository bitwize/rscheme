(define-module gui.util.x.device ()
  (&module
   (import usual-inlines
           tables)
   (import rs.util.properties
           rs.util.msgs
           gui.x)
   (import graphics.device
           graphics.fontmgr
           graphics.afm
           graphics.geometry)
   ;;
   (load "driver.scm")
   ;;
   (export configure-x-graphics-device
           with-x-graphics-device
           with-x-redraw)))
  
