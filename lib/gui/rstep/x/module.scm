;(load "pixmap-cache.scm")

(define-module gui.rstep.x ()
  (&module
   (import usual-inlines tables objsys)
   (import gui.x)
   (import gui.util.x)
   (import rs.util.iterate)
   (import rs.util.msgs)
   (import rs.util.properties)
   (import rs.sys.threads.manager)
   (import graphics.geometry)
   (import graphics.image)
   (import graphics.color)
   ;;
   (load "msgs.scm")
   ;;
   (load "client.scm")
   (load "colormgr.scm") ; polymorphic (solid & tiled) colors
   ;;
   (load "atoms.scm")
   (load "connect.scm")
   (load "resource.scm")
   (load "eventloop.scm")
   ;;
   ;;  client management
   ;;
   (export <client>
	   with-client
	   current-client
	   open-client
           open-client-on-x
	   flush-client)
   ;;
   ;; resource management
   ;;
   (export application-font
	   get-pixel-resource
           get-font-resource)
   ;;
   (load "window.scm")
   (export make-window make-offscreen-window content-view)
   ;;
   (load "view.scm")
   (export make-view <view> frame update)
   (export draw-self lock-focus)
   (export mouse-moved
           mouse-down
           mouse-up
           mouse-enter
           mouse-leave)

   (load "control.scm")
   (export <control>)

   (load "button.scm")
   (export make-button <button> state)

   (load "box.scm")
   (export make-box <box>)

   (load "progressbar.scm")
   (export make-progress-bar <progress-bar> set-value! value)

   (load "colorwell.scm")
   (export make-color-well <color-well>)

   (load "colorpick.scm")
   (export make-color-picker <color-picker> set-color! color)

   (load "drag-and-drop.scm")

   (load "scroller.scm")
   (export make-horz-scroller)

   ;; support `load' from command line
   (import rs.lang.eval)
))
