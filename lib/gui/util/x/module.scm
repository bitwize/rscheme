
(define-module gui.util.x ()
  (&module
   (import usual-inlines)
   (import gui.x)
   (import rs.util.msgs)
   (import rs.util.properties)
   (import tables)
   (import graphics.color
           graphics.color.dither)
   (import graphics.image)
   (import graphics.geometry))
  ;
  (define-message-table gui.util.x 612)
  ;
  (&module
   (load "open.scm")
   (export open-x-display)
   ;
   (load "keys.scm")
   (export key->event)
   ;
   (load "ximages.scm")
   (export x-composite 
	   x-image-rep
	   get-color-map)
   ;
   (load "pixmap-cache.scm")
   (export x-pixmap-image-rep
	   image->pixmap)
   ;
   (load "transparent.scm")
   (export image->transparency
           subimage)))

