
; See also:
;  ftp://ftp.uu.net/graphics/png/documents/png-1.1-pdg/PNG-Structure.html.Z
; or
;  http://www12.w3.org/TR/png.html


(define-module graphics.png ()
  (&module
   (import usual-inlines)
   (import graphics.image)
   (import graphics.color)
   (import rs.sys.compression)
   (import rs.util.pack)
   (import rs.util.properties)
   (import rs.util.iterate)
   ;;
   (load "chunk.scm")
   (load "filter.scm")
   (load "iterate.scm")
   (load "as-image.scm")
   (load "crc.scm")
   (load "write.scm")
   ;;
   (export read-png-image
	   write-png-image)))
