
(define-module graphics.tiff ()
  (&module
   (import usual-inlines syscalls tables)
   (import rs.util.properties)
   (import gui.x)
   (import graphics.color)
   (import graphics.color.dither)
   (import graphics.image)
   ;
   (load "rationals.scm")
   (load "errors.scm")
   (load "tags.scm")
   (load "types.scm")
   (load "tiff.scm")
   (load "unpack.scm")
   (load "to-x-image.scm")
   ;
   (export open-tiff-image
           <tiff-image>
	   tiff-image-size
	   get-tag-scalar
	   get-tag-array
	   number-of-subimages
           get-tiff-subimage
	   tiff->x-image
	   load-into-heap
	   for-each-row
           get-pixel
	   for-each-pixel)))

