(define-module graphics.pnm ()
  (&module
   (import usual-inlines)
   (import syscalls)
   (import graphics.color)
   ;
   (load "pgm.scm")
   (export <image>
	   image-width
	   image-height)
   ;
   (export open-input-pgm-file          ; deprecated; use open-input-pnm-file
           open-input-pnm-file
	   open-output-pgm-file
	   close-image
	   get-pixel
	   set-pixel!)))


