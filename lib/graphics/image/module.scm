(define-module graphics.image ()
  (&module
   (import usual-inlines)
   (import graphics.color)
   (import graphics.geometry)
   ;;
   (load "generic.scm")
   (export <pixel-source>
	   image-width
	   image-height
	   get-pixel
	   for-each-pixel)
   ;;
   (export make-graphic-image
	   <graphic-image>
	   <image-rep>
	   <memory-image-rep>
	   set-pixel!
	   memory-image-rep) ;; extract the memory image rep,
                             ;; constructing one if necessary
   ;;
   (export <rgba-matrix>)       ;; graphics.png needs this for now...
   ;;
   ;; GIP procedures (Graphic Image Protocol); needed by
   ;; modules implementing new image reps
   ;;
   (export copy-to-memory-image-rep
	   rep-of
	   data
	   image-rep-quality
	   make-image-rep
	   get-image-rep)
   ;;
   (load "virtual.scm")
   (export make-sub-image
	   make-composite-image
	   make-constant-image
	   make-filtered-image)))


