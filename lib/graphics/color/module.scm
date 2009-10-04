(define-module graphics.color ()
  (&module
   (import rs.lang)
   (import regex)  ; for parsing color strings
   (import rs.sys.tables) ; for color hash tables
   ;;
   (load "color.scm")
   (load "table.scm")
   ;; public
   (export <color>
           <gray-color>
           <rgb-color>
           <cmyk-color>
	   color?
	   $white
	   $black
	   make-color
           make-gray

	   color-red
	   color-green
	   color-blue

	   color-rgb            ;; 3 float values
           color-cmyk           ;; 4 float values
           
	   color-rgb-components ;; 3 16-bit values
	   color=?
	   color->hash
	   make-color-table)
   ;; internal
   (export red-component
	   green-component
	   blue-component
           white-component
           cyan-component
           magenta-component
           yellow-component
           black-component)
   ;;
   ;;(load "dither.scm")          ; moved to graphics.color.dither
   ;;(export compute-dithering)   ; moved to graphics.color.dither
   ;;
   (load "pixel.scm")
   ;; public
   (export <pixel>              ; <pixel> : a color with transparency (alpha)
	   $clear
	   make-pixel
	   pixel-alpha
           rgba8->pixel)
   ;; internal
   (export alpha-component
	   pixel-rgba)
   ;;
   (load "parse.scm")
   (export string->color)
   ;;
   (load "math.scm")
   (export color*        ;; <color> * <real>
	   color+)       ;; <color> + <color>
   ))

