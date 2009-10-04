;;;
;;;   This file defines a common style sheet for some
;;;   of the illustrations in this directory

;;;
;;;   Define some standard colors
;;;

(define-solid-paint-style black () color: 'black)
(define-solid-paint-style white () color: 'white)

(define-solid-paint-style light-gray () color: '(gray 0.8))
(define-solid-paint-style light-blue () color: '(cmyk 0.333 0.333 0 0))

;;;
;;;   Define some standard strokes
;;;

(define-stroke-style medium-stroke ()
  paint: 'black
  linewidth: 0.707
  linejoin: 'miter
  linecap: 'butt
  miterlimit: 2)

(define-stroke-style hairline (medium-stroke)
  linewidth: 0.1)

(define-stroke-style thin-stroke (medium-stroke)
  linewidth: 0.5)

(define-stroke-style thick-stroke (medium-stroke)
  linewidth: 1)

;;;
;;;   Define some standard fonts
;;;

(define-font-style default-font ()
  family: "Times" 
  angle: 'normal
  weight: 'normal
  width: 'normal
  variation: 'none
  size: 10.5
  stretch: 0)
 
(define-font-style sans-font (default-font) family: "Helvetica")
(define-font-style sans-bold-font (sans-font) weight: 'bold)
(define-font-style sans-italic-font (sans-font) angle: 'italic)

(define-font-style serif-font (default-font) family: "Times")
(define-font-style serif-bold-font (serif-font) weight: 'bold)
(define-font-style serif-italic-font (serif-font) angle: 'italic)
(define-font-style helvetica-font (sans-font) size: 8)

(define-font-style dimen-font (serif-italic-font)
  size: 8)

;;;
;;;  Here is some code to make it easier to get the <text-font>
;;;  object from the style name...
;;;

,(use graphics.fontmgr)

(define-method get-text-font ((self <symbol>))
  (with-module
   graphics.styles
   (get-text-font (get-style self))))

