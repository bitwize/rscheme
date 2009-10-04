
(define-class <graphics-device> (<object>) :abstract
  (properties type: <vector> init-value: '#()))

;;;

(define-generic-function close-graphics-device)

           ;;
(define-mm-generic-function areapath)
(define-mm-generic-function segmentto)

(define-mm-generic-function areastroke)
(define-mm-generic-function areafill)
(define-generic-function areashow)

           ;;
           ;;  transformation
           ;;
;(define-generic-function scale)                ; GF from graphics.geometry
;(define-generic-function rotate)               ; GF from graphics.geometry
;(define-generic-function translate)            ; GF from graphics.geometry
(define-generic-function concat)

           ;;
           ;;  path construction
           ;;
(define-generic-function moveto)
(define-generic-function lineto)
(define-generic-function curveto)
(define-generic-function arc)
(define-generic-function arcn)
(define-generic-function closepath)
(define-generic-function newpath)

           ;;
           ;;  graphics state
           ;;
(define-generic-function with-gstate-saved)
(define-generic-function setdash)
(define-generic-function setfont)
(define-generic-function currentfont)
(define-generic-function currentpoint)
(define-generic-function currentmatrix)
(define-generic-function setcolor)
(define-generic-function setfillstyle)
(define-generic-function setstrokestyle)
(define-generic-function device-color)
(define-generic-function setlinewidth)
(define-generic-function setlinecap)
(define-generic-function setlinejoin)
(define-generic-function clip)
(define-generic-function rectclip)
(define-generic-function pdfmark-annotation)
(define-generic-function to-pdfmark)

           ;;
           ;;  painting
           ;;
(define-generic-function stroke)
(define-generic-function fill)
(define-generic-function show)
(define-generic-function xshow)
(define-generic-function dxshow)
(define-generic-function rectfill)
(define-generic-function rectstroke)
(define-generic-function composite-image)
(define-generic-function include-eps)
(define-generic-function get-eps-bbox)
           ;;
           ;;  document structure
           ;;
(define-generic-function startpage)
(define-generic-function endpage)
(define-generic-function starttext)
(define-generic-function endtext)

;;;

(define-method segmentto ((self <graphics-device>) (s <line>))
  (lineto self (to s)))

(define-method segmentto ((self <graphics-device>) (s <bezier-curve>))
  (curveto self
           (first-handle s)
           (second-handle s)
           (end-point s)))

(define-method areafill ((self <graphics-device>) (area <rect>))
  (rectfill self area))

(define-method areafill ((self <graphics-device>) area)
  (with-gstate-saved
   self
   (lambda ()
     (areapath self area)
     (fill self))))

(define-method areastroke ((self <graphics-device>) (area <rect>))
  (rectstroke self area))

(define-method areastroke ((self <graphics-device>) area)
  (with-gstate-saved
   self
   (lambda ()
     (areapath self area)
     (stroke self))))

(define-method areapath ((self <graphics-device>) (path <segment-path>))
  (areapath* self (segments path)))

(define-method areapath ((self <graphics-device>) (area <area>))
  (areapath* self (area->segments area)))

(define-method areapath ((self <graphics-device>) (path <pair>))
  (moveto self (from (car path)))
  (for-each (lambda (p)
              (segmentto self p))
            path))

(define (areapath* (self <graphics-device>) (subpaths <list>))
  (for-each
   (lambda (sp)
     (bind ((close? segs (if (eq? (car sp) 'closed)
                             (values #t (cdr sp))
                             (values #f sp))))
       (moveto self (from (car segs)))
       (for-each (lambda (s)
                   (segmentto self s))
                 segs)
       (if close?
           (closepath self))))
   subpaths))

;;;
;;; default implementation
;;;

(define-method rectstroke ((self <graphics-device>) rect)
  (with-gstate-saved
   self
   (lambda ()
     (moveto self (lower-left rect))
     (lineto self (lower-right rect))
     (lineto self (upper-right rect))
     (lineto self (upper-left rect))
     (closepath self)
     (stroke self))))

(define-method rectfill ((self <graphics-device>) rect)
  (with-gstate-saved
   self
   (lambda ()
     (moveto self (lower-left rect))
     (lineto self (lower-right rect))
     (lineto self (upper-right rect))
     (lineto self (upper-left rect))
     (closepath self)
     (fill self))))

;;;

(define-method starttext ((self <graphics-device>))
  (values))

(define-method endtext ((self <graphics-device>))
  (values))
