(define-module graphics.geometry (unquote)
  (unquote
   ;(import usual-inlines)
   (import rs.lang tables)
   (import rs.sys.numeric)
   (load "geom.scm")
   (export rect->values point->values size->values
	   point->size size->point)
   ;; `bbox' computes a bounding box for a <geometric>
   (export bbox)
   (export <geometric>)
   ;;
   (load "circle.scm")
   (export <circle>
           center
           radius
           make-circle)
   ;;
   (load "areas.scm")
   (export same-direction?)     ; two <size>'s going the same way?

   (import sort)
   (load "contours.scm")        ; for doing area logic
   (export <area>
           <path-bounded-area>

           <area-subpath>
           subpaths
           path-points
           position

           area-union
           area-intersection
           area-subtract
           area-xor
           ;;
           path-add
           path-subtract
           rect-area
           simple-polygon-area
           simplify-area)
   ;
   (load "transforms.scm")
   (export translate scale rotate 
           rotate-and-scale
           translate-and-scale
           translate-and-scale-uniform
	   make-affine-transform transform <transform> 
	   concatenate-transform $identity-transform
	   invert-transform inverse-transform
           vector->affine-transform
	   matrix)
   ;
   (load "algebra.scm")
   (load "bezier.scm")
   (export clear-coefficient-cache!
           flatness unit-circle-beziers
           bezier-quadratic-roots)
   ;
   (load "vecmath.scm")
   ;
   (load "clip.scm")
   ;
   (import tables)
   (load "geometry-table.scm")
   (export make-geometry-table <geometry-table>)
   ;
   (export <point> x y $zero-point)
   (export <line> from to)
   (export <size> width height dx dy $zero-size)
   (export make-point make-line make-size)
   (export point+ point- size- size+ size* normalize 
           inner-product)
   (export point-average)
   ;
   (export <rect>
	   origin size 
	   origin-x origin-y size-width size-height $zero-rect
	   ;; rectangle constructors
	   make-rect make-rect2 bbox-rect trim-rect)
   ;
   (export limit-x limit-y center-x center-y point-in-rect? inset-rect)
   (export lower-left upper-right lower-right upper-left)
   (export offset-point offset-rect)
   (export intersect-rect union-rect union-rects union-points)

   (export clip-to-rect #|inside-rect?|# rects-intersect? clip-to-segment 
	   find-perp line-intersect)
   (export line-vector point-on distance^2 distance
	   intersection-parameter)
   (export <bezier-curve> curv subdivide subdivide-at
	   start-point first-handle second-handle end-point
	   set-start-point! set-first-handle! 
	   set-second-handle! set-end-point!
           make-bezier reverse-bezier
           sine-wave-beziers)
   ;
   (load "ellipse.scm")
   (export <ellipse>
           make-ellipse
           ellipse-top-at
           ellipse-tangent-at
           ellipse->bezier-segments
           ;*rotations*
           tangent-on
           curvature-on
           normal-on)
   ;;
   (load "area-seg.scm")
   (export area->segments
           segments->area
           eval-postscript-path
           <segment-path>
           segments)
   ;
   (load "persist.scm")
   (export *geometry-classes*)
   ;
   (export parameter->distance-along
           distance-along->parameter)
   ;
   ; methods for standard math operators
   ;
   (load "stdmath.scm")
   ;;
   (export path-length)

   ))

