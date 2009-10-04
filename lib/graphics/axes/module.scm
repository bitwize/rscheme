;;;
;;;  Useful and common procedures for dealing with
;;;  axes of plots (e.g., the x-axis or y-axis)
;;;

(define-module graphics.axes ()
  (&module
   (import usual-inlines)
   (import graphics.geometry
           graphics.device
           graphics.fontmgr
           graphics.afm)
   ;;
   (load "axes.scm")
   (load "tics.scm")
   (load "common.scm")
   (load "autorange.scm")
   ;;
   (export <graphic-axis>
           set-compute-label-hook!

           <full-2d-graph>
           plot-graph
           graph-bbox
           linear-scale-axis
           log-scale-axis
           draw-axis)))

