
(define-class <transparent-image> (<object>)
  pixmap                ; pixmap
  bitmap                ; mask
  transparency-gc)      ; GC with the bitmap as it's mask

(define-class <transparent-subimage> (<object>)
  (base-image type: <transparent-image>)
  (frame type: <rect>))

(define-method x-composite ((self <transparent-subimage>) win gc x y)
  (bind ((fx fy fw fh (rect->values (frame self)))
         ((base <transparent-image>) (base-image self))
         (g (transparency-gc base)))
    (set-gcontext-clip-x! g (- x fx))
    (set-gcontext-clip-y! g (- y fy))
    (copy-area (pixmap base)
               g
               fx fy fw fh
               win x y)))

(define-method subimage ((self <transparent-subimage>) (subframe <rect>))
  (make <transparent-subimage>
        base-image: (base-image self)
        frame: (intersect-rect
                (offset-rect subframe
                             (origin-x (frame self))
                             (origin-y (frame self)))
                (frame self))))

(define (image->transparency (self <graphic-image>) win)
  (let* ((p (create-pixmap width: (image-width self)
                           height: (image-height self)
                           depth: (drawable-depth win)
                           drawable: win))
         (b (create-pixmap width: (image-width self)
                           height: (image-height self)
                           depth: 1
                           drawable: win))
         (gc-temp (create-gcontext drawable: p))
         (gc-bitmap (create-gcontext drawable: b foreground: 0))
         (gc-apply (create-gcontext drawable: win
                                    exposures: #f)))
    (x-composite self p gc-temp 0 0)
    ;;
    (draw-rectangle b gc-bitmap 0 0 (image-width self) (image-height self) #t)
    (set-gcontext-foreground! gc-bitmap 1)
    ;;
    (for-each-pixel
     self
     (lambda (x y p)
       (if (>= (pixel-alpha p) 0.5)
           (draw-point b gc-bitmap x y))))
    ;;
    (free-gcontext gc-temp)
    (free-gcontext gc-bitmap)
    (set-gcontext-clip-mask! gc-apply b)
    (make <transparent-subimage>
          base-image: (make <transparent-image>
                            pixmap: p
                            bitmap: b
                            transparency-gc: gc-apply)
          frame: (make-rect 0 0 (image-width self) (image-height self)))))

#|
;;


(define (p0) (paintg '#(0 0 10 20 30 60 60 10)))

(define *transp* (make-graphic-image from: (read-png-image "transp.png")))

(define (splat (self <transparent-image>) src-x src-y w h dest x y)
  (let ((g (transparency-gc self)))
    (set-gcontext-clip-x! g (- x src-x))
    (set-gcontext-clip-y! g (- y src-y))
    (copy-area (pixmap self)
               g
               src-x
               src-y
               w
               h
               dest
               x
               y)))

#|
  (x-image-rep img (screen-default-colormap (drawable-screen win))))
                    
  (make <transparent-image>
        pixmap:
        bitmap:
        gc:)
|#

(define xx (make-transparent-image w *transp*))

(define (ts #optional (px default: 0) (py default: 0))
  (splat xx 0 0 16 16 w px py)
  (display-force-output x))
|#
