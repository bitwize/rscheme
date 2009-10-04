;;;
;;;  A scheme to render fonts using GhostScript to actually
;;;  render the fonts
;;;

(define-class <glyph-image> (<object>)
  image
  pixmap
  origin
  image-size)

(define (render-char win gc (at <point>) (ch <glyph-image>))
  (dm 991 "composite ~s into ~s" (image ch) win)
  (x-composite (image ch) win gc 0 0)
#|
  (copy-area (pixmap ch)
             gc
             0 0 (dx (image-size ch)) (dy (image-size ch))
             win
             (- (x at) (x (origin ch)))
             (- (y at) (y (origin ch))))
  (draw-rectangle win gc
                  (- (x at) (x (origin ch)))
                  (- (y at) (y (origin ch)))
                  (dx (image-size ch))
                  (dy (image-size ch)))
|#
  (set-gcontext-clip-x! gc (- (x at) (x (origin ch))))
  (set-gcontext-clip-y! gc (- (y at) (y (origin ch))))
  (set-gcontext-clip-mask! gc (pixmap ch))
  (draw-rectangle win gc
                  (- (x at) (x (origin ch)))
                  (- (y at) (y (origin ch)))
                  (dx (image-size ch))
                  (dy (image-size ch))
                  #t))

(define (load-glyph-image (for <x-window>) file)
  (let* ((png (read-png-image file))
         (img (make-graphic-image from: png))
         (bitmap (create-pixmap width: (image-width img)
                                height: (image-height img)
                                depth: 1
                                drawable: for))
         (gc (create-gcontext drawable: bitmap
                              foreground: 0
                              background: 0)))
    ;(x-composite img bitmap gc 0 0)
    (dm 990 "composite into ~s" bitmap)
    (draw-rectangle bitmap gc 0 0 (image-width img) (image-height img) #t)
    (set-gcontext-foreground! gc 1)
    (for-each-pixel png (lambda (x y c)
                          (if (eq? (color-rgb-components c) 0)
                              (begin
                                ;(format #t "(~d ~d) " x y)
                                (draw-points bitmap gc (list x y))))))
    (dm 991 "ok, done")
    (make <glyph-image>
          image: img
          pixmap: bitmap
          origin: $zero-point
          image-size: (make-size (image-width img)
                                 (image-height img)))))

;;;

(load "debug.scm")

(define (t)
  (let* ((xw (content-window (the-open-view)))
         (xgc (create-gcontext drawable: xw
                               foreground: (screen-black-pixel
                                            (drawable-screen xw))))
         (gi (load-glyph-image xw "/tmp/A.png")))
    ;;
    (print gi)
    ;;
    (render-char xw xgc (make-point 100 90) gi)))
