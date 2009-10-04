(define (landscape)
  (translate (make-point (* 8.5 72) 0))
  (rotate 90))

(define (getlinewidth style)
  (with-module
      graphics.styles
    (get-style-attribute style 'linewidth)))

(define (getfontsize style)
  (with-module
      graphics.styles
    (get-style-attribute style 'size)))

;;;

(define (compute-arrow-path fromp top #key 
                            (scale default: 1)
                            (style default: 'default)
                            (setback default: #f)
                            (setback-line default: #f)
                            (setback-linewidth default: 1))
  (let* ((d (normalize (point- top fromp)))
         (n (make-size (- (dy d)) (dx d)))
         (tip (cond
               (setback-line
                ;; doesn't take into account the line's orientation
                (let ((c (abs (inner-product d (normal-on setback-line)))))
                  (if (< c 0.0001)
                      ;; forget the setback --
                      ;; the line and the setback-line are essentially parallel
                      top
                      (point+ top (size* d (- (/ setback-linewidth 2 c)))))))
               (setback
                (point+ top (size* d (- setback))))
               (else
                top))))
    (compute-default-arrow tip d n scale)))

(define (compute-default-arrow tip d n scale)
  (let* ((arrow-width (* scale 2))
         (arrow-length (* arrow-width 3))
         (ap2 (point+ tip (size* d (- arrow-length))))
         (ap3 (point+ ap2 (size* n arrow-width)))
         (ap4 (point+ ap2 (size* n (- arrow-width)))))
    (values (point+ tip (size* d (- arrow-length)))
            (simple-polygon-area tip ap3 ap4))))
