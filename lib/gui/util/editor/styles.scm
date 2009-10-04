,(use graphics.styles
      rs.util.memoize)

(define-style-type <font-style> foundry family size weight slant width)

(define (getcolor dpy spec)
  (alloc-color (x-colormap dpy) (string->color spec)))

(memoize getcolor)

(define (getfont dpy foundry family weight slant width size)
  (open-font
   (x-display dpy)
   (~ "-~a-~a-~a-~a-~a--~d-*-*-*-*-*-*-*"
      foundry
      family
      (case weight
        ((black) "black")
        ((bold) "bold")
        ((book) "book")
        ((demi) "demi")
        ((light) "light")
        ((medium) "medium")
        ((regular) "regular")
        (else (error "Bad <font-style> weight: ~s" weight)))
      (case slant
        ((italic) "i")
        ((oblique) "o")
        ((roman) "r")
        (else (error "Bad <font-style> slant: ~s" slant)))
      (case width
        ((condensed) "condensed")
        ((normal) "normal")
        ((semicondensed) "semicondensed")
        (else (error "Bad <font-style> width: ~s" width)))
      size)))
  
(memoize getfont)

(define-method style-compile ((self <font-style>))
  (apply getfont
         *display*
         (get-style-attributes 
          self
          '(foundry family weight slant width size))))

;;;

;;;

(define-style-type <color-style> spec)

(define-method style-compile ((self <color-style>))
  (getcolor *display* (get-style-attribute self 'spec)))
  
(define-style-type <face-style> font foreground background 
  line-height
  line-depth)

(define-method style-compile ((self <face-style>))
  (let ((fg (style-compile (get-style-attribute self 'foreground)))
        (bg (cond
             ((get-style-attribute self 'background)
              => style-compile)
             (else #f)))
        (fnt (style-compile (get-style-attribute self 'font))))
    ;;
    (lambda (win gc x y w h d str)
      (if bg
          (begin
            (set-gcontext-foreground! gc bg)
            (draw-rectangle win gc x (- y h) w (+ h d) #t)))
      ;;
      (set-gcontext-foreground! gc fg)
      (set-gcontext-font! gc fnt)
      (draw-glyphs win gc x y str))))

;;;

