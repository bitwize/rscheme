
(define (resolve-color-changers client data)
  (vector-map 
   (lambda (elem)
     (let ((c (if (symbol? elem)
		  (get-color-resource elem client)
		  (string->color elem))))
       (make-color-changer client c)))
   data))

(define (get-color-changers-resource name
				     #optional
				     (client default: (current-client)))
  (get-resource client name resolve-color-changers $color-changer-resource))

;;;

(define (make-pixel-installer (client <client>) pixel)
  (lambda (gc)
    (set-gcontext-foreground! gc pixel)
    (set-gcontext-fill-style! gc 'solid)))

(define (make-pixmap-installer (client <client>) color)
  (let ((dith (delay
		(image->pixmap
		 (make-constant-image color width: 4 height: 4)
		 (screen-root (on-screen client))
		 (use-colormap client)))))
    (lambda (gc)
      (set-gcontext-tile! gc (force dith))
      (set-gcontext-fill-style! gc 'tiled))))

;;;

(define (make-color-changer (client <client>) color)
  (or (table-lookup (color-changers client) color)
      (let ((ch (handler-case
		 (let ((pix (alloc-color (use-colormap client) color)))
		   (dm 611 "color changer -> pixel ~d for ~s" pix color)
		   (make-pixel-installer client pix))
		 ((<x-error>)
		  (dm 611 "color changer -> pixmap for ~s" color)
		  (make-pixmap-installer client color)))))
	(table-insert! (color-changers client) color ch)
	ch)))


;;;

(define (draw-bezeled win gc (colors <vector>) pat x y w h)
  (let loop ((x x)
	     (y y)
	     (w w)
	     (h h)
	     (p pat))
    (if (pair? p)
	(case (car p)
	  ((left)
	   (draw-rectangle win gc x y 1 h #t)
	   (loop (+ x 1) y (- w 1) h (cdr p)))
	  ((right)
	   (draw-rectangle win gc (+ x w -1) y 1 h #t)
	   (loop x y (- w 1) h (cdr p)))
	  ((top)
	   (draw-rectangle win gc x y w 1 #t)
	   (loop x (+ y 1) w (- h 1) (cdr p)))
	  ((bottom)
	   (draw-rectangle win gc x (+ y h -1) w 1 #t)
	   (loop x y w (- h 1) (cdr p)))
	  ((middle)
	   (draw-rectangle win gc x y w h #t))
	  (else
	   (let ((f (vector-ref colors (car p))))
	     (if (fixnum? f) ; backward compat.
		 (begin
		   (set-gcontext-fill-style! gc 'solid)
		   (set-gcontext-foreground! gc f))
		 (f gc))
	     (loop x y w h (cdr p))))))))


(define (draw-bezeled-rect win gc (colors <vector>) pat (r <rect>))
  (draw-bezeled win gc colors
		pat
		(origin-x r)
		(origin-y r)
		(size-width r)
		(size-height r)))
