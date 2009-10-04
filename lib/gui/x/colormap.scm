
(define-class <x-colormap> (<x-object>)
  (properties type: <vector> init-value: '#())
  colormap-visual-type)

(define (colormap-display (colormap <x-colormap>))
  (x-display colormap))

(define (colormap? thing)
  (instance? thing <x-colormap>))

(define (colormap-id (colormap <x-colormap>))
  (x-id colormap))
