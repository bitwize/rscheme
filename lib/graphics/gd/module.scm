(define-module graphics.gd ()
  (&module
   (import usual-inlines)
   (load "gd.scm")
   (export 
    ;;
    ;;  Image creation, loading, and saving
    ;;
    gd-image-create
    gd-image-create-from-png
    gd-image-png
    gd-image-png-ctx
    gd-image-destroy
    ;;
    open-gd-io-ctx
    ;;
    ;;  Management and Inspection
    ;;
    gd-image-color-resolve
    gd-image-color-get-rgba
    gd-image-color-allocate
    gd-image-sx
    gd-image-sy
    gd-image-size                       ; returns two values: sx sy
    ;;
    ;;  Painting operations
    ;;
    gd-image-set-pixel
    gd-image-get-pixel
    gd-image-line
    gd-image-string-ft
    gd-image-string-ft-size
    <gd-font-error>
    gd-image-rectangle
    gd-image-filled-rectangle
    gd-image-arc
    gd-image-copy
    )))
