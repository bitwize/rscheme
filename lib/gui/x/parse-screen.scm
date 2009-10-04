
(define (make-screen-root root-id dpy)
  (let ((w (make <x-window>
		 x-id: root-id
		 x-display: dpy
		 drawable-root: #f)))
    (set-drawable-root! w w)
    w))

(define-method initialize ((self <x-screen>))
  (set-property! (screen-root self) 'screen self)
  (let ((cmap (screen-default-colormap self)))
    (table-insert! (xid-table (x-display self))
		   (x-id cmap)
		   cmap)
    (set-colormap-visual-type! 
     cmap
     (table-lookup (xid-table (x-display self)) (screen-root-visual self)))))

(define-method initialize ((self <x-visual-type>))
  (table-insert! (xid-table (x-display self)) (x-id self) self))

(define (parse-screen dpy prt)
  (with-unpacked-from-input-string prt
     (u4: root
      u4: default-colormap
      u4: white-pixel
      u4: black-pixel
      u4: current-input-mask
      u2: width-in-pixels
      u2: height-in-pixels
      u2: width-in-mm
      u2: height-in-mm
      u2: min-installed-maps
      u2: max-installed-maps
      u4: root-visual
      u1: backing-stores
      u1: save-unders
      u1: root-depth
      u1: num-depths)
    (make <x-screen>
	  x-display: dpy
	  x-id: -1 ; does this make sense?
	  screen-root: (make-screen-root root dpy)
	  screen-default-colormap: (make <x-colormap>
					 x-id: default-colormap
					 x-display: dpy
					 colormap-visual-type: -1)
	  screen-white-pixel: white-pixel
	  screen-black-pixel: black-pixel
	  screen-event-mask-at-open: current-input-mask ;; XX is this right?
	  screen-width: width-in-pixels
	  screen-height: height-in-pixels
	  screen-width-in-millimeters: width-in-mm
	  screen-height-in-millimeters: height-in-mm
	  screen-min-installed-maps: min-installed-maps
	  screen-max-installed-maps: max-installed-maps
	  screen-root-visual: root-visual
	  screen-backing-store: (case backing-stores   ;; XX is this right?
				  ((0) 'none)
				  ((1) 'when-mapped)
				  ((2) 'never))
	  screen-save-unders?: (eq? save-unders 1) ;; XX check this
	  screen-root-depth: root-depth
	  screen-depths: (vector->list
			  (unpack-list-of num-depths
					  (lambda ()
					    (parse-depth dpy prt)))))))


(define (parse-depth dpy prt)
  (with-unpacked-from-input-string prt
     (u1: depth
      u1: -
      u2: num-visual-types
      u4: -)
     (cons depth
	   (vector->list (unpack-list-of num-visual-types
					 (lambda ()
					   (parse-visual-type dpy prt)))))))


(define-constant $visual-classes '#(static-gray
				    gray-scale
				    static-color
				    pseudo-color
				    true-color
				    direct-color))

(define (parse-visual-type dpy prt)
  (with-unpacked-from-input-string prt
    (u4: id
     u1: class
     u1: bprgb
     u2: cmap-entries
     u4: rmask
     u4: gmask
     u4: bmask
     u4: -)
   (make <x-visual-type>
	 x-id: id
	 x-display: dpy
	 class: (vector-ref $visual-classes class)
	 bits-per-rgb-value: bprgb
	 colormap-entries: cmap-entries
	 red-mask: rmask
	 green-mask: gmask
	 blue-mask: bmask)))
