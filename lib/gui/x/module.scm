(define-module gui.x ()
  (&module
   (import usual-inlines)
   (import tables)
   (import syscalls)
   (import rs.sys.threads.manager)
   (import rs.util.properties)
   (import rs.util.collection)
   ;;;
   (import rs.util.msgs))
  (define-message-table gui.x 611)
  (&module
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;			  Class Definitions
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (load "bufferops.scm")
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (load "display.scm")
   (export display?
	   display-bitmap-format
	   display-byte-order
	   display-display
	   display-image-lsb-first?
	   display-keycode-range
	   display-max-keycode
	   display-max-request-length
	   display-min-keycode
	   display-motion-buffer-size
	   display-pixmap-formats
	   display-protocol-major-version
	   display-protocol-minor-version
	   display-protocol-version
	   display-release-number
	   display-resource-id-base
	   display-resource-id-mask
	   display-roots
	   display-vendor
	   display-vendor-name
	   display-version-number
	   display-xid)
   (export depth bits-per-pixel scanline-pad)
   (export unit pad lsb-first?)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (load "xobject.scm")
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;			  Method Definitions
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (load "open.scm")
   (load "xauth.scm")
   ;
   (export open-display
	   close-display)
   ;
   ;
   (load "buffer.scm")
   (export display-after-function
	   set-display-after-function!
	   display-force-output
	   display-finish-output
	   with-display)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (load "colormap.scm")
   (export colormap-display colormap? colormap-id)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (load "window.scm")
   (export window? drawable?)
   (export drawable-display drawable-screen drawable-root)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (load "screen.scm")
   (export screen?
	   screen-backing-store
	   screen-black-pixel
	   screen-default-colormap
	   screen-display
	   screen-depths
	   screen-event-mask-at-open
	   screen-height
	   screen-height-in-millimeters
	   screen-max-installed-maps
	   screen-min-installed-maps
	   screen-root
	   screen-root-depth
	   screen-root-visual
	   screen-save-unders?
	   screen-white-pixel
	   screen-width
	   screen-width-in-millimeters)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (load "cursor.scm")
   (export cursor? cursor-id)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (load "event-mask.scm")
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (load "create-window.scm")
   (export create-window)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (load "set-window.scm")
   (export set-window-cursor!
	   set-window-event-mask!
	   set-drawable-frame!) ; CLX extn
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (load "parse-screen.scm")
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (load "map-window.scm")
   (export map-window)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (load "unmap-window.scm")
   (export unmap-window)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (load "error-handler.scm")
   (export default-error-handler <x-error>)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (load "focus.scm")
   (export set-input-focus input-focus)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (load "get-window-attributes.scm")
   (export with-state with-state*)
   (export window-colormap)
   (export drawable-x set-drawable-x!
	   drawable-y set-drawable-y!
	   drawable-width set-drawable-width!
	   drawable-height set-drawable-height!
	   drawable-border-width set-drawable-border-width!
	   drawable-depth)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;(load "color.scm")
   (import graphics.color)
   (export make-color <color> color? make-color-table color=?
	   color-red color-green color-blue color-rgb)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (load "alloc-color.scm")
   (export alloc-color)
   (export alloc-color-cells store-color free-colors)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (load "query-colors.scm")
   (export query-colors)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (load "gcontext.scm")
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (load "create-gcontext.scm")
   (export create-gcontext)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (load "copy-gcontext.scm")
   (export copy-gcontext)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (load "free-gcontext.scm")
   (export free-gcontext)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (load "set-gcontext.scm")
   (export set-gcontext-foreground!)
   (export set-gcontext-font!)
   (export set-gcontext-line-width!)
   (export set-gcontext-stipple!)
   (export set-gcontext-tile!)
   (export set-gcontext-fill-style!)
   (export set-gcontext-clip-x!)
   (export set-gcontext-clip-y!)
   (export set-gcontext-clip-mask!)
   (export set-gcontext-dashes!)
   (export set-gcontext-line-style!)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (load "change-keyboard-control.scm")
   (export change-keyboard-control)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (load "drawing.scm")
   (export clear-area)
   (export draw-line draw-lines)
   (export draw-rectangle)
   (export draw-point draw-points)
   (export draw-arc)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (load "atoms.scm")
   (export find-atom atom-name intern-atom)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (load "properties.scm")
   (export change-property)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (load "events.scm")
   (export process-event queue-event gui.x%event-setup)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;  some classes (type names) that are exposed as an extension to CLX
   (export <x-window> <x-drawable> <x-gcontext>)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (load "create-cursor.scm")
   (export create-cursor)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (load "create-pixmap.scm")
   (export create-pixmap)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (load "keyboard-mapping.scm")
   (export keyboard-mapping)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (load "font.scm")
   (export open-font 
           close-font 
           draw-glyphs
           list-font-names)
   (export font-all-chars-exist?
	   font-ascent
	   font-default-char
	   font-descent
	   font-direction
	   font-max-byte1
	   font-max-byte2
	   font-max-char
	   font-min-byte1
	   font-min-byte2
	   font-min-char
	   font-properties
	   ;
	   char-ascent
	   char-left-bearing
	   char-right-bearing
	   char-ascent
	   char-descent
	   char-width
	   char-attributes
	   ;
	   text-extents) ;; XX partial implementation
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (load "images.scm")
   (export create-image put-image)
   (export image-depth) ;; CLX extension (needed for gui.util.x)
   (load "rawimage.scm")
   (export put-raw-image)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (load "copy-area.scm")
   (export copy-area)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (load "query-tree.scm")
   (export query-tree translate-coordinates query-pointer)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;
   ;;  the following exported variables go beyond 
   ;;  what's available in CLX
   ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   (load "truecolor.scm")
   (export rgb->pixel-proc 
	   color->pixel-proc)
   ;;;
   (export colormap-visual-type colormap-visual-class)
   (export <x-object>)
   ;;;
   ))
