(define-class <x-visual-type> (<x-object>)
  class
  bits-per-rgb-value
  colormap-entries
  red-mask
  green-mask
  blue-mask)

(define (visual? object)
  (instance? object <x-visual-type>))

(define-class <x-screen> (<x-object>)
  (properties type: <vector> init-value: '#())
  (screen-backing-store init-value: 'never)
  (screen-black-pixel type: <fixnum>)
  (screen-default-colormap type: <x-colormap>)
  (screen-depths type: <list>)
  (screen-event-mask-at-open type: <fixnum>)
  (screen-height type: <fixnum>)
  (screen-height-in-millimeters type: <fixnum>)
  (screen-max-installed-maps type: <fixnum>)
  (screen-min-installed-maps type: <fixnum>)
  (screen-root type: <x-window>)
  (screen-root-depth type: <fixnum>)
  (screen-root-visual type: <fixnum>)
  (screen-save-unders? type: <boolean>)
  (screen-white-pixel type: <fixnum>)
  (screen-width type: <fixnum>)
  (screen-width-in-millimeters type: <fixnum>))

(define (screen? object)
  (instance? object <x-screen>))

(define (screen-display (scrn <x-screen>))
  (x-display scrn))

(define (colormap-visual-class (colormap <x-colormap>))
  (class (colormap-visual-type colormap)))
