
(define-class <x-bitmap-format> (<object>)
  unit
  pad
  lsb-first?)

(define-class <x-pixmap-format> (<object>)
  depth
  bits-per-pixel
  scanline-pad)

(define-class <x-display> (<object>)
  ;
  (properties type: <vector> init-value: '#())
  (close-proc type: <function>)
  ;
  (display-plist type: <list> init-value: '()) ;; CLX compatibility
  ;
  (input-port type: <mbox-input-port>)
  (output-port type: <queued-output-port>)
  (output-lock type: <semaphore>)
  (next-sequence-number type: <fixnum> init-value: 1)
  (atom-table type: <string-table>)
  (xid-table type: <hash-integer-table>)
  ;
  (event-queue type: <mailbox>)
  (reply-queue type: <mailbox>)
  (queue-lock type: <semaphore>)
  ;
  (display-after-function init-value: #f)
  (display-version-number type: <fixnum> init-value: 1) ;; this is version 1
  (display-vendor-name type: <string>)
  (display-release-number type: <fixnum>) ;; vendor release number
  (display-protocol-major-version type: <fixnum>)
  (display-protocol-minor-version type: <fixnum>)
  (display-resource-id-base type: <fixnum>)
  (display-resource-id-mask type: <fixnum>)
  (display-xid type: <function>)
  (display-motion-buffer-size type: <fixnum>)
  (display-max-request-length type: <fixnum>)
  (display-image-lsb-first? type: <boolean>)
  (display-bitmap-format type: <x-bitmap-format>)
  (display-min-keycode type: <fixnum>)
  (display-max-keycode type: <fixnum>)
  (display-pixmap-formats type: <list>)
  (display-roots type: <list>)
  (display-error-handler))

(define (display? object)
  (instance? object <x-display>))

(define-method display-display ((self <x-display>))
  (get-property self 'display))

(define (display-keycode-range (self <x-display>))
  (values (display-min-keycode self) (display-max-keycode self)))

(define (display-protocol-version (self <x-display>))
  (values (display-protocol-major-version self)
	  (display-protocol-minor-version self)))

(define (display-vendor (self <x-display>))
  (values (display-vendor self)
	  (display-release-number self)))

(define (display-byte-order dpy)
  (case (xbo-endianess)
    ((little-endian) 'lsbfirst)
    (else 'msbfirst)))

(define (alloc-x-id (self <x-display>))
  ((display-xid self)))
