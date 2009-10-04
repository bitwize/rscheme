
(define (close-tcp-display (self <x-display>))
  (let ((fd (get-property self 'file-descriptor)))
    (close-input-port (input-port self))
    (close-output-port (output-port self))
    (fd-close fd)
    (remove-property! self 'file-descriptor)
    (set-close-proc! self (lambda (dpy) (error "already closed")))
    (values)))

(define (open-tcp-display host server)
  (let* ((tcphost (if (string=? host "")
                      "localhost"
                      host))
	 (auth (get-xauth (string-append host ":" (number->string server))))
         (fd (inet-client tcphost (+ server 6000)))
	 (inp (open-mbox-input-port fd))
	 (out (open-queued-output fd))
	 (dpy (handshake-with-display inp out auth)))
    (set-property! dpy 'file-descriptor fd)
    (set-property! dpy 'protocol 'tcp)
    (set-property! dpy 'host tcphost)
    (set-property! dpy 'display server)
    (set-close-proc! dpy close-tcp-display)
    dpy))

(define (close-display (self <x-display>))
  ((close-proc self) self))

(define-method write-object ((self <x-display>) port)
  (format port "#[<x-display> ~a:~d]"
	  (get-property self 'host)
	  (get-property self 'display)))

(define (handshake-with-display inp out auth-info)
  (write-string out (initial-message auth-info))
  (flush-output-port out)
  ;; how does it really work when the response is Authenticate?
  ;; the docs are less than clear
  (let ((open-reply (read-string inp 8)))
    (with-unpacked open-reply
      (u1: success
       u1: reason-length
       u2: proto-major
       u2: proto-minor
       u2: additional-words)
      (let ((additional (open-input-string
			 (read-string inp (* 4 additional-words)))))
	(case success
	  ((0)
	   (print open-reply)
	   (print additional)
	   (error (read-string additional reason-length)))
	  (else
	   (with-unpacked-from-input-string additional
	     (u4: release-number
	      u4: resource-id-base
	      u4: resource-id-mask
	      u4: motion-buffer-size
	      u2: vendor-length
	      u2: max-request-length
	      u1: num-screens
	      u1: num-formats
	      u1: image-byte-order
	      u1: bitmap-order
	      u1: bitmap-unit
	      u1: bitmap-pad
	      u1: min-keycode
	      u1: max-keycode
	      u4: -)
	     (let ((vendor (read-string additional vendor-length)))
	       (read-string additional (pad4 vendor-length))
	       (let* ((formats (unpack-list-of
				num-formats
				(lambda () 
				  (parse-pixmap-format additional))))
		      (bf (make <x-bitmap-format>
				unit: bitmap-unit
				pad: bitmap-pad
				;;XXX CHECK THIS XXX:
				lsb-first?: (= bitmap-order 0)))
		      (dpy (make <x-display>
				 ;
				 close-proc: (lambda (dpy))
				 input-port: inp
				 output-port: out
				 atom-table: (make-string-table)
				 output-lock: (make-semaphore 1)
				 event-queue: (make-mailbox)
				 reply-queue: (make-mailbox)
				 queue-lock: (make-semaphore 1)
				 xid-table: (make-table eq? integer->hash)
				 ;
				 display-error-handler: default-error-handler
				 ;
				 display-protocol-major-version: proto-major
				 display-protocol-minor-version: proto-minor
				 ;;XXX is this what CLX means...? XXX:
				 display-release-number: release-number
				 display-resource-id-base: resource-id-base
				 display-resource-id-mask: resource-id-mask
				 display-xid: (make-id-generator 
					       resource-id-mask
					       resource-id-base)
				 display-motion-buffer-size: motion-buffer-size
				 display-max-request-length: max-request-length
				 ;;XXX CHECK THIS XXX
				 display-image-lsb-first?: (= image-byte-order
							      0)
				 display-bitmap-format: bf
				 display-min-keycode: min-keycode
				 display-max-keycode: max-keycode
				 display-vendor-name: vendor
				 display-pixmap-formats: (vector->list formats)
				 display-roots: '())))
		 ; this makes ButtonPress `child' window work, I hope
		 (table-insert! (xid-table dpy) 0 'none)
		 ;
		 (set-display-roots!
		  dpy
		  (vector->list (unpack-list-of
				 num-screens
				 (lambda ()
				   (parse-screen dpy additional)))))
		 dpy)))))))))


(define (make-id-generator resource-id-mask resource-id-base)
  (let loop ((multiplier 1)
	     (mask resource-id-mask))
    (if (even? mask)
	(loop (* multiplier 2)
	      (quotient mask 2))
	(let ((count 0))
	  (lambda ()
	    (set! count (+ count 1))
	    (+ resource-id-base (* multiplier count)))))))

;;;

(define (parse-pixmap-format prt)
  (with-unpacked-from-input-string prt
    (u1: depth
     u1: bpp
     u1: scanline-pad
     u1: -
     u4: -)
    (make <x-pixmap-format>
	  depth: depth
	  bits-per-pixel: bpp
	  scanline-pad: scanline-pad)))

;;;


(define (open-display host #key (display default: 0) 
		                (protocol default: 'tcp))
  (let ((dpy (case protocol
	       ((tcp)
		(open-tcp-display host display))
	       (else
		(error "open-display: unsupported protocol: ~s" protocol)))))
    ;; start the listener thread
    (thread-resume
     (make-thread (lambda ()
		    (slurp-stuff-from-server dpy))
		  "listener"))
    dpy))

;;;
