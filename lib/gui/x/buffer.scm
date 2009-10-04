
(define-syntax (with-display display . body)
  (with-semaphore-rec (output-lock display) . body))

;;;

(define-syntax (sync-out display)
  (if (display-after-function display)
      ((display-after-function display) display)))

;;;

(define (display-force-output (self <x-display>))
  (flush-output-port (output-port self))
  (values))

(define (display-finish-output (self <x-display>))
  (flush-output-port (output-port self))
  (input-focus self)
  (values))

;;;
;;;  the caller should already have locked this display
;;;

(define (internal-send (self <x-display>) buffer)
  (let (((n <fixnum>) (next-sequence-number self)))
    (set-next-sequence-number! self (bitwise-and (add1 n) #xFFFF))
    (if (string? buffer)
	(write-string (output-port self) buffer)
	(writev (output-port self) buffer))
    (sync-out self)
    n))

;;;

(define (internal-rpc (self <x-display>) buffer)
  (let (((n <fixnum>) (next-sequence-number self)))
    (set-next-sequence-number! self (bitwise-and (add1 n) #xFFFF))
    (if (string? buffer)
	(write-string (output-port self) buffer)
	(writev (output-port self) buffer))
    (sync-out self)
    (flush-output-port (output-port self))
    (let ((nxt (get-next-reply self)))
      (if (eq? (bvec-ref (car nxt) 0) 0)
	  (invoke-error-handler self (car nxt))
	  (make <x-reply>
		request: buffer
		common-reply: (car nxt)
		remainder-reply: (cdr nxt))))))

(define-class <x-reply> (<object>)
  request
  (common-reply type: <string>)
  (remainder-reply type: <string>))

;;;

(define (unget-event (self <x-display>) event)
  (dequeue-push-front! (event-queue self) event)
  (values))

(define (get-next-event (self <x-display>))
  (receive-message! (event-queue self)))

(define (get-next-event-if-present (self <x-display>))
  (receive-message-if-present! (event-queue self)))

(define (get-next-reply (self <x-display>))
  (receive-message! (reply-queue self)))

(define (slurp-stuff-from-server (dpy <x-display>))
  (let loop ()
    (if (read-next dpy)
	(loop)
	(wm 404 "display closed: ~s" dpy))))

(define (read-next (self <x-display>))
  (let ((common (read-string (input-port self) 32)))
    (if (eof-object? common)
	#f
	(case (bvec-ref common 0)
	  ((1)  ;; reply
	   (with-unpacked common
	     (u2: -
	      u2: -
	      u4: reply-length)
           (send-message!
	      (reply-queue self)
	      (cons common
		    (if (eq? reply-length 0)
			""
			(read-string (input-port self) (* 4 reply-length))))))
	   #t)
	  ((0)  ;; error
         (send-message! (reply-queue self) (cons common ""))
	   #t)
	  (else ;; event
         (send-message! (event-queue self) common)
	   #t)))))

(&module (export read-next get-next-event))
