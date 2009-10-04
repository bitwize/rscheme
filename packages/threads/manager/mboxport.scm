
(define-class <mbox-input-port> (<buffered-input-port>)
  (mbox type: <mailbox>) ;; [3]
  (event init-value: 0 setter: #f) ;; [4]
  (fd type: <fixnum>)
  (owner? type: <boolean> init-value: #f))

;;

(define (refresh-read-event (self <mbox-input-port>))
  (if (fixnum>=? (fd self) 0)
      (begin
	(if (eq? (event self) 0)
	    ;; this fills in the `event' slot implicitly
	    (make-read-event (fd self) self))
	#t)
      #f))

(define (underlying-fd-did-close (self <mbox-input-port>))
  ;; detach any thread-system event that we are using
  (set-fd! self -1)
  (if (not (eq? (event self) 0))
      ;; our back ptr in `event' gets cleared
      ;; automatically by `free-read-event'
      (free-read-event* self (%slot-index <mbox-input-port> event))))

;;;

(define-method provide-more-input ((self <mbox-input-port>))
  (if (refresh-read-event self)
      (receive-message! (mbox self))
      ;; already got EOF, and never getting anything else!
      #f))

(define-method more-input-ready? ((self <mbox-input-port>))
  (if (eq? (event self) 0)
      (begin
	(refresh-read-event self)
	(thread-sleep 0)))
  (mailbox-has-data? (mbox self)))

;;; this should probably be called something else, since
;;; it only works for file descriptors, not arbitary mbox
;;; sources...

(define (open-mbox-input-port fd #optional owner)
  (let ((port (make <mbox-input-port>
		    fd: fd
		    mbox: (make-mailbox fd)
                    owner?: owner)))
    (make-read-event fd port)
    port))

(define-method close-input-port ((self <mbox-input-port>))
  (if (owner? self)
      (begin
        (fd-close (fd self))
        (set-fd! self -1)))
  (underlying-fd-did-close self))

(define (filedes->input-port filedes #optional owner)
  (open-mbox-input-port filedes owner))
