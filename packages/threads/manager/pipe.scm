(define-class <internal-pipe-input> (<buffered-input-port>)
  mbox)

(define-method provide-more-input ((self <internal-pipe-input>))
  (if (mbox self)
      (let ((m (receive-message! (mbox self))))
        (cond
         ((pair? m)     ; it's markup... ignore it
          (provide-more-input self))
         ((string? m) m)
         (else          ; it's a close
          (set-mbox! self #f)
          #f)))
      #f))

(define (flush-internal-pipe (self <internal-pipe-input>))
  (let ((q (make-dequeue)))
    (let loop ()
      (if (mailbox-has-data? (mbox self))
          (begin
            (dequeue-push-back! q (receive-message! (mbox self)))
            (loop))
          (dequeue-state q)))))

(define-method more-input-ready? ((self <internal-pipe-input>))
  ;; XXX this fails if the only available data is markup
  (mailbox-has-data? (mbox self)))

;;;

(define-class <internal-pipe-output> (<output-port>)
  mbox)

(define-method write-markup ((self <internal-pipe-output>) markup)
  (send-message! (mbox self) (cons 'markup markup)))

(define-method close-output-port ((self <internal-pipe-output>))
  (send-message! (mbox self) #f))

(define-method write-string ((self <internal-pipe-output>) str)
  (if (not (string=? str ""))
      (send-message! (mbox self) str)))

(define-method output-port-write-char ((self <internal-pipe-output>) ch)
  (send-message! (mbox self) (string ch)))

;;;

(define (make-internal-pipe #optional name)
  (let ((mbox (make-mailbox (or name 'internal-pipe))))
    (values (make <internal-pipe-input> mbox: mbox)
            (make <internal-pipe-output> mbox: mbox))))

(define (is-waiting-for-internal-pipe? (t <thread>) (p <internal-pipe-output>))
  (and (thread-blocked? t)
       (eq? (thread-blocked-on t) (mbox p))))
