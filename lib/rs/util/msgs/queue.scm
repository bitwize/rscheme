(define-class <message-dest> (<object>) :abstract)

(define-class <message-entry> (<object>)
  (timestamp type: <time> :sealed)
  (message type: <message> :sealed)
  (message-arguments type: <vector> :sealed)
  (place init-value: #f :sealed))

(define-class <message-queue> (<dequeue>))
  
(define-class <head-message-queue> (<message-queue>)
  (messages-dropped init-value: 0 :sealed)
  (max-capacity :sealed))

(define-class <tail-message-queue> (<message-queue>)
  (messages-dropped init-value: 0 :sealed)
  (max-capacity :sealed))

(define-method write-object ((self <message-queue>) port)
  (format port "#[<message-queue> ~d msgs]" (dequeue-count self)))

(define-method write-object ((self <head-message-queue>) port)
  (format port "#[<message-queue> first ~d/~d msgs]" 
          (dequeue-count self)
          (+ (dequeue-count self)
             (messages-dropped self))))

(define-method write-object ((self <tail-message-queue>) port)
  (format port "#[<message-queue> last ~d/~d msgs]" 
          (dequeue-count self)
          (+ (dequeue-count self) (messages-dropped self))))

(define (make-message-queue #key (head default: #f) (tail default: #f))
  (cond
   ((and head tail)
    (error "make-message-queue: cannot specify both head: and tail:"))
   (head
    (make <head-message-queue>
          max-capacity: head
          state: (make-vector (+ head 1))))
   (tail
    (make <tail-message-queue>
          max-capacity: tail
          state: (make-vector (+ tail 1))))
   (else
    (make <message-queue>
          state: (make-vector 10)))))

(define-method display-message ((queue <message-queue>)
                                (self <message>) (argv <vector>) plc)
  (dequeue-push-back! queue (make <message-entry>
                                  timestamp: (time) 
                                  message: self
                                  message-arguments: argv
                                  place: plc))
  (values))

(define-method display-message ((queue <tail-message-queue>)
                                (self <message>) (argv <vector>) plc)
  (if (>= (dequeue-count queue) (max-capacity queue))
      (begin
        (dequeue-pop-front! queue)
        (set-messages-dropped! queue (add1 (messages-dropped queue)))))
  (dequeue-push-back! queue (make <message-entry>
                                  timestamp: (time) 
                                  message: self
                                  message-arguments: argv
                                  place: plc))
  (values))

(define-method display-message ((queue <head-message-queue>)
                                (self <message>) (argv <vector>) plc)
  (if (>= (dequeue-count queue) (max-capacity queue))
      (set-messages-dropped! queue (add1 (messages-dropped queue)))
      (dequeue-push-back! queue (make <message-entry>
                                      timestamp: (time) 
                                      message: self 
                                      message-arguments: argv 
                                      place: plc)))
  (values))

(define *message-time-format* "%Y-%m-%d %H:%M:%S %Z")

(define (time->msg-time t)
  (time->string t *message-time-format*))

(define (message-extract-and-clear (self <message-queue>))
  (dequeue-state-and-clear self))

(define (dequeue-state-and-clear (self <dequeue>))
  (let ((x (dequeue-state self)))
    (gvec-set! self 1 0)
    (gvec-set! self 2 0)
    x))

#|
(define-safe-glue (dequeue-state-and-clear (self <dequeue>))
  literals: ('#())
{
  obj r;

  if (dequeue_empty( self )) {
    r = LITERAL(0);
  } else {
    r = dequeue_state( self );
    gvec_set( self, SLOT(1), ZERO );
    gvec_set( self, SLOT(2), ZERO );
  }
  REG0 = r;
  RETURN1();
})
|#

(define-method print ((self <message-queue>))
  (bind ((v (dequeue-state self))
         (ds maxw (delta-strs v))
         (port (current-output-port)))
    ;;
    (vector-for-each
     (lambda ((e <message-entry>)
              (ts <string>))
       (format port "~a~a~a: "
               (time->msg-time (timestamp e))
               (make-string (- maxw (string-length ts)) #\space)
               ts)
       (display-message port (message e) (message-arguments e) (place e)))
     v
     ds)))

(define-method write-object ((self <message-entry>) port)
  (format port "#[~a ~a:]"
          (name (object-class self))
          (name (message self))))

(define-method to-sxml ((self <message-entry>))
  (let (((m <message>) (message self))
        ((t <time>) (timestamp self)))
    `(message-entry
      (message (@ (major ,(number->string (id (owner m))))
                  (minor ,(number->string (id m)))
                  (type ,(string (type-char m)))))
      (timestamp (@ (unix ,(~ "~.3f" (time->epoch-seconds t))))
                 ,(~ "~a.~06dZ" 
                     (time->string t "%Y-%m-%dT%H:%M:%S" #f)
                     (time-microseconds t)))
      (text ,(call-with-output-string
              (lambda (port)
                (let ((f (get-stripped-message-displayer m)))
                  (f port (message-arguments self)))))))))

(define-method to-string ((self <message-entry>))
  (call-with-output-string
   (lambda (port)
     (display-message port
                      (message self)
                      (message-arguments self) 
                      (place self)))))

(define (delta-strs v)
  ;;
  (define (str1 t1 t0)
    (sprintf-float " (+%.3f)" 40 (interval->seconds (time-time t1 t0))))
  ;;
  (let ((a (make-vector (vector-length v) "")))
    (let loop ((i 1)
               (maxw 0))
      (if (< i (vector-length v))
          (let ((s (str1 (timestamp (vector-ref v i))
                         (timestamp (vector-ref v (sub1 i))))))
            (vector-set! a i s)
            (loop (add1 i) (max maxw (string-length s))))
          (values a maxw)))))

;;;

(define (set-message-dest! dest)
  (assert (message-dest? dest))
  (set! *current-message-queue* dest))

(define (with-message-dest dest thunk)
  (assert (message-dest? dest))
  (thread-let ((*current-message-queue* dest))
    (thunk)))

              
(define (message-dest? item)
  (or (instance? item <output-port>)
      (instance? item <message-queue>)
      (instance? item <message-dest>)))

(define-class <message-forwarder> (<message-dest>)
  recipients)

(define (make-message-forwarder . rcpt)
  (for-each 
   (lambda (r)
     (if (not (message-dest? r))
         (error "make-message-forwarder: ~s not a message dest" r)))
   rcpt)
  ;;
  (make <message-forwarder>
        recipients: rcpt))

(define-method display-message ((self <message-forwarder>)
                                (msg <message>) (argv <vector>) plc)
  (for-each
   (lambda (rcpt)
     (display-message rcpt msg argv plc))
   (recipients self)))

