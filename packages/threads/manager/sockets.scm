
(define-class <socket-input-port> (<input-port>)
  (underlying-input-port type: <input-port>)
  (owner type: <peer-socket>))

(define-class <socket-output-port> (<output-port>)
  (underlying-output-port type: <output-port>)
  (owner type: <peer-socket>))

;;;

(define (make-service fd)
  (let ((s (make <service>
		 mbox: (make-mailbox fd)
		 fd: fd)))
    (make-accept-event fd s)
    s))

(define (close-service (self <service>))
  (if (free-accept-event self)
      (close (mbox self)))
  (values))
  
(define (get-next-client (s <service>))
  ;; discard errors arriving on the mbox
  (let loop ()
    (let ((n (receive-message! (mbox s))))
      (if (pair? n)
          (values (car n) (clone2 (cdr n) <inet-socket-addr>))
          (begin
            ;; we got an error; assume that it's about the socket
            ;; being accept()'ed and not about the listen socket...
            (make-accept-event (fd s) s)
            (loop))))))

;;;
;;;  `dir' 0 = read side, 1 = write site, 2 = both sides
;;;

(define-safe-glue (close-socket-side (fd <raw-int>) (dir <raw-int>))
{
  if (shutdown( fd, dir ) < 0)
    os_error( "shutdown", 2, raw_fd, raw_dir );
  RETURN0();
})

(define-safe-glue (free-accept-event (owner <service>))
{
  obj evt = gvec_ref( owner, SLOT(1) );
  struct sys_event *p = (struct sys_event *)OBJ_TO_RAW_PTR( evt );
  if (p) {
    free_accept_event( p );
    REG0 = TRUE_OBJ;
  } else {
    REG0 = FALSE_OBJ;
  }
  RETURN1();
})

(define-safe-glue (make-accept-event (fd <raw-int>) (owner <service>))
{
  REG0 = RAW_PTR_TO_OBJ( make_accept_event( fd, owner ) );
  RETURN1();
})


;;;


(define-method close ((self <server-socket>))
  (if (service self)
      (begin
        (close-service (service self))
        (set-service! self #f)
        (fd-close (filedes self)))
      (error "Already closed: ~s" self)))

(define-method finalize ((self <server-socket>))
  (if (service self)
      (close self)))

(define-method open-client-socket ((addr <inet-socket-addr>) #rest r)
  (case (length r)
    ((0) (open-client-socket* addr))
    ;; backward (pre 0.7.3.3-b25) compatible protocol:
    ((1) (open-client-socket* addr name: (car r)))
    ;; new protocol is keyword based
    (else (apply open-client-socket* addr r))))


  
                                 
(define (open-client-socket* (remote <inet-socket-addr>) 
                             #key 
                             (name default: #f)
                             (local default: #f))
  (let* ((s (internal-socket-connect remote name local))
         (fd (filedes s))
         (i (make <socket-input-port>
                  underlying-input-port: (filedes->input-port fd)
                  owner: s))
         (o (make <socket-output-port>
                  underlying-output-port: (filedes->output-port fd)
                  owner: s)))
    (set-input-port! s i)
    (set-output-port! s o)
    (register-for-finalization i)
    (register-for-finalization o)
    s))

;;; This is also used by the SSL code

(define (internal-socket-connect (remote <inet-socket-addr>) name local)
  (let (((s <initiator-socket>)
         (make <initiator-socket>
               filedes: (socket-create 
                         (socket-address-family->integer 
                          'address-family/internet)
                         (socket-type->integer 'socket-type/stream)
                         0)
               peer: remote
               name: name)))
    (fd-set-blocking (filedes s) #f)
    (if local
        (socket-bind/inet-sockaddr (filedes s) local))
    (let ((rc (do-connect s)))
      (cond
       ((eq? rc 0) s)
       ((fixnum? rc)
        (fd-close (filedes s))
        (signal (make <os-error>
                      error-number: rc
                      system-call: "connect"
                      arguments: (vector (filedes s) remote))))
       (else
        (fd-close (filedes s))
        (signal (make <os-error>
                      error-number: (cdr rc)
                      system-call: (case (car rc)
                                     ((0) "connect")
                                     ((1) "getsockopt")
                                     (else "do-connect"))
                      arguments: (vector (filedes s)))))))))


(define-method open-client-socket ((host <string>) (port <fixnum>) #rest rest)
  (apply open-client-socket
         (make-inet-socket-addr (to-inet-addr host) port)
         rest))

(%early-once-only
 (define ip-pattern
   (reg-expr->proc '(entire
                     (seq (+ digit) 
                          #\. (+ digit)
                          (? (seq #\. (+ digit)
                                  (? (seq #\. (+ digit))))))))))
                                         
(define-method to-inet-addr ((self <string>))
  (if (ip-pattern self)
      (string->inet-addr self)
      (gethostbyname self)))

(define-method to-inet-addr ((self <inet-addr>))
  self)

(define (open-server-socket port #key 
                            (local default: #f)
                            (name default: #f))
  (let* ((spec (if local
                   (make-inet-socket-addr (to-inet-addr local) port)
                   port))
         (fd (inet-server spec))
         (s (make <server-socket>
                  filedes: fd
                  name: name
                  service: (make-service fd))))
    (register-for-finalization s)
    s))

(define-method accept-client ((self <server-socket>))
  (bind ((fd peer (get-next-client (service self))))
    (fd-set-blocking fd #f)
    (let* ((s (make <responder-socket>
                    peer: peer
                    filedes: fd
                    input-port: #f
                    output-port: #f))
           (i (make <socket-input-port>
                    underlying-input-port: (filedes->input-port fd)
                    owner: s))
           (o (make <socket-output-port>
                    underlying-output-port: (filedes->output-port fd)
                    owner: s)))
      (set-input-port! s i)
      (set-output-port! s o)
      (register-for-finalization i)
      (register-for-finalization o)
      s)))

(define-method close ((self <peer-socket>) #optional side)
  (if (and (or (eq? side #f) (eq? side 'input))
           (input-port self))
      (begin
        (close-input-port (underlying-input-port (input-port self)))
        (set-input-port! self #f)
        (if (and (output-port self) (eq? side 'input))
            (close-socket-side (filedes self) 0))))
  (if (and (or (eq? side #f) (eq? side 'output))
           (output-port self))
      (begin
        (close-output-port (underlying-output-port (output-port self)))
        (set-output-port! self #f)
        (if (input-port self)
            (close-socket-side (filedes self) 1))))
  (if (and (not (input-port self))
           (not (output-port self)))
      (begin
        (fd-close (filedes self))
        (set-filedes! self -1)))
  (values))

(define-method finalize ((self <peer-socket>))
  (if (>= (filedes self) 0)
      (close self)))
  
;;

(define-safe-glue (do-connect (sock <initiator-socket>))
{
  obj saddr = gvec_ref( sock, INITIATOR_SOCKET_SADDR );
  struct sockaddr_in *a = (struct sockaddr_in *)PTR_TO_DATAPTR( saddr );
  int rc, fd;

  fd = fx2int( gvec_ref( sock, INITIATOR_SOCKET_FILEDES ) );
  rc = connect( fd, (struct sockaddr *)a, SIZEOF_PTR( saddr ) );

  if (rc < 0) {
     if (errno == EINPROGRESS) {
       gvec_write( sock, INITIATOR_SOCKET_WAITER, current_thread );
       make_connect_event( fd, sock );
       SAVE_CONT1( connect_fin );
       SWITCH_THREAD( sock, TSTATE_BLOCKED );
     } else {
       REG0 = cons( int2fx( 0 ), int2fx( errno ) );
       RETURN1();
     }
  } else {
    REG0 = ZERO;
    RETURN1();
  }
}
("connect_fin" {
  RESTORE_CONT1();
  RETURN1();
}))

;;;

(define-method close-input-port ((self <socket-input-port>))
  (close (owner self) 'input)
  (values))

(define-method finalize ((self <socket-input-port>))
  (close-input-port self))

(define-delegation ((self <socket-input-port>) (underlying-input-port self))
  (input-port-read-char self)
  (input-port-peek-char self)
  (input-port-char-ready? self)
  (input-port-scan-token self)
  (input-port-read self)
  (input-port-read-line self)
  (input-port-read-rest self)     ;; / constituents of internal
  (input-port-read-len self len)  ;; \ read-string protocol
  (input-port-read-max self len))

;;;

(define-method close-output-port ((self <socket-output-port>))
  (close (owner self) 'output)
  (values))

(define-method finalize ((self <socket-output-port>))
  (close-output-port self))

(define-delegation ((self <socket-output-port>) 
                    (underlying-output-port self))
  (flush-output-port self)
  (output-port-write-char self char)
  (write-string self string))

;;;

(define-method write-object ((self <server-socket>) port)
  (if (name self)
      (format port "#[~a ~s]" (name (object-class self)) (name self))
      (next-method)))
