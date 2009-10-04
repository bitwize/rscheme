
(define-class <inet-addr> (<object>) :bvec)

(define-class <socket-addr> (<object>) :abstract)

(define-class <unix-socket-addr> (<socket-addr>) :bvec)

(define-class <inet-socket-addr> (<socket-addr>) :bvec)

(define-method to-string ((self <inet-addr>))
  (inet-addr->string self))

(define-method to-string ((self <unix-socket-addr>))
  (let ((str (make-string (bvec-length self))))
    (bvec-copy str 0 self 0 (bvec-length self))
    str))

(define-method to-string ((self <inet-socket-addr>))
  (bind ((host port (inet-socket-addr-parts self)))
    (string-append (inet-addr->string host) ":" (number->string port))))

;;

(define-syscall-glue (inet-addr->string (self <inet-addr>))
{
  REG0 = make_string( inet_ntoa( *self ) );
  RETURN1();
})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  convert a dotted IP address (eg, "206.81.237.66") 
;;;  to an <inet-addr>

(define-syscall-glue (string->inet-addr (ipaddr <raw-string>))
  literals: ((& <inet-addr>))
{
  struct in_addr a;
  int rc;

  if (ipaddr[0] == 0) {
    a.s_addr = INADDR_ANY;
    rc = 0;
  } else {
    rc = inet_aton( ipaddr, &a );
  }
  if (rc < 0) {
    RETURN0();
  } else {
    REG0 = bvec_alloc( sizeof a, TLREF(0) );
    memcpy( PTR_TO_DATAPTR(REG0), &a, sizeof a );
    RETURN1();
  }
})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  group an <inet-addr> and a port number together into
;;;  an <inet-socket-addr>

(define-syscall-glue (make-inet-socket-addr (host <inet-addr>)
					    (port <raw-int>))
  literals: ((& <inet-socket-addr>))
{
  struct sockaddr_in a;

  memset( &a, 0, sizeof a );

  a.sin_family = PF_INET;
  a.sin_addr = *host;
  a.sin_port = htons((short)port);

  REG0 = bvec_alloc( sizeof a, TLREF(0) );
  memcpy( PTR_TO_DATAPTR(REG0), &a, sizeof a );
  RETURN1();
})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  destructure an <inet-socket-addr>
;;;
;;;  returns an <inet-addr> and a <fixnum>, the host address and port #
;;;

(define-syscall-glue (inet-socket-addr-parts (addr <inet-socket-addr>))
 literals: ((& <inet-addr>))
{
  obj a;

  a = bvec_alloc( sizeof( struct in_addr ), TLREF(0) );
  memcpy( PTR_TO_DATAPTR(a), &addr->sin_addr, sizeof( struct in_addr ) );
  REG0 = a;
  REG1 = int2fx( ntohs(addr->sin_port) );
  RETURN(2);
})

;;;
;;; returns the canonical hostname and port number
;;; (returns a dotted IP address if the host info cannot be found)

(define (inet-socket-hostname (sa <inet-socket-addr>))
  (bind ((addr port (inet-socket-addr-parts sa))
	 (addr2 names (gethostinfo addr)))
    (values (if names
		(car names)
		(inet-addr->string addr))
	    port)))
