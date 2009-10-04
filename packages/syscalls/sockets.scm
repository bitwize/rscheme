
(define-syscall-glue (socket-address-family->integer family)
    literals: ('address-family/unix 'address-family/internet)
{
    if (EQ(family,LITERAL(0)))
	REG0 = int2fx( PF_UNIX );
    else if (EQ(family,LITERAL(1)))
	REG0 = int2fx( PF_INET );
    else
	REG0 = FALSE_OBJ;
    RETURN1();
})

(define-syscall-glue (socket-type->integer type)
  literals: ('socket-type/stream
	     'socket-type/datagram
	     'socket-type/raw)
{
    if (EQ(type,LITERAL(0)))
	REG0 = int2fx( SOCK_STREAM );
    else if (EQ(type,LITERAL(1)))
	REG0 = int2fx( SOCK_DGRAM );
    else if (EQ(type,LITERAL(2)))
	REG0 = int2fx( SOCK_RAW );
    else
	REG0 = FALSE_OBJ;
    RETURN1();
})

(define-syscall-glue (socket-create (proto_family <raw-int>)
				    (sock_type <raw-int>)
				    (protocol <raw-int>))
{
int fd;

    fd = socket( proto_family, sock_type, protocol );
    REG0 = (fd < 0) ? FALSE_OBJ : int2fx(fd);
    RETURN1();
})

(define-syscall-glue (socket-listen (socket_fd <raw-int>)
				    (queue_depth <raw-int>))
{
int rc;

    rc = listen( socket_fd, queue_depth );
    if (rc < 0)
      os_error( "listen", 2, raw_socket_fd, raw_queue_depth );
    RETURN0();
})

(define-syscall-glue (socket-bind/unix (socket_fd <raw-int>)
				       (path <string>))
{
int rc;

    rc = bind( socket_fd, 
	       (struct sockaddr *)string_text(path), 
	       string_length(path) );
    if (rc < 0)
      os_error( "bind", 2, raw_socket_fd, raw_path );
    RETURN0();
})

(define-syscall-glue (socket-bind/inet (socket_fd <raw-int>)
                                       (port <raw-int>))
{
struct sockaddr_in addr;
int rc;

    memset( &addr, 0, sizeof addr );
    addr.sin_family = PF_INET;
    addr.sin_port = htons((short)port);
    addr.sin_addr.s_addr = INADDR_ANY;
    
    rc = bind( socket_fd, (struct sockaddr *)&addr, sizeof addr );
    if (rc < 0)
      os_error( "bind", 2, raw_socket_fd, raw_port );
    RETURN0();
})

(define-syscall-glue (socket-bind/inet-sockaddr (socket_fd <raw-int>)
                                                (addr <inet-socket-addr>))
{
  int rc;

  rc = bind( socket_fd, (struct sockaddr *)addr, SIZEOF_PTR(raw_addr) );
  if (rc < 0) {
    os_error( "bind", 2, raw_socket_fd, raw_addr );
  }
  RETURN0();
})


(define-syscall-glue (get-sock-or-peer-name (fd <raw-int>) 
					    sock_addr_class
					    sockq)
{
  char temp[128]; /* :-( */
  obj addr;
  int rc, name_len;

  name_len = 128;
  if (truish(sockq))
    {
      rc = getsockname( fd, (struct sockaddr *)temp, &name_len );
      if (rc < 0)
        os_error( "getsockname", 1, raw_fd );
    }
  else
    {
      rc = getpeername( fd, (struct sockaddr *)temp, &name_len );
      if (rc < 0)
        os_error( "getpeername", 1, raw_fd );
    }

  addr = bvec_alloc( name_len, sock_addr_class );
  memcpy( PTR_TO_DATAPTR(addr), temp, name_len );
  REG0 = addr;
  RETURN1();
})

(define (getsockname/inet fd)
  (get-sock-or-peer-name fd <inet-socket-addr> #t))

(define (getpeername/inet fd)
  (get-sock-or-peer-name fd <inet-socket-addr> #f))

(define-syscall-glue (socket-accept (socket_fd <raw-int>))
{
struct sockaddr_in addr;
int n, fd;

    memset( &addr, 0, sizeof addr );
    n = sizeof addr;
    fd = accept( socket_fd, (struct sockaddr *)&addr, &n );
    if (fd < 0)
    {
	REG0 = FALSE_OBJ;
	RETURN1();
    }
    else
    {
	REG0 = int2fx(fd);
	REG1 = make_string( inet_ntoa( addr.sin_addr ) );
	RETURN(2);
    }
})

(define (socket-connect/inet (socket_fd <fixnum>)
			     (port <fixnum>)
			     (host_id <string>))
  (connect/inet socket_fd 
		(make-inet-socket-addr (string->inet-addr host_id)
				       port)))

(define-syscall-glue (connect/inet (socket_fd <raw-int>) 
				   (addr <inet-socket-addr>))
{
int rc;

    rc = connect( socket_fd, (struct sockaddr *)addr, SIZEOF_PTR(raw_addr) );
    if (rc < 0)
      os_error( "connect", 2, raw_socket_fd, raw_addr );
    RETURN0();
})


(define-class <fd-select-set> (<object>) :bvec)

(define-glue (make-fd-set read_list write_list exception_list)
  literals: ("read-list" "write-list" "exception-list"
             (& <fd-select-set>))
{
  obj set, list;
  int i;
  fd_set *p;

  set = alloc( 3*sizeof(fd_set), TLREFB(3) );
  memset( PTR_TO_DATAPTR(set), 0, 3*sizeof(fd_set) );
  
  p = (fd_set *)PTR_TO_DATAPTR(set);
  for (i=0; i<3; i++, p++)
    {
      list = reg_ref(i);
      while (PAIR_P(list))
	{
	  obj fd;

	  fd = pair_car(list);
	  list = pair_cdr(list);
	  if (!OBJ_ISA_FIXNUM(fd) 
              || fx2int(fd) < 0 
              || fx2int(fd) >= FD_SETSIZE) {
            scheme_error( "make-fd-set: ~s in ~a is invalid",
                          2, fd, LITERAL(i) );
          }
          if (FD_ISSET( fx2int(fd), p )) {
            scheme_error( "make-fd-set: ~s appears twice in ~a",
                          2, fd, LITERAL(i) );
          }
	  FD_SET( fx2int(fd), p );
	}
      if (!EQ(list,NIL_OBJ)) {
        scheme_error( "make-fd-set: ~a is invalid at: ~s", 
                      2, LITERAL(i), list );
      }
  }
  REG0 = set;
  RETURN1();
})

(define-syscall-glue (fd-select delay_ms (set <fd-select-set>))
{
  fd_set temp[3];
  struct timeval *tp, t;
  int n;

  memcpy( temp, (fd_set *)PTR_TO_DATAPTR(set), 3*sizeof(fd_set) );

  if (OBJ_ISA_FIXNUM(delay_ms))
    {
      t.tv_sec = fx2int(delay_ms)/1000;
      t.tv_usec = (fx2int(delay_ms) % 1000)*1000;
      tp = &t;
    }
  else
    {
      tp = NULL;
    }
  
  n = select( FD_SETSIZE, &temp[0], &temp[1], &temp[2], tp );
  if (n < 0)
    {
      REG0 = FALSE_OBJ;
      RETURN1();
    }
  else if (n == 0)
    {
      REG0 = NIL_OBJ;
      REG1 = NIL_OBJ;
      REG2 = NIL_OBJ;
    }
  else
    {
      obj result;
      int i, fd;

      for (i=0; i<3; i++)
	{
	  result = NIL_OBJ;
	  for (fd=0; fd<FD_SETSIZE; fd++)
	    {
	      if (FD_ISSET(fd,&temp[i]))
		{
		  result = cons( int2fx(fd), result );
		}
	    }
	  reg_set(i,result);
	}
    }
  RETURN(3);
})

(define-syscall-glue (recv-from (fd <raw-int>)
				(buf <raw-string>)
				(offset <raw-int>)
				(len <raw-int>)
				(peekq <raw-bool>)
		                (oobq <raw-bool>)
				from_class)
{
  int n, from_len;
  char from[128];

  from_len = 128;

  n = recvfrom( fd, 
	        buf + offset,
	        len,
	        (peekq ? MSG_PEEK : 0) | (oobq ? MSG_OOB : 0),
	        (struct sockaddr *)from,
	        &from_len );

  if (truish(from_class))
    {
      REG1 = bvec_alloc( from_len, from_class );
      memcpy( PTR_TO_DATAPTR(REG1), from, from_len );
    }
  else
    REG1 = FALSE_OBJ;

  if (n < 0)
    REG0 = FALSE_OBJ;
  else
    REG0 = int2fx(n);
  RETURN(2);
})

(define-syscall-glue (send-to (fd <raw-int>)
			      (buf <raw-string>)
			      (offset <raw-int>)
			      (len <raw-int>)
			      (oobq <raw-bool>)
			      to)
{
 int n;

  n = sendto( fd, 
	      buf + offset,
	      len,
	      (oobq ? MSG_OOB : 0),
	      PTR_TO_DATAPTR(to),
	      SIZEOF_PTR(to) );
  if (n < 0)
    REG0 = FALSE_OBJ;
  else
    REG0 = int2fx(n);
  RETURN1();
})

;; convenience function to create a server socket on a given port

(define-method socket-binder ((port <fixnum>) fd)
  (socket-bind/inet fd port))

(define-method socket-binder ((sock <inet-socket-addr>) fd)
  (socket-bind/inet-sockaddr fd sock))

(define (inet-server addr)
  (let ((fd (socket-create (socket-address-family->integer 
                            'address-family/internet)
                           (socket-type->integer 'socket-type/stream)
                           0)))
    ;; reuse the addr in case we were using it just recently
    (set-socket-option fd 'level/socket 'socket/reuse-addr #t)
    ;;
    (socket-binder addr fd)
    (socket-listen fd 3)
    fd))

(define (inet-client host port)
  (let ((fd (socket-create (socket-address-family->integer 
                            'address-family/internet)
                           (socket-type->integer 'socket-type/stream)
                           0))
	(addr (cond
	       ((instance? host <string>)
		(string->hostaddr host))
	       ((instance? host <inet-addr>)
		host)
	       (else
		(error "~s: expected host name, ip address (string), or <inet-addr>"
		       host)))))
    (connect/inet fd (make-inet-socket-addr addr port))
    fd))
