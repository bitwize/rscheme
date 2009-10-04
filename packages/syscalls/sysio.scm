
(define-syscall fd-read ((fd <raw-int>)
			 (buf <raw-string>)
			 (offset <raw-int>)
			 (length <raw-int>)) => (<obj>)
{
  int rc;

  rc = read( fd, buf + offset, length );
  VALUES( (rc < 0) ? FALSE_OBJ : int2fx(rc) );
})

(define-syscall fd-write ((fd <raw-int>)
			  (buf <raw-string>)
			  (offset <raw-int>)
			  (length <raw-int>)) => (<obj>)
{
  int rc;

  rc = write( fd, buf + offset, length );
  VALUES( (rc < 0) ? FALSE_OBJ : int2fx(rc) );
})

(define-syscall fd-close ((fd <raw-int>)) => (<obj>)
{
  VALUES( (close(fd) < 0) ? FALSE_OBJ : TRUE_OBJ );
})

(define-syscall fd-open ((path <raw-string>)
			 (mode <raw-int>)
			 (perm <raw-int>)) => (<obj>)
{
  int fd, i_mode = O_RDONLY;

  if ((mode & 3) == 0)
    {
      i_mode = O_RDONLY;
    }
  else if ((mode & 3) == 1)
    {
      i_mode = O_WRONLY;
    }
  else if ((mode & 3) == 2)
    {
      i_mode = O_RDWR;
    }

  if (mode & 4)        i_mode |= O_APPEND;
  if (mode & 8)        i_mode |= O_CREAT;
  if (mode & (1<<4))   i_mode |= O_EXCL;
  if (mode & (1<<5))   i_mode |= O_TRUNC;

  fd = open( path, i_mode, perm );
  VALUES( (fd < 0) ? FALSE_OBJ : int2fx(fd) );
})

(define-syscall fd-lseek ((fd <raw-int>)
		          (offset <raw-int>)
			  (whence <raw-int>)) => (<obj>)
{
  int rc, i_whence;
  static int os_whence[3] = { SEEK_SET, SEEK_CUR, SEEK_END };

  assert( whence >= 0 && whence < 3 );

  rc = lseek( fd, offset, os_whence[whence] );
  VALUES( (rc < 0) ? FALSE_OBJ : int2fx(rc) );
})

(define-syscall fd-dup ((fd <raw-int>)) => (<raw-int>)
{
  int rc = dup( fd );
  if (rc < 0)
     os_error( "dup", 1, int2fx(fd) );
  VALUES(rc);
})

(define-syscall fd-dup2 ((fd1 <raw-int>) (fd2 <raw-int>)) => (<raw-int>)
{
  int rc = dup2( fd1, fd2 );
  if (rc < 0)
     os_error( "dup2", 2, int2fx(fd1), int2fx(fd2) );
  VALUES(rc);
})

(define-syscall errno () => (<raw-int>)
{
  extern int errno;
  VALUES(errno);
})

;;
;;          socket options
;;

(define-syscall local-socket-option ((opt <fixnum>)) => (<raw-int>)
{
  static int sockopts[] = { SO_DEBUG, SO_REUSEADDR, SO_KEEPALIVE, 
			      SO_DONTROUTE,
			      SO_LINGER, SO_BROADCAST, SO_OOBINLINE,
			      SO_SNDBUF, SO_RCVBUF, 
			      SO_TYPE, SO_ERROR };
  VALUES(sockopts[fx2int(opt)]);
})

(define-syscall local-socket-level ((opt <fixnum>)) => (<raw-int>)
{
  static int socklevels[] = { SOL_SOCKET, 
			      IPPROTO_IP, 
			      IPPROTO_TCP, 
			      IPPROTO_UDP };
  VALUES(socklevels[fx2int(opt)]);
})


(define-syscall setsockopt-int ((fd <raw-int>)
				(level <raw-int>)
				(opt <raw-int>)
				(val <raw-int>)) => (<raw-int>)
{
 int temp = val;

  VALUES( setsockopt( fd, level, opt, (void *)&temp, sizeof temp ) );
})

(define-syscall setsockopt-time ((fd <raw-int>)
				 (level <raw-int>)
				 (opt <raw-int>)
				 (d <obj>)) => (<raw-int>)
{
 struct timeval t;

 t.tv_sec = PTR_TO_SCMTIME(d)->sec;
 t.tv_usec = PTR_TO_SCMTIME(d)->usec;

 VALUES( setsockopt( fd, level, opt, (void *)&t, sizeof t ) );
})

(define-syscall setsockopt-linger ((fd <raw-int>)
				   (level <raw-int>)
				   (opt <raw-int>)
				   (ling <obj>)) => (<raw-int>)
{
 struct linger l;

 if (OBJ_ISA_FIXNUM(ling))
   {
     l.l_onoff = 1;
     l.l_linger = fx2int(ling);
   }
 else
   {
     l.l_onoff = 0;
     l.l_linger = 0;
   }

 VALUES( setsockopt( fd, level, opt, (void *)&l, sizeof l ) );
})

(define-syscall file-truncate ((path <raw-string>) (len <raw-int>))
{
  if (truncate( path, len ) < 0)
     os_error( "truncate", 2, make_string(path), int2fx(len) );
  VALUES();
})

(define-syscall fd-truncate ((fd <raw-int>) (len <raw-int>))
{
  if (ftruncate( fd, len ) < 0)
     os_error( "ftruncate", 2, int2fx(fd), int2fx(len) );
  VALUES();
})
