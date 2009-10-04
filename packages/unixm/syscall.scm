
(define-unix-glue (mkdir (path <raw-string>) #rest rest)
{
  int mode = 0777;

  if (arg_count_reg > 2) {
    wrong_num_args_range( "mkdir", 1, 2 );
  }

  if (arg_count_reg > 1) {
    mode = fx2int( CHECK_FIXNUM( REG1 ) );
  }

  if (mkdir( path, mode ) < 0) {
    os_error( "mkdir", 1, raw_path );
  }
  RETURN0();
})

(define (mkdirs path)
  (let loop ((parents (dir-parents (string->dir path))))
    (if (null? parents)
	path
	(let* ((path (pathname->os-path (car parents)))
	       (s (stat path)))
	  (if s
	      (if (not (stat-directory? s))
		  (error "~a: not a directory" path))
	      (mkdir path))
	  (loop (cdr parents))))))

(define-unix-glue (system (str <raw-string>))
{
  int rc;
  rc = system( str );
  REG0 = int2fx(rc);
  RETURN1();
})

(define-unix-glue (getrusage)
  literals: ((& <interval>))
{
#if HAVE_GETRUSAGE
  struct rusage info;

  getrusage( RUSAGE_SELF, &info );
  REG0 = make8( vector_class, 
	        os_time( (struct timeval *)&info.ru_utime, 
			 TLREF(0) ),
	        os_time( (struct timeval *)&info.ru_stime, 
		         TLREF(0) ),
	        int2fx( info.ru_ixrss ),  /* integral shared memory size */
	        int2fx( info.ru_idrss ),  /* integral unshared data size */
	        int2fx( info.ru_inblock ),
	        int2fx( info.ru_oublock ),
	        int2fx( info.ru_msgsnd ),
	        int2fx( info.ru_msgrcv ) );
  RETURN1();
#else
  scheme_error( "getrusage: not implemented on this system", 0 );
  RETURN0();
#endif
})

;;;(define-unix-glue (errorstr (code <raw-int>)) ...)

(define-unix-glue (sleep sleep_time)
{
  if (OBJ_ISA_FIXNUM(sleep_time))
    sleep( fx2int(sleep_time) );
  else if (LONGFLOAT_P(sleep_time))
#if HAVE_USLEEP
    usleep( (int)(extract_float(sleep_time) * 1e6) );
#else
    sleep( (int)(extract_float(sleep_time) + 0.5) );
#endif
  else
    scheme_error( "sleep: invalid arg ~s (expected a number)", 1, sleep_time );
  RETURN0();
})

(define-unix-glue (fork)
{
  int pid = fork();
  if (pid < 0)
    os_error("fork",0);
  if (pid)
    REG0 = int2fx(pid);
  else
    REG0 = FALSE_OBJ;
  RETURN1();
})

(define-unix-glue (wait)
{
  int pid, stat;
  pid = wait( &stat );
  if (pid < 0)
    {
      REG0 = FALSE_OBJ;
      REG1 = FALSE_OBJ;
    }
  else
    {
      REG0 = int2fx(pid);
      REG1 = int2fx(stat);
    }
  RETURN(2);
})

(define-unix-glue (wait-for (pid <raw-int>))
{
  int rc, stat;

  rc = waitpid( pid, &stat, 0 );
  REG0 = int2fx(rc);
  REG1 = int2fx(stat);
  RETURN(2);
})

(define-unix-glue (unix-misc-1 (tag <raw-int>) opt)
{
  int rc = -1;
  static const char *(rsel[]) = { "getpid",
                                  "getpgrp",
                                  "getppid",
                                  "getuid",
                                  "geteuid",
                                  "getegid" };
  static const char *(wsel[]) = { "setpid",
                                  "setpgrp",
                                  "setppid",
                                  "setuid",
                                  "seteuid",
                                  "setegid" };
  switch (tag)
    {
    case 0: rc = getpid(); break;
    case 1: rc = getpgrp(); break;
    case 2: rc = getppid(); break;
    case 3: rc = getuid(); break;
    case 4: rc = geteuid(); break;
    case 5: rc = getegid(); break;
    default: scheme_error( "unix-misc-1: bad tag ~s", 1, raw_tag );
    }
  if (rc < 0) {
    os_error( rsel[tag], 0 );
  }
  if (truish(opt)) {
    int k = fx2int( opt );
    switch (tag)
      {
      case 2:
      case 0: rc = -1; errno = EINVAL; break;
      case 1: rc = setpgid( 0, k ); break;
      case 3: rc = setuid( k ); break;
      case 4: rc = seteuid( k ); break;
      case 5: rc = setegid( k ); break;
      }
    if (rc < 0) {
      os_error( wsel[tag], 1, opt );
    }
  }
  REG0 = int2fx(rc);
  RETURN1();
})

(define (getpid) (unix-misc-1 0 #f))
(define (getpgrp) (unix-misc-1 1 #f))
(define (getppid) (unix-misc-1 2 #f))
(define (getuid) (unix-misc-1 3 #f))
(define (geteuid) (unix-misc-1 4 #f))
(define (getegid) (unix-misc-1 5 #f))

(define (setpgrp arg) (unix-misc-1 1 arg))
(define (setuid arg) (unix-misc-1 3 arg))
(define (seteuid arg) (unix-misc-1 4 arg))
(define (setegid arg) (unix-misc-1 5 arg))

(define-unix-glue (getgroups)
{
  int i, n, rc, ngroups, did_malloc = 0;
  gid_t *gset, gset_tmp[20];
  obj vec;

  /*  POSIX defines passing 0 to return the number of groups 
   *  without trying to return the list 
   */
  n = getgroups( 0, &gset_tmp[0] );
  if (n < 0) {
    os_error( "getgroups", 0 );
  }
  if (n > 20) {
    did_malloc = 1;
    gset = (gid_t *)malloc( sizeof( gid_t ) * n );
  } else {
    gset = &gset_tmp[0];
  }

  rc = getgroups( n, gset );
  if (rc < 0) {
    if (did_malloc) {
      free( gset );
    }
    os_error( "getgroups", 0 );
  }
  vec = alloc( SLOT(rc), vector_class );

  /* Note that POSIX does not specify whether or not our effective gid
     (i.e., as returned by (getegid)) will be in the list.
  */
  for (i=0; i<rc; i++) {
    gvec_write_init( vec, SLOT(i), uint_32_compact( gset[i] ) );
  }
  if (did_malloc) {
    free( gset );
  }

  REG0 = vec;
  RETURN1();
})

(define-unix-glue (setgroups (vec <vector>))
{
  int rc, n, i, did_malloc = 0;
  gid_t *gset, gset_tmp[20];

  n = SIZEOF_PTR( vec ) / SLOT(1);
  if (n > 20) {
    gset = (gid_t *)malloc( sizeof( gid_t ) * n );
    if (!gset) {
      scheme_error( "setgroups: could not alloc group list for ~s", 1, vec );
    }
    did_malloc = 1;
  } else {
    gset = &gset_tmp[0];
  }

  for (i=0; i<n; i++) {
    gset[i] = basic_raw_uint( gvec_ref( vec, SLOT(i) ) );
  }

  rc = setgroups( n, gset );
  if (did_malloc) {
    free( gset );
  }
  if (rc < 0) {
    os_error( "setgroups", 1, vec );
  }
  RETURN0();
})

(define-unix-glue (getlogin)
{
  char *l = getlogin();

  if (l)
     REG0 = make_string(l);
  else
     REG0 = FALSE_OBJ;
  RETURN1();
})

(define-unix-glue (getpw who)
{
  struct passwd *pw = NULL;

  if (STRING_P(who))
    pw = getpwnam( string_text(who) );
  else if (OBJ_ISA_FIXNUM(who))
    pw = getpwuid( fx2int(who) );
  else
    scheme_error( "getpw: invalid arg '~s'", 1, who );

  if (pw)
    REG0 = make7( vector_class,
		  pw->pw_name ? make_string(pw->pw_name) : FALSE_OBJ,
		  int2fx(pw->pw_uid),
		  int2fx(pw->pw_gid),
		  pw->pw_dir ? make_string(pw->pw_dir) : FALSE_OBJ,
		  pw->pw_shell ? make_string(pw->pw_shell) : FALSE_OBJ,
		  pw->pw_passwd ? make_string(pw->pw_passwd) : FALSE_OBJ,
		  pw->pw_gecos ? make_string(pw->pw_gecos) : FALSE_OBJ );
  else
    REG0 = FALSE_OBJ;
  RETURN1();
})

(define-unix-glue (exec* (path <raw-string>) (argv <vector>) (env <vector>))
 literals: ("argv" "env")
{
  char **(dst[2]);
  int i, k;

  dst[0] = dst[1] = NULL;
  for (k=0; k<2; k++)
    {
      obj s, a;
      int n;
      char **v;

      s = (k==0) ? argv : env;
      n = SIZEOF_PTR(s)/SLOT(1);
      dst[k] = v = (char **)malloc( sizeof(char *) * (n + 1) );
      for (i=0; i<n; i++)
	{
	  a = gvec_read( s, SLOT(i) );
	  if (!STRING_P(a))
	  {
	    if (dst[0]) free(dst[0]);
	    if (dst[1]) free(dst[1]);
	    scheme_error( "exec*: ~a[~d] is ~s, not a string",
			  3,
			  (k==0) ? LITERAL(0) : LITERAL(1),
			  int2fx(i),
			  a );
	  }
	  v[i] = string_text( a );
	}
      v[n] = NULL;
    }
  execve( path, dst[0], dst[1] );
  free(dst[0]);
  free(dst[1]);
  os_error( "execve", 3, raw_path, argv, env );
  RETURN0();
})

(define-glue (exec path)
{
  char **argv;
  int i, n = arg_count_reg - 1;
  obj s, a;

  argv = (char **)malloc( sizeof(char *) * (n+1) );
  for (i=0; i<n; i++)
    {
      a = reg_ref( i+1 );
      if (!STRING_P(a))
	{
	  free(argv);
	  scheme_error( "exec: arg[~d] is ~s, not a string", 
		        2, int2fx(i), a );
	}
      argv[i] = string_text(a);
    }
  argv[i] = NULL;

  execv( string_text( CHECK_STRING( path ) ), argv );
  free( argv );
  COLLECT1();
  os_error( "execv", 2, path, REG1 );
  RETURN0(); /* never executed -- os_error() does not return */
})

(define-unix-glue (pipe)
{
  int fildes[2];
  if (pipe( fildes ) < 0)
      os_error( "pipe", 0 );
  REG0 = int2fx( fildes[0] );
  REG1 = int2fx( fildes[1] );
  RETURN(2);
})

(define-unix-glue (signal-name->number (sig_name <symbol>))
{
  int n = rs_c_signal_num( sig_name );
  if (n < 0) {
    scheme_error( "C signal '~s' is invalid", 1, sig_name );
  }
  REG0 = int2fx( n );
  RETURN1();
})

(define-unix-glue (kill (pid <fixnum>) (sig <fixnum>))
{
  REG0 = int2fx( kill( fx2int(pid), fx2int(sig) ) );
  RETURN1();
})

(define-unix-glue (hostname)
{
  char temp[1000];

  if (gethostname( temp, 1000 ) < 0)
     os_error( "gethostname", 0 );
  REG0 = make_string( temp );
  RETURN1();
})
