
(define-class <stat-buf> (<object>) :bvec)

;;;  This procedure, stat, operates against a raw (process-relative)
;;;  directory and is deprecated in favor of file-stat, which operates
;;;  against context-relative path objects (i.e., using context set up
;;;  using `within-directory')

(define-syscall-glue (stat (path <raw-string>))
 literals: ((& <stat-buf>))
{
  obj x;
  struct stat *sb;

  x = alloc( sizeof(struct stat), TLREF(0) );
  sb = (struct stat *)PTR_TO_DATAPTR(x);

  if (stat( path, sb ) < 0)
    REG0 = FALSE_OBJ;
  else
    REG0 = x;
  RETURN1();
})

(define-method file-stat ((self <string>))
  (file-stat (string->file self)))

(define-method file-stat ((self <file-name>))
  (stat (pathname->os-path (append-path (current-directory) self))))


(define-syntax access-mask
  (syntax-form () 0)
  ;;
  ;; these are useful until we get constant folding implemented...
  ;;
  (syntax-form ('read) 4)
  (syntax-form ('write) 2)
  (syntax-form ('execute) 1)
  (syntax-form ('exist) 0)
  ;;
  (syntax-form ('read . more) (bitwise-or 4 (access-mask . more)))
  (syntax-form ('write . more) (bitwise-or 2 (access-mask . more)))
  (syntax-form ('execute . more) (bitwise-or 1 (access-mask . more)))
  (syntax-form ('exist . more) (bitwise-or 0 (access-mask . more))))

(define-syscall-glue (file-access? (path <raw-string>) (mode <raw-int>))
{
  REG0 = rb_to_bo( access( path, mode ) == 0 );
  RETURN1();
})


(define-syscall-glue (readlink (path <raw-string>))
{
#ifdef MAXPATHLEN
  char buf[MAXPATHLEN+1];
#else
  char buf[1025];
#endif
  int rc;

  rc = readlink( path, buf, sizeof(buf)-1 );
  if (rc < 0) {
    os_error( "readlink", 1, raw_path );
  }
  buf[rc] = 0;
  REG0 = make_string(buf);
  RETURN1();
})

(define-syscall-glue (lstat (path <raw-string>))
 literals: ((& <stat-buf>))
{
  obj x;
  struct stat *sb;

  x = alloc( sizeof(struct stat), TLREF(0) );
  sb = (struct stat *)PTR_TO_DATAPTR(x);

  if (lstat( path, sb ) < 0)
    REG0 = FALSE_OBJ;
  else
    REG0 = x;
  RETURN1();
})

(define-syscall-glue (stat-type (stat <stat-buf>))
 literals: ('directory 'regular 'fifo 'character-special 'block-special
            'symbolic-link 'socket)
{
  int m = stat->st_mode;

  if (S_ISDIR(m))
    REG0 = LITERAL(0);
  else if (S_ISREG(m))
    REG0 = LITERAL(1);
  else if (S_ISFIFO(m))
    REG0 = LITERAL(2);
  else if (S_ISCHR(m))
    REG0 = LITERAL(3);
  else if (S_ISBLK(m))
    REG0 = LITERAL(4);
#ifdef S_ISLNK
  else if (S_ISLNK(m))
    REG0 = LITERAL(5);
#endif
#ifdef S_ISSOCK
  else if (S_ISSOCK(m))
    REG0 = LITERAL(6);
#endif
  else
    REG0 = FALSE_OBJ;
  RETURN1();
})

(define-syscall-glue (stat-mode (stat <stat-buf>))
{
  REG0 = int2fx( stat->st_mode );
  RETURN1();
})

(define-syscall-glue (stat-owner (stat <stat-buf>))
{
  REG0 = int2fx( stat->st_uid );
  REG1 = int2fx( stat->st_gid );
  RETURN(2);
})


(define-syscall-glue (stat-eq? (a <stat-buf>) (b <stat-buf>))
{
  /*  NOTE:  This is according to POSIX.  Some systems buggily
   *         assign the same (dev,ino) pair to distinct files,
   *         apparently including some Linux NFS servers with
   *         large dev numbers (>255) and large inode numbers (>16M).
   *
   *         See comments in GNU diffutils <src/system.h>
   */
  REG0 = rb_to_bo( (a->st_ino == b->st_ino)
		   && (a->st_dev == b->st_dev) );
  RETURN1();
})

(define-syscall-glue (stat-id->hash (stat <stat-buf>))
{
  UINT_32 loc[2];

  loc[0] = stat->st_ino;
  loc[1] = stat->st_dev;
  REG0 = raw_bytes_hash( loc, sizeof loc );
  RETURN1();
})

(define-syscall-glue (stat-id-vector (stat <stat-buf>))
{
  UINT_32 device, inode;

  inode = stat->st_ino;
  device = stat->st_dev;
  REG0 = make4( vector_class, 
	        int2fx( device >> 16 ),
	        int2fx( device & 0xFFFF ),
	        int2fx( inode >> 16 ),
	        int2fx( inode & 0xFFFF ) );
  RETURN1();
})

(define-syscall-glue (stat-mtime (stat <stat-buf>))
  literals: ((& <time>))
{
  REG0 = make_time_sec( stat->st_mtime, TLREF(0) );
  RETURN1();     
})

(define-syscall-glue (set-stat-times! (path <raw-string>) (mtime <time>) (atime <time>))
{
  struct utimbuf tbuf;
  int rc;

  tbuf.actime = atime->sec;
  tbuf.modtime = mtime->sec;
         
  rc = utime( path, &tbuf );
  if (rc < 0) {
    os_error( "utime", 1, raw_path );
  }
  RETURN0();
})

(define-syscall-glue (stat-times (stat <stat-buf>))
  literals: ((& <time>))
{
  REG0 = make_time_sec( stat->st_mtime, TLREF(0) );
  REG1 = make_time_sec( stat->st_atime, TLREF(0) );
  REG2 = make_time_sec( stat->st_ctime, TLREF(0) );
  RETURN(3);
})

(define-syscall-glue (stat-size (stat <stat-buf>))
{
  REG0 = uint_32_compact( stat->st_size );
  RETURN1();
})

(define (stat-directory? stat)
 (eq? (stat-type stat) 'directory))

(define (stat-file? stat)
 (eq? (stat-type stat) 'regular))

(define (stat-access? stat entity mode)
    (not (eq? (bitwise-and
	       (stat-mode stat)
	       (logical-shift-left
		(case mode
		  ((read) 4)
		  ((write) 2)
		  ((execute) 1)
		  (else (abort 'stat-access? 
			       "Bad stat mode: ~a" mode)))
		(case entity
		  ((owner) 6)
		  ((group) 3)
		  ((world) 0)
		  (else (abort 'stat-access? 
			       "Bad stat entity: ~a" entity)))))
	      0)))

