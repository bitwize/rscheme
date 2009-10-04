#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/file.h>
#include <string.h>

#include "lsspriv.h"

static int lssi_probe_file( const char *file, int writable, int *fmtv )
{
  char *vsep, ffile[1024];
  int fd ;
  UINT_32 magic_header[2];
  int n;
  
  vsep = strchr( file, ':' );
  if (vsep)
    {
      memcpy( ffile, file, vsep - file );
      ffile[vsep-file] = 0;
      file = ffile;
    }
  fd = open( file, writable ? O_RDWR : O_RDONLY );
  if (fd < 0)
    return -1;

  n = read( fd, magic_header, sizeof( UINT_32 ) * 2 );

  if (n == (sizeof( UINT_32 ) * 2))
    {
      switch (magic_header[0])
	{
	case LSS_MAGIC:
	  *fmtv = magic_header[1];
	  return fd;

	default:
	  close( fd );
	  errno = LSSERR_NOT_LSS;
	  return -1;
	}
    }
  else if (n < 0)
    {
      close( fd );
      return -1;
    }
  else
    {
      close( fd );
      errno = LSSERR_NOT_LSS;
      return -1;
    }
}

static int lssi_lock( int fd )
{
#if HAVE_FLOCK
  return flock( fd, LOCK_EX|LOCK_NB );
#else
# if HAVE_LOCKF
  return lockf( fd, F_TLOCK, 0 );
# else
  fprintf( stderr, "*** lss_lock() stubbed out; file not really locked!\n" );
  return 0;
# endif
#endif
}

/**
 *   `opts' is one of:                    var args are:
 *
 *     LSS_CREATE                         (int)filemode
 *     LSS_EXTEND                         (int)filemode, (char *)from
 *     LSS_EXTEND | LSS_BACKLEVEL         (int)filemode, (char *)from, (int)gen
 *     LSS_OPEN | LSS_RDONLY
 *     LSS_OPEN | LSS_RDWR
 *     LSS_OPEN | LSS_RDWR | LSS_BACKLEVEL    (int)gen    (*)
 *     LSS_OPEN | LSS_RDONLY | LSS_BACKLEVEL  (int)gen
 *
 *  (* - Use at your own risk -- committing a derivative from a
 *       backlevel makes existing later generations inaccessible)
 */

LSS *lss_vopen( const char *file, int opts, va_list va )
{
  LSS *lss = NULL;
  int wr = 0;

  if (opts & LSS_CREATE)
    {
      int filemode = va_arg( va, int );
      lss = lssv3_create( file, filemode );
      if (!lss)
	return NULL;
      wr = 1;
    }
  else if (opts & LSS_EXTEND)
    {
      int filemode = va_arg( va, int );
      const char *from = va_arg( va, const char * );
      int at = 0;
      if (opts & LSS_BACKLEVEL)
	at = va_arg( va, int );

      lss = lssv3_extend( file, filemode, from, at );
      if (!lss)
	return NULL;
      wr = 1;
    }
  else if (opts & LSS_OPEN)
    {
      int v, fd, at = 0;
      if (opts & LSS_BACKLEVEL)
	{
	  at = va_arg( va, int );
	}

      wr = (opts & LSS_RDWR) ? 1 : 0;
      fd = lssi_probe_file( file, wr, &v );
      if (fd < 0)
	return NULL;

      if (opts & LSS_RDWR)
	{
	  if (lssi_lock( fd ) < 0)
	    {
	      close( fd );
	      return NULL;
	    }
	}
      switch (v)
	{
	case 2:
	  lss = lssv2_open( file, fd, (opts & LSS_RDWR) ? 1 : 0, at);
	  break;
	case 3:
	  lss = lssv3_open( file, fd, (opts & LSS_RDWR) ? 1 : 0, at);
	  break;
	default:
	  errno = LSSERR_BAD_VER;
	  return NULL;
	}
      if (!lss)
	return NULL;
    }
  else
    {
      errno = EINVAL;
      return NULL;
    }
  /* all error cases should have exited */
  assert( lss );
  lss->writable = wr;
  return lss;
}

void lss_close( LSS *lss )
{
  lss->fn->close_meth( lss );
}

LSSAccess *lss_read_access( LSS *lss, UINT_32 recnum )
{
  return lss->fn->read_access_meth( lss, recnum );
}

void lss_readv( LSS *lss, zipbuf *vec, LSSAccess *a )
{
  lss->fn->readv_meth( lss, vec, a );
}

void lss_read_release( LSS *lss, LSSAccess *a )
{
  lss->fn->read_release_meth( lss, a );
}

void lss_writev( LSS *lss, UINT_32 recnum, zipbuf *vec, zip_algorithm *use )
{
  if (!lss->writable)
    lssi_signal_error( lss, LSSERR_READ_ONLY, "" );

  lss->fn->writev_meth( lss, recnum, vec, use );
}

LSS *lss_open( const char *file, int opts, ... )
{
  LSS *lss;
  va_list va;

  va_start( va, opts );
  lss = lss_vopen( file, opts, va );
  va_end( va );
  return lss;
}

UINT_32 lss_commit( LSS *lss, int flag )
{
  return lss->fn->commit_meth( lss, flag );
}

UINT_32 *lss_get_record_index( LSS *lss, UINT_32 *cnt )
{
  return lss->fn->get_index_meth( lss, cnt );
}

void lss_get_record_info( LSS *lss, UINT_32 record_num, 
			  struct LSSRecordInfo *info )
{
  lss->fn->get_record_info_meth( lss, record_num, info );
}

static _rs_volatile void default_handler( LSS *lss,
					  void *client_info,
					  int code,
					  char *fmt, va_list va )
{
  int i;
  int used[10];
  char *args[10];
  char temp[1000], *tmpp;

  for (i=0; i<10; i++)
    {
      used[i] = -1;
      args[i] = NULL;
    }

  tmpp = temp;
  for (i=0; fmt[i]; i++)
    {
      switch (fmt[i])
	{
	case 'i':
	  {
	    int v = va_arg( va, int );
	    args[i] = tmpp;
	    tmpp += 1 + sprintf( tmpp, "%d", v );
	    break;
	  }
	case 'l':
	  {
	    long v = va_arg( va, long );
	    args[i] = tmpp;
	    tmpp += 1 + sprintf( tmpp, "%ld", v );
	    break;
	  }
	case 's':
	  {
	    char *s = va_arg( va, char * );
	    /* cool -- the only unbounded one, we don't copy into temp[] */
	    args[i] = s;
	  }
	default:
	  {
	    void *p = va_arg( va, void * );
	    args[i] = tmpp;
	    tmpp += 1 + sprintf( tmpp, "(%c)%p", fmt[i], p );
	  }
	}
      used[i] = 0;
    }

  fprintf( stderr, "** LSS error: ", code );
  if (code == LSSERR_SYS_ERR)
    {
      fprintf( stderr, " %s (%d)\n", strerror(errno), errno );
    }
  else
    {
      char *msg = strlsserror( code );
      
      for (i=0; msg[i]; i++)
	{
	  if (msg[i] == '~')
	    {
	      int k = msg[i+1] - '0';
	      if (used[k] == -1)
		{
		  fprintf( stderr, "?(no arg ~%d)", k );
		}
	      else
		{
		  fprintf( stderr, args[k] );
		  used[k] = 1;
		}
	      i++;
	    }
	  else
	    {
	      fputc( msg[i], stderr );
	    }
	}
    }

  for (i=0; fmt[i]; i++)
    {
      if (!used[i])
	{
	  fprintf( stderr, "\n  arg[%d] => %s", i, args[i] );
	}
    }
  fprintf( stderr, "\n" );
  abort();
}

static _rs_volatile void call_handler( LSS *lss, int code, 
				       char *fmt, va_list va )
{
  lss_error_handler_t *errh;

  errh = lss->error_handler;
  if (!errh)
    errh = default_handler;
  
  errh( lss, lss->client_info, code, fmt, va );
}

_rs_volatile void lssi_signal_error( LSS *lss, int code, char *fmt, ... )
{
  va_list va;

  va_start( va, fmt );
  call_handler( lss, code, fmt, va );
}

_rs_volatile void lssi_sys_error( LSS *lss, char *fmt, ... )
{
  va_list va;

  va_start( va, fmt );
  call_handler( lss, LSSERR_SYS_ERR, fmt, va );
}

/*  the non-opacity is that all implementations store the
 *  uncompressed size at the beginning, as a `size_t'
 */

size_t lss_access_bytes( LSSAccess *a )
{
  if (a)
    return *(size_t *)a;
  else
    return 0;
}

static char *lssmsgs[] = {
  /* 0 */   "No error",
  /* 1 */   "Not an LSS (bad magic ~0)",
  /* 2 */   NULL,
  /* 3 */   NULL,
  /* 4 */   NULL,
  /* 5 */   NULL,
  /* 6 */   NULL,
  /* 7 */   NULL,
  /* 8 */   "Short write (only wrote ~0 out of ~1)"
};

char *strlsserror( int code )
{
  static char temp[20];

  if ((code >= LSSERR_MIN) && (code <= LSSERR_MAX))
    {
      int i = code - LSSERR_MIN;
      if ((i < (sizeof( lssmsgs ) / sizeof(char *))) && lssmsgs[i])
	{
	  return lssmsgs[i];
	}
      else
	{
	  sprintf( temp, "LSS Error %d", code );
	  return temp;
	}
    }
  else
    {
      return strerror( code );
    }
}

void lss_read_recnum( LSS *lss, void *buf, size_t len, UINT_32 recnum )
{
  struct zipbuf v[2];
  LSSAccess *a;

  a = lss_read_access( lss, recnum );

  if (!a)
    {
      lssi_signal_error( lss, LSSERR_NO_RECORD, "l", (long)recnum );
    }

  if (len != lss_access_bytes( a ))
    {
      lssi_signal_error( lss, LSSERR_WRONG_RECORD_SIZE, "ll",
			 (long)len, (long)lss_access_bytes(a) );
    }

  v[0].ptr = buf;
  v[0].limit = (char *)buf + len;
  v[1].ptr = NULL;

  lss_readv( lss, v, a );
  lss_read_release( lss, a );
}

void lss_write( LSS *lss, UINT_32 recnum, void *buf, size_t len, 
		zip_algorithm *use )
{
  struct zipbuf v[2];

  v[0].ptr = buf;
  v[0].limit = (char *)buf + len;
  v[1].ptr = NULL;
  lss_writev( lss, recnum, v, use );
}

/*
 *  copy the raw bits -- preserves the compression, because
 *  it does not uncompress the data
 *
 *  src == dst is valid and acts like `touch'
 */

size_t lss_copy_record( LSS *dst, LSS *src, UINT_32 recnum )
{
  return src->fn->copy_record_meth( src, dst, recnum );
}

/* (vol_num == -1) => latest volume */

const char *lss_filename( LSS *lss, int vol_num )
{
  return lss->fn->filename_meth( lss, vol_num );
}

UINT_32 lss_find_record_on_vol( LSS *lss, unsigned mask, int pass )
{
  return lss->fn->find_vrecord_meth( lss, mask, pass );
}

int lss_alloc_recs( LSS *lss, UINT_32 minrec, UINT_32 count, UINT_32 *result )
{
  return lss->fn->alloc_recs_meth( lss, minrec, count, result );
}

int lss_delete( LSS *lss, UINT_32 rec )
{
  return lss->fn->delete_meth( lss, rec );
}

int lss_delete_recs( LSS *lss, UINT_32 first, UINT_32 count )
{
  unsigned i;
  int rc;

  for (i=0; i<count; i++) {
    rc = lss_delete( lss, first+i );
    if (rc < 0) {
      return rc;
    }
  }
  return 0;
}

