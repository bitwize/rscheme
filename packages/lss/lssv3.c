#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <errno.h>
#if HAVE_ALLOCA_H
#include <alloca.h>
#endif
#include <stdarg.h>
#include <signal.h>  /* for ##PANIC## state */

#include <time.h>

#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>
#include <sys/uio.h>
#include <fcntl.h>

#include "lsspriv.h"
#include "lssv3.h"
#include <rscheme/irc.h>           /* for RS_LPGC() */

#define DEBUG_PRINT             (0)
#define CAPTURE_IOTRACE         (0)

static void lss_panic( const char *file, int line, const char *msg )
{
  int me = getpid();

  fprintf( stderr,
           "%s:%d: ############# PANIC (%s) #############\n", 
           file, line, msg );
  fprintf( stderr, 
           "%s:%d: ##  SIGSTOP'ing self (%d) and everyone else in PGRP\n", 
           file, line, me );

  kill( 0, SIGSTOP );
}

#define LSS_PANIC(msg) lss_panic( __FILE__, __LINE__, msg )

#if CAPTURE_IOTRACE

void iotrace( LSS_V3 *ptr, const char *msg, ... )
{
  va_list va;
  char tmp[256];

  va_start( va, msg );
  vsprintf( tmp, msg, va );
  va_end( va );
  fprintf( stderr, "%%[ %d <%p> (io) %s ]%%\n", getpid(), ptr, tmp );
}

#define IOTRACE(args) iotrace args
#else
#define IOTRACE(args) do { ; } while (0)
#endif

#define ACCESS_IN(a)   ((a)->in_buf)
#define HEADER_FOR(a)  (((struct LSSRecordHeader *)((a)->addr)) - 1)


static struct LSSMethods lss_v3;


static void lssi_close( LSS *generic_ptr );
static void lssi_write_done_z( LSS_V3 *lss, LSSAccess *a, 
			      UINT_32 z_len, UINT_32 unz_len, int z_alg );
#define lssi_write_done_noz(lss,a,len)  lssi_write_done_z(lss,a,len,len,0)

static UINT_32 lssi_write_alloc( LSS_V3 *lss, LSSAccess *a, UINT_32 space, 
				 UINT_32 type );
static void *lssi_read_it( LSS_V3 *lss, UINT_32 at, UINT_32 type );
static void lssi_update_volume_headers( LSS_V3 *lss );
static int lssi_read_chk( LSS_V3 *lss, UINT_32 at, void *ptr, UINT_32 len,
			  UINT_32 type );
static int lssi_read( LSS_V3 *lss, UINT_32 at, void *ptr, UINT_32 len );

static LSS_V3 *v3chk( LSS *generic_ptr )
{
  if (generic_ptr->fn != &lss_v3)
    {
      lssi_signal_error( generic_ptr, LSSERR_NOT_IMPL, "" );
    }
  return (LSS_V3 *)generic_ptr;
}

static UINT_64 lssi_time_ms( void )
{
  struct timeval tv;
  gettimeofday( &tv, NULL );
  /* XXX FIXME -- work with (UINT_16 * 4) implementation of UINT_64 */
  return (UINT_64)tv.tv_sec * (UINT_64)1000 + (tv.tv_usec / 1000);
}

static UINT_64 lssi_gen_serno( int fd )
{
  struct timeval tv;
  UINT_64 serno;
  UINT_32 state;
  int i;
  struct stat sb;

  gettimeofday( &tv, NULL );

  state = (tv.tv_usec + tv.tv_sec) * 4;
  if (fstat( fd, &sb ) == 0)
    {
      state += (sb.st_dev << 10) + sb.st_ino;
    }
  state ^= 0x317febc1;
  serno = 0;

  for (i=0; i<64; i++)
    {
      serno = (serno << 1) + (state & 1);
      switch (state & 3)
	{
	case 0:
	case 3:
	  state = (state >> 1);
	  break;
	case 1:
	case 2:
	  state = (state >> 1) + 0x80000000;
	  break;
	}
    }
  return serno;
}

/**
 *   Write out a data segment (DSEG) from the contents of some
 *   output buffers.
 */

static void lssi_flush_obufsv( LSS_V3 *lss,
                               struct LSSVolume *invol,
                               struct IOBuf *buf,
                               int nbuf )
{
  struct iovec iov[MAX_OUT_BUFS+2];
  int i, ntrailer, nbytes, rc;
  struct LSSRecordHeader gap;

  nbytes = 0;
  for (i=0; i<nbuf; i++)
    {
      assert( buf[i].num_accesses == 0 );

      iov[i].iov_base = (void *)buf[i].base;
      iov[i].iov_len = buf[i].ptr - buf[i].base;
      nbytes += iov[i].iov_len;
#if DEBUG_PRINT
      printf( "iov[%d] -- %d bytes @ %p\n", i, 
	      iov[i].iov_len, iov[i].iov_base );
#endif /* DEBUG_PRINT */
      
      buf[i].ptr = buf[i].base;
    }
  IOTRACE(( lss, "append %d %d data", invol - &lss->vol[0], nbytes ));

  if (nbytes == 0)
    return;

  ntrailer = IO_BLOCK_SIZE - (nbytes & (IO_BLOCK_SIZE - 1));
  if (ntrailer > sizeof( struct LSSSegmentTrailer ))
    {
      /* add a GAP to make the DSEG appear at the end of an IO_BLOCK */

      nbytes += sizeof gap;
      ntrailer = IO_BLOCK_SIZE - (nbytes & (IO_BLOCK_SIZE - 1));

      gap.magic = GAP_MAGIC;
      gap.recnum = 0;
      /*  the `gap.space' measures the size of the gap including
       *  the gap header.  Since the gap header and the trailer
       *  are the same size, and `ntrailer' is the amount of trailer
       *  including the trailer block, the gap space = ntrailer
       */
      gap.space_word = MAKE_SPACE_WORD( ntrailer, 0 );
      gap.length = 0;

      iov[i].iov_base = (void *)&gap;
      iov[i].iov_len = sizeof gap;
      i++;
      IOTRACE(( lss, "append %d %d gap", invol - &lss->vol[0], sizeof gap ));
    }

  iov[i].iov_base = (void *)(lss->trailing_buf + IO_BLOCK_SIZE - ntrailer);
  iov[i].iov_len = ntrailer;
  i++;
  IOTRACE(( lss, "append %d %d trailer", invol - &lss->vol[0], ntrailer ));

  lss->trailer->magic = DATASEG_MAGIC;
  lss->trailer->length = nbytes + ntrailer;

  rc = lseek( invol->filedes, 0, SEEK_END );
  rc = ROUND_UP( rc );
#if DEBUG_PRINT
  printf( "rc = %d, buf[0].offset = %lu\n", rc, buf[0].base_offset );
#endif /* DEBUG_PRINT */

  if (rc != buf[0].base_offset)
    {
      if (rc < 0)
	lssi_sys_error( &lss->com, "" );
      else
	lssi_signal_error( &lss->com, LSSERR_NOT_WHERE_EXPECTED,
			   "ll", (long)rc, (long)lss->outbuf[0].base_offset );
    }

#if DEBUG_PRINT
  printf( "tip fd %d\n", invol->filedes );
#endif /* DEBUG_PRINT */
  assert( i <= (MAX_OUT_BUFS+2) );

  errno = 0;
  rc = writev( invol->filedes, iov, i );
  if (rc < 0)
    {
      lssi_sys_error( &lss->com, "l", (long)(nbytes + ntrailer) );
    }
  if (rc != (nbytes + ntrailer))
    {
      lssi_signal_error( &lss->com,
			 LSSERR_SHORT_WRITE,
			 "ll",
			 (long)rc,
			 (long)nbytes + (long)ntrailer );
    }

#if DEBUG_PRINT
  printf( "%s: wrote %d bytes (%d is trailer)\n", 
	  lss->tip_vol->file, rc, ntrailer );
#endif /* DEBUG_PRINT */
  buf[0].base_offset += rc;
}

static void lssi_flush_obufs( LSS_V3 *lss )
{
  int i;

  lssi_flush_obufsv( lss, lss->tip_vol, lss->outbuf, lss->num_outbufs );

  for (i=1; i<lss->num_outbufs; i++)
    {
      free( lss->outbuf[i].base );
    }
  lss->num_outbufs = 1;
}

/**
 *  Flush the in-memory commit record to disk,
 *  thereby committing the changes
 */

/**
 *  NOTE:  We could use the existing output buffering mechanism,
 *  if we just provide `flush_out' with a flag that says
 *  "sync before&after last IOBLOCK"
 *
 *  though we need to pad things so that the commit record goes
 *  at the end of the last io block
 */

static void lssi_write_cr( LSS_V3 *lss, int flag )
{
  int rc;
  int fd = lss->tip_vol->filedes;
  struct iovec iov[3];
  struct LSSRecordHeader gap;
  UINT_32 ntrailer, at;

  off_t lrc = lseek( fd, 0, SEEK_END );

  if (lrc < 0)
    {
      lssi_sys_error( &lss->com, "" );
    }

  at = lrc;

  if (lss->do_fsync) {
    rc = fsync( fd );
    if (rc < 0)
      {
        lssi_sys_error( &lss->com, "" );
      }
  }

  lss->cr.prev_cr_at = lss->cr.self_cr_at;
  lss->cr.self_cr_at = MAKE_BASED_LOCATOR( lss->tip_base , at );

  lss->cr.cr_space_w = MAKE_SPACE_WORD( CR_SPACE, 0 );

  iov[0].iov_base = (void *)&lss->cr;
  iov[0].iov_len = CR_SPACE;

  IOTRACE(( lss, "append %d %d commit %d", lss->tip_vol - &lss->vol[0], CR_SPACE, flag ));

  ntrailer = IO_BLOCK_SIZE - (iov[0].iov_len + sizeof gap);

  if ((flag > 0) && (flag <= NUM_BOOKMARKS))
    {
      lss->cr.bookmarks[flag-1] = lss->cr.self_cr_at;
    }

  gap.magic = GAP_MAGIC;
  gap.length = 0;
  gap.recnum = 0;
  /* see above comment re `gap.space = ntrailer' */
  gap.space_word = MAKE_SPACE_WORD( ntrailer, 0 ); 

  iov[1].iov_base = (void *)&gap;
  iov[1].iov_len = sizeof gap;
  IOTRACE(( lss, "append %d %d commit-gap", lss->tip_vol - &lss->vol[0], sizeof gap ));

  iov[2].iov_base = (void *)(lss->trailing_buf + IO_BLOCK_SIZE - ntrailer);
  iov[2].iov_len = ntrailer;
  IOTRACE(( lss, "append %d %d commit-trailer", lss->tip_vol - &lss->vol[0], ntrailer ));

  lss->trailer->magic = EOF_MAGIC;
  lss->trailer->length = IO_BLOCK_SIZE;

  lss->cr.generation++;
  lss->cr.commit_time_ms = lssi_time_ms();

  rc = writev( fd, iov, 3 );
  if (rc != IO_BLOCK_SIZE)
    {
      if (rc < 0)
	lssi_sys_error( &lss->com, "" );
      else
	lssi_signal_error( &lss->com, LSSERR_WRITE_COM_FAILED, "" );
    }

  if (lss->do_fsync) {
    rc = fsync( fd );
    if (rc < 0)
      {
        lssi_sys_error( &lss->com, "" );
      }
  }

  /*  adjust the base offset of the output buffer to refer
   *  PAST the CR we just wrote -- since this mechanism
   *  bypasses the output buffering system
   */
  lss->outbuf[0].base_offset += IO_BLOCK_SIZE;

  if (RS_LPGC_ACTIVE()) {
    struct stat sb;
    int i, rc;
    char *p, buf[MAX_VOLUMES*16+1];

    p = &buf[0];
    *p++ = '(';
    for (i=0; i<lss->num_vols; i++) {
      rc = fstat( lss->vol[i].filedes, &sb );
      if (i) {
        *p++ = ' ';
      }
      if (rc < 0) {
        *p++ = 'E';
      } else {
        p += sprintf( p, "%lu", sb.st_size );
      }
    }
    *p++ = ')';
    *p = 0;
    RS_LPGC( 462, 5400, "stat %s", buf );
  }
}

static UINT_32 lssi_write_out( LSS_V3 *lss, const void *data, UINT_32 len, 
			       UINT_32 type )
{
  UINT_32 at;
  LSSAccess a;

  at = lssi_write_alloc( lss, &a, len, type );
  memcpy( a.addr, data, len );
  lssi_write_done_noz( lss, &a, len );
  return at;
}

static void lssi_init_bufs( LSS_V3 *l )
{
  l->num_outbufs = 1;
  l->outbuf[0].base = malloc( FIRST_OBUF_SIZE );
  l->outbuf[0].ptr = l->outbuf[0].base;
  l->outbuf[0].limit = l->outbuf[0].base + FIRST_OBUF_SIZE;
  l->outbuf[0].num_accesses = 0;
  l->outbuf[0].base_offset = 0;
  l->outbuf[0].vol_base = 0; /* XXX ? */
  l->outbuf[0].last_access = NULL;

  l->trailing_buf = malloc( IO_BLOCK_SIZE );
  memset( l->trailing_buf, 0, IO_BLOCK_SIZE );
  l->trailer = (struct LSSSegmentTrailer *)
                   ((l->trailing_buf + IO_BLOCK_SIZE) - TRAILER_SIZE);
  l->trailer->trailer_len = 0;
  l->trailer->trailer_space_w = MAKE_SPACE_WORD( STORAGE_GRANULE, 0 );
}

static void lssi_write_line( LSS_V3 *lss, IndexEntry *ip )
{
  LSSAccess a;
  UINT_32 at;
  unsigned i;

  at = lssi_write_alloc( lss,
			 &a,
			 sizeof( struct LSSIndexEntries ),
			 INDEX_MAGIC );

  memcpy( a.addr, ip->mem_ptr, sizeof( struct LSSIndexEntries ) );
  HEADER_FOR(&a)->recnum = ip->m.line_key;

  lssi_write_done_noz( lss, &a, sizeof( struct LSSIndexEntries ) );
  ip->m.line_offset = at;
  /*
   *  update the 'HAS_FREE' flag
   */

  ip->m.vol_flags &= ~VOLFLAG_HAS_FREE;
  for (i=0; i<INDEX_LINE_SIZE; i++) {
    if (ip->mem_ptr->entry[i] == 0) {
      ip->m.vol_flags |= VOLFLAG_HAS_FREE;
      break;
    }
  }
}

static int lssi_alloc_auxbuf( LSS_V3 *lss, int v )
{
  struct IOBuf *b = malloc( sizeof( struct IOBuf ) + MIN_OBUF_SIZE );

  b->base = (UINT_8 *)(b + 1);
  b->ptr = b->base;
  b->limit = b->base + MIN_OBUF_SIZE;
  b->num_accesses = 0;
  b->last_access = NULL;
  b->base_offset = ROUND_UP( lseek( lss->vol[v].filedes, 0, SEEK_END ) );
  b->vol_base = MAKE_LOCATOR( v, 0 );

  lss->vol[v].auxbuf = b;
}

static void lssi_flush_auxbufs( LSS_V3 *lss )
{
  int i;

  for (i=0; i<lss->num_vols; i++)
    {
      if (lss->vol[i].auxbuf)
        {
          lssi_flush_obufsv( lss, &lss->vol[i], lss->vol[i].auxbuf, 1 );
        }
    }
}

static UINT_32 lssi_commit( LSS *generic_ptr, int flag )
{
  LSS_V3 *lss = (LSS_V3 *)generic_ptr;

  /* write out the index records, if needed */

  if (lss->cr.mix_offset == 0)
    {
      UINT_32 i, j, n, cap;
      struct MasterIndexEntry *mix;

      cap = lss->mindex_cap;
      n = lss->cr.mix_cnt;
      mix = ALLOCN( struct MasterIndexEntry, n );

#if DEBUG_PRINT
      printf( "flushing index (%lu lines)\n", n );
#endif /* DEBUG_PRINT */

      /* collect all the lines, and write out the dirty ones */
      j = 0;
      for (i=0; i<cap; i++)
	{
	  struct IndexEntry *ip = &lss->mindex[i];

	  if (ip->mem_ptr)
	    {
	      if (ip->m.line_offset == 0)
		{
		  lssi_write_line( lss, ip );  /* updates `line_offset' */
		}
	    }
	  if (ip->m.line_offset)
	    {
	      mix[j++] = ip->m;
	    }
	}
      /*  verify that we found exactly the right
       *  number of entries
       */
      assert( j == n );

      lss->cr.mix_offset = lssi_write_out( lss, 
					   mix,
					   n*sizeof( struct MasterIndexEntry ),
                                           MINDEX_MAGIC );
      free( mix ); /* that was a temporary representation */
      /* all the diffs have been flushed out */
      lss->cr.num_diffs = 0;
      memset( &lss->cr.diffs, 0, sizeof(lss->cr.diffs) );
    }

  lss->cr.vh_fuel -= COMMIT_FUEL_COST;

  /*  however, if we have not written out the volume header yet,
   *  then flush the output buffer so `update' can read it back
   *  in.
   */
  if (lss->outbuf[0].base_offset == 0)
    {
      /*  if we were really smart, we could probably just modify
       *  it in place, instead of writing it and then reading
       *  it in order to update it...  future work.
       */
      lssi_flush_obufs( lss );
    }

  /* update volume header if (necessary or) desired */
  if (lss->cr.vh_fuel <= 0)
    {
      lssi_update_volume_headers( lss );
      lss->cr.vh_fuel = VOLUME_HEADER_FUEL;
    }
  /* flush the data segment */
  lssi_flush_obufs( lss );

  /* flush any other auxillary buffers */
  lssi_flush_auxbufs( lss );

  /*  note that we have to commit AFTER updating the volume header
   *  because new compression algorithms may have been specified,
   *  and we can't run without them!
   *
   * -- on the other hand, what if we crash before the commit;
   * will the volume header have an incorrect forward pointer to
   * the CR?  We should handle that at load time...
   */
  lssi_write_cr( lss, flag );

#if DEBUG_PRINT
  printf( "fuel left in volume header: %d\n", lss->cr.vh_fuel );
#endif /* DEBUG_PRINT */
  return lss->cr.generation;
}

static LSS_V3 *lssi_new( void )
{
  LSS_V3 *l = (LSS_V3 *)malloc( sizeof( LSS_V3 ) );
  memset( l, 0, sizeof( LSS_V3 ) );
  LSS_INIT_COM( l->com, &lss_v3 );  /* set up function pointers */
  
  memset( &l->zip_algs[0], 0, sizeof( l->zip_algs ) );

  l->zip_algs[0] = &lss_null_zip;
  l->zip_algs[1] = NULL;
  l->tip_vol = NULL;
  initFreeMap( &l->freemap );
  l->do_fsync = 1;
  return l;
}

/**
 *  Return -1 on error
 */

static int lssi_read_volume_header( int fd, struct LSSVolumeHeader *vh )
{
  int rc;
  struct LSSRecordHeader rh;

  rc = lseek( fd, 0, SEEK_SET );
  if (rc != 0)
    {
      return -1;
    }

  rc = read( fd, &rh, sizeof rh );
  if (rc != sizeof rh)
    {
      errno = LSSERR_SHORT_READ;
      return -1;
    }

  if ((rh.magic != LSS_MAGIC)
      || (rh.recnum != LSSX_VERSION)
      || (rh.length != sizeof( struct LSSVolumeHeader )))
    {
      errno = LSSERR_NOT_LSS;
      return -1;
    }

  rc = read( fd, vh, sizeof( struct LSSVolumeHeader ) );
  if (rc != sizeof( struct LSSVolumeHeader ))
    {
      errno = LSSERR_SHORT_READ;
      return -1;
    }

  return 0;
}

static void lssi_update_one_volume_header( LSS_V3 *lss, int vol )
{
  int rc;
  int fd = lss->tip_vol->filedes;
  struct LSSVolumeHeader vh;
  int nz;

  rc = lseek( fd, sizeof( struct LSSRecordHeader ), SEEK_SET );
  if (rc != sizeof( struct LSSRecordHeader ))
    {
      lssi_signal_error( &lss->com, LSSERR_NOT_WHERE_EXPECTED,
			 "ll", 
			 (long)rc, (long)sizeof( struct LSSRecordHeader ) );
    }

  rc = read( fd, &vh, sizeof vh );
  if (rc != sizeof vh)
    {
      if (rc < 0)
	{
	  lssi_sys_error( &lss->com, "" );
	}
      else
	{
	  lssi_signal_error( &lss->com, LSSERR_SHORT_READ, 
			     "ll", (long)rc, (long)(sizeof vh) );
	}
    }

#if DEBUG_PRINT
  printf( "updating volume header (generation %lu++)\n", vh.vh_generation );
#endif /* DEBUG_PRINT */

  vh.vh_generation++;

  vh.last_cr_at = lss->cr.self_cr_at;
  vh.last_cr_generation = lss->cr.generation;

  /* add any new zip algorithms */

  for (nz = 1; (nz < MAX_ZIP_ALGORITHMS) && vh.zip_alg_at[nz-1]; nz++)
    {
    }
  /*  printf( "had %d algorithms listed\n", nz ); */
  while (lss->zip_algs[nz])
    {
      const char *name = lss->zip_algs[nz]->name;
#if DEBUG_PRINT
      printf( " -- adding zip algorithm[%d] = %s\n", nz, name );
#endif
      vh.zip_alg_at[nz-1] = lssi_write_out( lss, name, strlen( name ) + 1,
					    ZIPA_MAGIC );
      nz++;
    }

  rc = lseek( fd, sizeof( struct LSSRecordHeader ), SEEK_SET );
  if (rc != sizeof( struct LSSRecordHeader ))
    {
      lssi_signal_error( &lss->com, LSSERR_NOT_WHERE_EXPECTED,
			 "ll", 
			 (long)rc, (long)sizeof( struct LSSRecordHeader ) );
    }

  rc = write( fd, &vh, sizeof vh );
  if (rc != sizeof vh)
    {
      if (rc < 0)
	{
	  lssi_sys_error( &lss->com, "" );
	}
      else
	{
	  lssi_signal_error( &lss->com, LSSERR_SHORT_WRITE,
			     "ll", (long)rc, (long)(sizeof vh)  );
	}
    }
}

static void lssi_update_volume_headers( LSS_V3 *lss )
{
  int i;

  /*
   *  update all volume headers that are open for WRITE access
   */
  for (i=0; i<lss->num_vols; i++)
    {
      if (lss->vol[i].flags & LSS_RDWR)
        {
          lssi_update_one_volume_header( lss, i );
        }
    }
}


static void lssi_write_volume_header( LSS_V3 *lss )
{
  int fd, i;
  struct LSSVolumeHeader *vh;
  LSSAccess vha;
  UINT_32 z;
  int tipv = lss->tip_vol - &lss->vol[0];

  fd = lss->tip_vol->filedes;

  memset( &vh, 0, sizeof vh );
  lss->outbuf[0].base_offset = 0;
  lss->outbuf[0].vol_base = MAKE_LOCATOR( tipv, 0 );

  z = lssi_write_alloc( lss, 
			&vha,
			sizeof( struct LSSVolumeHeader ), 
			LSS_MAGIC );
  HEADER_FOR(&vha)->recnum = LSSX_VERSION;

  vh = vha.addr;

  vh->num_vols = lss->num_vols;
  vh->vol_number = tipv;

  vh->vh_generation = 1;
  
  vh->vol_create_time_ms = lssi_time_ms();

  vh->last_cr_at = 0;
  vh->last_cr_generation = 99;  /* first commit will be gen 100 */

  /* write out the zip algorithms */
  
  for (i=1; lss->zip_algs[i]; i++)
    {
      const char *name = lss->zip_algs[i]->name;
#if DEBUG_PRINT
      printf( "zip algorithm[%d] = %s\n", i, name );
#endif /* DEBUG_PRINT */
      vh->zip_alg_at[i-1] = lssi_write_out( lss, name, strlen( name ) + 1,
					    ZIPA_MAGIC );
    }
  for (; i<MAX_ZIP_ALGORITHMS; i++)
    {
      vh->zip_alg_at[i-1] = 0;
    }

  /* write out the volume filenames */
  
  for (i=0; i<lss->num_vols; i++)
    {
      struct LSSVolume *v = &lss->vol[i];
      LSSAccess a;
      UINT_32 n, x;

      n = strlen( v->file );

      x = lssi_write_alloc( lss, &a, n + 1, VOLFN_MAGIC );
      strcpy( a.addr, v->file );
      lssi_write_done_noz( lss, &a, n + 1 );

      vh->vol_info[i].vol_file_at = x;
      vh->vol_info[i].cr_offset = v->cr_offset;
      vh->vol_info[i].vol_serial_num = v->serial_num;
      vh->vol_info[i].spare1 = 0;
      vh->vol_info[i].spare2 = 0;
    }
  lssi_write_done_noz( lss, &vha, sizeof( struct LSSVolumeHeader ) );
  /*lssi_flush_obufs( lss ); */
}


/* 

PrevPrime[i_] := i /; PrimeQ[i]
PrevPrime[i_] := PrevPrime[i-1]

PrevPrime /@ (2^# & /@ Range[3,20])
 */
#define NUM_CAPACITIES  (20-3+1)

/** NOTE:
 *     Because we are using a decent hash function,
 *     these numbers need not really be primes, they
 *     can even be powers of 2.
 */

static UINT_32 cap_table[NUM_CAPACITIES] =
	{ 7, 13, 31, 61, 127, 251, 509, 1021, 2039, 4093, 8191, 16381, 
	  32749, 65521, 131071, 262139, 524287, 1048573 };

LSS *lssv3_create( const char *file, int filemode )
{
  LSS_V3 *lss;

  int fd = open( file, O_RDWR | O_CREAT, filemode );
  if (fd < 0)
    return NULL;

#if DEBUG_PRINT
  printf( "open (for creation) %s\n", file );
#endif

  if (ftruncate( fd, 0 ) < 0)
    {
      close( fd );
      return NULL;
    }
  lss = lssi_new();

  IOTRACE(( lss, "truncate 0 v3create" ));

  lss->num_vols = 1;
  lss->tip_vol = &lss->vol[0];
  lss->vol[0].filedes = fd;
  lss->vol[0].cr_offset = 0;
  lss->vol[0].serial_num = lssi_gen_serno( fd );
  lss->vol[0].file = strdup( file );
  lss->vol[0].auxbuf = NULL;
  lss->vol[0].flags = LSS_RDWR;

  lssi_init_bufs( lss );

  /* write the volume descriptor */

  lssi_write_volume_header( lss );

  /* initialize the in-memory commit record image */

  lss->cr.magic = COMMIT_MAGIC;
  lss->cr.generation = 99; /* first commit will be generation 100 */
  lss->cr.cr_space_w = MAKE_SPACE_WORD( IO_BLOCK_SIZE, 0 );
  lss->cr.cr_length = 0;
  lss->cr.commit_time_ms = 0;
  lss->cr.prev_cr_at = 0;
  lss->cr.self_cr_at = 0;
  lss->cr.mix_cnt = 0;
  lss->cr.mix_offset = 0;  /* causes write at next commit */
  lss->cr.num_diffs = 0;
  lss->cr.vh_fuel = VOLUME_HEADER_FUEL;
  lss->cr.minor_version = LSSV3_MINOR_VERSION; /* latest minor version */

  {
    unsigned i;

    for (i=0; i<NUM_BOOKMARKS; i++)
      lss->cr.bookmarks[i] = 0;
  }

  {
    unsigned i, n;

    n = lss->mindex_cap = cap_table[0];
    lss->mindex = ALLOCN( struct IndexEntry, n );
    for (i=0; i<n; i++)
      {
	lss->mindex[i].mem_ptr = NULL;
	lss->mindex[i].m.line_key = 0;
	lss->mindex[i].m.line_offset = 0;
	lss->mindex[i].m.vol_flags = 0;
      }
  }

  return &lss->com;
}

static UINT_32 next_higher_capacity( UINT_32 old_cap )
{
unsigned i;

    /* find the next higher capacity; Note that if
       the old capacity is taken from a different table,
       this may not increase the capacity by much! (but at least 1)
    */
    for (i=0; i<NUM_CAPACITIES; i++)
    {
	if (cap_table[i] > old_cap)
	{
	    return cap_table[i];
	}
    }
    
    /* make it 1.75 times bigger, and fairly odd */

    return ((old_cap * 3) / 2) | 15;
}

static UINT_32 cap_to_use( UINT_32 cnt )
{
  next_higher_capacity( (cnt * 400) / 256 ); 
}


static IndexEntry *lssi_get_line( LSS_V3 *lss, unsigned h )
{
  IndexEntry *ip = &lss->mindex[h];

  if (ip->mem_ptr)
    {
      return ip;
    }
  else if (ip->m.line_offset)
    {
      struct LSSRecordHeader h;
      struct LSSIndexLine *line;
      int fd, rc;
      unsigned i;
      UINT_32 at = ip->m.line_offset;

      fd = lssi_read( lss, at, &h, sizeof( struct LSSRecordHeader ) );
      if (h.magic != INDEX_MAGIC)
	{
	  lssi_signal_error( &lss->com, LSSERR_BAD_TYPE,
			     "ll", (long)h.magic, (long)INDEX_MAGIC );
	}
      
      ip->mem_ptr = ALLOC( struct LSSIndexEntries );
      rc = read( fd, ip->mem_ptr, sizeof( struct LSSIndexEntries ) );

      if (rc != sizeof( struct LSSIndexEntries ))
	{
	  lssi_signal_error( &lss->com, LSSERR_SHORT_READ,
			     "ll",
			     (long)rc,
			     (long)(sizeof( struct LSSIndexEntries )) );
	}
      if (ip->m.line_key != h.recnum)
	{
	  lssi_signal_error( &lss->com, LSSERR_BAD_TYPE, "" );
	}
      /*
       *  Scan for any record numbers which should be marked "free"...
       */
      for (i=0; i<INDEX_LINE_SIZE; i++) {
        if (ip->mem_ptr->entry[i] == 0) {
          fmapInsert( &lss->freemap, (h.recnum)*INDEX_LINE_SIZE + i );
        }
      }
      return ip;
    }
  else
    {
      return NULL;
    }
}

static void lssi_grow_index( LSS_V3 *lss )
{
  UINT_32 i;
  UINT_32 old_cap = lss->mindex_cap;
  UINT_32 new_cap = next_higher_capacity( old_cap );
  IndexEntry *old_index, *new_index;

  old_index = lss->mindex;
  new_index = ALLOCN( IndexEntry, new_cap );

  for (i=0; i<new_cap; i++)
    {
      new_index[i].mem_ptr = NULL;
      new_index[i].m.line_key = 0;
      new_index[i].m.line_offset = 0;
    }

#if DEBUG_PRINT
  printf( "--- at fill %lu, expanded index capacity from %lu to %lu\n", 
	  lss->mindex_cap, old_cap, new_cap );
#endif /* DEBUG_PRINT */
  for (i=0; i<old_cap; i++)
    {
      /* TODO -- fix this to not read everything in */
      IndexEntry *ip = &old_index[i];

      if (ip->m.line_offset || ip->mem_ptr)
	{
	  unsigned h = lssi_hash( ip->m.line_key ) % new_cap;

	  while (new_index[h].m.line_offset || new_index[h].mem_ptr)
		{
	      h = (h + 1) % new_cap;
	    }
	  new_index[h] = *ip;
	}
    }
  lss->mindex = new_index;
  lss->mindex_cap = new_cap;

  lss->cr.mix_offset = 0;   /* force a write */
  lss->cr.num_diffs = 0;    /* roll in diffs, too */
}


/**
 *   Store a key/value pair into the index.
 *   This may involve writing an entry into the diff list,
 *   and/or allocating a new index line for the index.
 */

/**
 *   Store an entry into the diff list
 */

static void lssi_diff_store( LSS_V3 *lss, UINT_32 key, UINT_32 value )
{
  unsigned i;

  /* store it in the diff list */

  /* don't bother if we are going to
   * write out the whole index anyway
   */

  if (lss->cr.mix_offset == 0)
    return;

  /* if the diff list is full, mark
   * the index as flushable
   */

  if (lss->cr.num_diffs >= MAX_DIFFS)
    {
      /* force a flush of the index to disk */
      lss->cr.mix_offset = 0;
      lss->cr.num_diffs = 0;  /* diffs are rolled in */
#if DEBUG_PRINT
      printf( "      flushing diff list\n" );
#endif /* DEBUG_PRINT */
      return;
    }

  /* if the key is already in the list, just update the value */

  for (i=0; i<lss->cr.num_diffs; i++)
    {
      if (lss->cr.diffs[i].diff_recnum == key)
	{
	  lss->cr.diffs[i].diff_offset = value;
#if DEBUG_PRINT
	  printf( "      already in entry diffs[%u]\n", i );
#endif /* DEBUG_PRINT */
	  return;
	}
    }

  /* store it in the diff list */

  i = lss->cr.num_diffs++;

  lss->cr.diffs[i].diff_recnum = key;
  lss->cr.diffs[i].diff_offset = value;
#if DEBUG_PRINT
  printf( "      entry in diffs[%u]\n", i );
#endif /* DEBUG_PRINT */
}

/*
 *  Recompute the volume flags for a given IndexEntry
 */

static void recompute_volflags( IndexEntry *ip )
{
  UINT_32 f = 0;
  unsigned i;
  UINT_32 *tbl;
  char summary[MAX_VOLUMES];

  assert( ip->mem_ptr );

  tbl = ip->mem_ptr->entry;
  memset( summary, 0, MAX_VOLUMES );

  for (i=0; i<INDEX_LINE_SIZE; i++)
    {
      if (tbl[i])
        {
          summary[ EXTRACT_VOLUME( tbl[i] ) ] = 1;
        }
    }
  for (i=0; i<MAX_VOLUMES; i++)
    {
      if (summary[i])
        {
          f |= VOLFLAG_VOL(i);
        }
    }
  ip->m.vol_flags = f;
}

/*
 *  Find an index line that contains records in one of the given
 *  source volumes.
 *  
 *  works in multiple passes:
 *    1. volflags `clean' and index line present in memory
 *    2. volflags `dirty' and index line in memory (recomputes volflags)
 *    3. volflags `clean'
 *    4. volflags `dirty' (loads line and computes volflags)
 */

static UINT_32 find_source_line( LSS_V3 *lss, UINT_32 vol_mask, int pass )
{
  UINT_32 test = vol_mask | VOLFLAG_DIRTY;
  IndexEntry *ip;
  unsigned i;

  for (i=0, ip=lss->mindex; i<lss->mindex_cap; i++, ip++)
    {
      if (ip->mem_ptr)
        {
          if (ip->m.vol_flags & vol_mask)
            {
              return i;
            }
          else if ((pass >= 2) && (ip->m.vol_flags & VOLFLAG_DIRTY))
            {
              recompute_volflags( ip );
              if (ip->m.vol_flags & vol_mask)
                {
                  return i;
                }
            }
        }
      else if ((ip->m.line_offset) && (pass >= 3))
        {
          if (ip->m.vol_flags & vol_mask)
            {
              return i;
            }
          else if (pass >= 4)
            {
              lssi_get_line( lss, i );
              recompute_volflags( ip );
              if (ip->m.vol_flags & vol_mask)
                {
                  return i;
                }
            }
        }
    }
  return ~(UINT_32)0;
}


/**
 *   Store an entry into the main index
 *   (use lssi_diff_store separately to store diff-list entries,
 *   so this procedure can be used when elaborating the diff list
 *   itself at load time)
 *
 */

static void lssi_index_store( LSS_V3 *lss, UINT_32 key, UINT_32 value )
{
  UINT_32 hash, my_key, in_line;

  /* compute the hash value */

  my_key = key / INDEX_LINE_SIZE;
  in_line = key % INDEX_LINE_SIZE;
  
restart_store:

  hash = lssi_hash( my_key ) % lss->mindex_cap;

#if DEBUG_PRINT
  printf( "STORE key %lu: hash %lu, entry %lu\n", key, hash, in_line );
#endif /* DEBUG_PRINT */

  /* find the index line */
  while (1)
    {
      IndexEntry *ip = lssi_get_line( lss, hash );

      if (!ip)
	{
	  /* allocate a new line */
	  int i;
	      
          if (value == 0) {
            /* it's a deletion... no need to allocate a line */
            break;
          }
            
#if DEBUG_PRINT
	  printf( "hash %lu not in line index...\n", hash );
#endif /* DEBUG_PRINT */
	  if (lss->cr.mix_cnt >= MAX_FILL(lss->mindex_cap))
	    {
	      lssi_grow_index( lss );
	      goto restart_store;
	    }
	      
	  ip = &lss->mindex[hash];
	  ip->mem_ptr = ALLOC( struct LSSIndexEntries );
	  ip->m.line_key = my_key;
	  ip->m.line_offset = 0;
          ip->m.vol_flags = VOLFLAG_DIRTY;

	  for (i=0; i<INDEX_LINE_SIZE; i++)
	    ip->mem_ptr->entry[i] = 0;

	  ip->mem_ptr->entry[in_line] = value;
	  lss->cr.mix_cnt++;
	  lss->cr.mix_offset = 0; /* force a MIDX flush */
	  lss->cr.num_diffs = 0; /* roll in the diffs */

#if DEBUG_PRINT
	  printf( "hash %lu makes %lu lines indexed\n", 
		  hash, lss->cr.mix_cnt );
#endif /* DEBUG_PRINT */
	      
          fmapDelete( &lss->freemap, key );
	  break;
	}
      else if (ip->m.line_key == my_key)
	{
	  ip->mem_ptr->entry[ in_line ] = value;
	  ip->m.line_offset = 0;	  /* mark the line for output */
          ip->m.vol_flags = VOLFLAG_DIRTY;
	  break;
	}
      hash = (hash + 1) % lss->mindex_cap;
    }
  if (value == 0) {
    fmapInsert( &lss->freemap, key );
  } else {
    fmapDelete( &lss->freemap, key );
  }
}

static UINT_32 lssi_index_load( LSS_V3 *lss, UINT_32 key )
{
  UINT_32 hash, my_key, in_line;

  /* compute the hash value */

  my_key = key / INDEX_LINE_SIZE;
  in_line = key % INDEX_LINE_SIZE;
  hash = lssi_hash( my_key ) % lss->mindex_cap;

  /* find the index line */
  while (1)
    {
      IndexEntry *ip = lssi_get_line( lss, hash );

      if (!ip)
	{
#if DEBUG_PRINT
	  printf( "LOAD key %lu: hash %lu, entry %lu: NO LINE\n", 
		  key, hash, in_line );
#endif /* DEBUG_PRINT */
	  return 0;
	}
      else if (ip->m.line_key == my_key)
	{
#if DEBUG_PRINT
	  printf( "LOAD key %lu: hash %lu, entry %lu: %#lx\n", 
		  key, hash, in_line, ip->mem_ptr->entry[ in_line ] );
#endif /* DEBUG_PRINT */
	  return ip->mem_ptr->entry[ in_line ];
	}
      hash = (hash + 1) % lss->mindex_cap;
    }
}

static int lssi_read( LSS_V3 *lss, UINT_32 at, void *ptr, UINT_32 len )
{
  int rc;
  UINT_32 vol, off;
  int fd;

  vol = EXTRACT_VOLUME( at );
  off = EXTRACT_OFFSET( at );
  /* printf( "%#lx ==> v.%lu offset %#lx\n", at, vol, off ); */
  fd = lss->vol[vol].filedes;
  
  rc = lseek( fd, off, SEEK_SET );
  if (rc != off)
    {
      if (rc < 0)
	lssi_sys_error( &lss->com, "" );
      else
	lssi_signal_error( &lss->com, LSSERR_NOT_WHERE_EXPECTED,
			   "ll", (long)rc, (long)off );
    }

  rc = read( fd, ptr, len );
  if (rc != len)
    {
      if (rc < 0)
	lssi_sys_error( &lss->com, "" );
      else
	lssi_signal_error( &lss->com, LSSERR_SHORT_READ,
			   "ll", (long)rc, (long)len );
    }
  return fd;
}

static int lssi_read_chk( LSS_V3 *lss, UINT_32 at, void *ptr, UINT_32 len,
			  UINT_32 type )
{
  int fd = lssi_read( lss, at, ptr, len );
  assert( ((struct LSSRecordHeader *)ptr)->magic == type );
  return fd;
}

static void *lssi_read_ith( LSS_V3 *lss, UINT_32 at, UINT_32 type,
			    struct LSSRecordHeader *h )
{
  void *ptr;
  int rc;
  int fd = lssi_read( lss, at, h, sizeof( struct LSSRecordHeader ) );
  int n;

  assert( h->magic == type );

  /* `h->length' is the uncompressed length -- we want to read
   * the compressed len, which we can only approximate
   */

  n = GET_SPACE_BYTES( h->space_word );

  ptr = malloc( n );
  rc = read( fd, ptr, n );
  if (rc != n)
    {
      if (rc < 0)
	lssi_sys_error( &lss->com, "" );
      else
	lssi_signal_error( &lss->com, LSSERR_SHORT_READ,
			   "ll", (long)rc, (long)n );
    }
  return ptr;
}

static void *lssi_read_it( LSS_V3 *lss, UINT_32 at, UINT_32 type )
{
  struct LSSRecordHeader h;
  return lssi_read_ith( lss, at, type, &h );
}

static int lssi_read_data( LSS_V3 *lss, UINT_32 at, void *ptr, UINT_32 len,
			   UINT_32 type )
{
  struct LSSRecordHeader h;
  int rc;
  int fd = lssi_read( lss, at, &h, sizeof h );

  assert( h.magic == type );
  assert( h.length == len );

  rc = read( fd, ptr, len );
  if (rc != len)
    {
      lssi_signal_error( &lss->com, LSSERR_SHORT_READ,
			 "ll", (long)rc, (long)len );
    }
  return fd;
}


static UINT_32 lssi_find_last_cr( LSS_V3 *lss, int volnum, UINT_32 at )
{
  int rc, fd;
  struct LSSRecordHeader h;
  UINT_32 i;

#if DEBUG_PRINT
  printf( "lssi_find_last_cr( vol[%d], at %#lx )\n", volnum, at );
#endif /* DEBUG_PRINT */

  fd = lss->vol[volnum].filedes;
 
  /* find the last commit record */

  /* check the EOF; if there is an '*EOF' segment there, then that's it */

  rc = lseek( fd, -IO_BLOCK_SIZE, SEEK_END );
  if ((rc & (IO_BLOCK_SIZE-1)) == 0)
    {
      char ioblock[IO_BLOCK_SIZE];
      struct LSSSegmentTrailer *t;
      int nr;

      t = (void *)&ioblock[IO_BLOCK_SIZE - sizeof( struct LSSSegmentTrailer )];

      nr = read( fd, ioblock, IO_BLOCK_SIZE );
      if (nr == IO_BLOCK_SIZE)
	{
	  if (t->magic == EOF_MAGIC)
	    {
	      at = MAKE_LOCATOR( volnum, rc );

#if DEBUG_PRINT
	      printf( "lssi_find_cr: found COMM block at %#lx\n", at );
#endif /* DEBUG_PRINT */
	      goto found_at;
	    }
	  else
	    {
#if DEBUG_PRINT
	      printf( "lssi_find_cr: magic is %08lx, not EOF_MAGIC\n", 
		      t->magic );
#endif /* DEBUG_PRINT */
	    }
	}
      else
	{
#if DEBUG_PRINT
	  printf( "lssi_find_cr: read %d at end -- not an IOBlock\n", nr );
#endif /* DEBUG_PRINT */
	}
    }
  
  /* start at the last known commit record */

#if DEBUG_PRINT
  printf( "lssi_find_cr( %#lx ), %u vols\n", at, lss->num_vols );
#endif /* DEBUG_PRINT */

  if (at == 0)
    {
      /* no guesses */
      return 0;
    }

  if (EXTRACT_VOLUME( at ) != volnum) {
    LSS_PANIC( "CR is not on this volume" );
  }
  assert( EXTRACT_VOLUME( at ) == volnum );
  i = EXTRACT_OFFSET( at );

  while (1)
    {
#if DEBUG_PRINT
      printf( "  checking %#lx\n", i );
#endif /* DEBUG_PRINT */
      rc = lseek( fd, i, SEEK_SET );
      if (rc != i)
	break;
      
      rc = read( fd, &h, sizeof h );
      if (rc != sizeof h)
	break;
      if (h.magic == COMMIT_MAGIC)
	{
	  at = MAKE_LOCATOR( volnum, i );
	}
      i += GET_SPACE_BYTES( h.space_word );
    }

found_at:

  lssi_read_chk( lss, at, &lss->cr, sizeof( struct LSSCommitRecord ),
		 COMMIT_MAGIC );
  return at;
}

static UINT_32 lssi_rewind_to_gen( LSS_V3 *lss, unsigned gen, UINT_32 at )
{
  if (gen == 0)
    return at;

  if ((gen > 0) && (gen <= NUM_BOOKMARKS))
    {
      at = lss->cr.bookmarks[gen-1];
#if DEBUG_PRINT
      printf( "bookmark[%d] at %08x\n", gen-1, at );
#endif      
      lssi_read_chk( lss, at, &lss->cr, sizeof( struct LSSCommitRecord ),
                     COMMIT_MAGIC );
      assert( lss->cr.self_cr_at == at );
      return at;
    }

  while (gen != lss->cr.generation)
    {
      at = lss->cr.prev_cr_at;
      if (!at)
        return 0;  /* XXX the lost generation! */

      lssi_read_chk( lss, at, &lss->cr, sizeof( struct LSSCommitRecord ),
                     COMMIT_MAGIC );
      assert( lss->cr.self_cr_at == at );
    }
  return at;
}

static UINT_32 lssi_find_cr( LSS_V3 *lss, unsigned gen, UINT_32 at )
{
  at = lssi_find_last_cr( lss, lss->num_vols - 1, at ); /* XXY later */
  return lssi_rewind_to_gen( lss, gen, at );
}

int read_mix_with_volflags( LSS_V3 *lss )
{
  UINT_32 i, num, cap;
  struct MasterIndexEntry *mix;
    
  num = lss->cr.mix_cnt;
  cap = cap_to_use( num );
  mix = ALLOCN( struct MasterIndexEntry, num );

  lssi_read_data( lss, lss->cr.mix_offset,
                  mix, sizeof( struct MasterIndexEntry ) * num,
                  MINDEX_MAGIC );

  lss->mindex = ALLOCN( IndexEntry, cap );
  lss->mindex_cap = cap;

  for (i=0; i<cap; i++)
    {
      lss->mindex[i].m.line_key = 0;
      lss->mindex[i].m.line_offset = 0;
      lss->mindex[i].mem_ptr = NULL;
    }

  for (i=0; i<num; i++)
    {
      /* insert into mindex */
      unsigned r, h = lssi_hash( mix[i].line_key ) % cap;

      assert( mix[i].line_offset != 0 );

      while (lss->mindex[h].m.line_offset != 0)
        {
          h = (h + 1) % cap;
        }
      lss->mindex[h].m = mix[i];

      /*
       *  Mark these record numbers as in-use.
       *  Later on we may load this cache line,
       *  in which case we'll re-insert any records
       *  that are actually available.
       */
      r = mix[i].line_key * INDEX_LINE_SIZE;
      fmapDeleteRange( &lss->freemap, r, r + (INDEX_LINE_SIZE-1) );
    }
  return 0;
}

int read_mix_no_volflags( LSS_V3 *lss )
{
  UINT_32 i, num, cap;
  struct MasterIndexEntryNOVF *mix;
    
  num = lss->cr.mix_cnt;
  cap = cap_to_use( num );
  mix = ALLOCN( struct MasterIndexEntryNOVF, num );

  lssi_read_data( lss, lss->cr.mix_offset,
                  mix, sizeof( struct MasterIndexEntryNOVF ) * num,
                  MINDEX_MAGIC );

  lss->mindex = ALLOCN( IndexEntry, cap );
  lss->mindex_cap = cap;

  for (i=0; i<cap; i++)
    {
      lss->mindex[i].m.line_key = 0;
      lss->mindex[i].m.line_offset = 0;
      lss->mindex[i].mem_ptr = NULL;
    }

  for (i=0; i<num; i++)
    {
      /* insert into mindex */
      unsigned h = lssi_hash( mix[i].line_key ) % cap;

      assert( mix[i].line_offset != 0 );

      while (lss->mindex[h].m.line_offset != 0)
        {
          h = (h + 1) % cap;
        }
      lss->mindex[h].m.line_key = mix[i].line_key;
      lss->mindex[h].m.line_offset = mix[i].line_offset;
      lss->mindex[h].m.vol_flags = VOLFLAG_DIRTY;
    }
  return 0;
}

static int link_to_compression_algorithms( LSS_V3 *lss,
                                           struct LSSVolumeHeader *vh )
{
  /* link to compression algorithms used */
  unsigned i;

  for (i=1; i<MAX_ZIP_ALGORITHMS; i++)
    {
      zip_algorithm *use = NULL;

      if (vh->zip_alg_at[i-1])
        {
          char *name;

#if DEBUG_PRINT
          printf( "ZIPA[%d] at %08x", i, vh->zip_alg_at[i-1] );
#endif /* DEBUG_PRINT */

          name = lssi_read_it( lss, vh->zip_alg_at[i-1], ZIPA_MAGIC );
#if DEBUG_PRINT
          printf( "  (\"%s\")\n", name );
#endif /* DEBUG_PRINT */

          use = lss_find_zip_algorithm( name );
          if (!use)
            {
              errno = LSSERR_ZIP_ALGORITHM_NOT_DEF;
              return -1;
            }
          free( name );
          lss->zip_algs[i] = use;
        }
    }
  return 0;
}

static void lssi_load_mindex( LSS_V3 *lss )
{
  unsigned i, n;

  n = lss->cr.num_diffs;

  if (lss->cr.minor_version == LSSV3_NOVF_MINOR_VERSION) {
    read_mix_no_volflags( lss );
    lss->cr.mix_offset = 0;                           /* force a MIDX flush */
    lss->cr.num_diffs = 0;
    lss->cr.minor_version = LSSV3_MINOR_VERSION;      /* update minor ver   */
  } else {
    read_mix_with_volflags( lss );
  }

  for (i=0; i<n; i++) {
    lssi_index_store( lss,
                      lss->cr.diffs[i].diff_recnum,
                      lss->cr.diffs[i].diff_offset );
  }
}

LSS *lssv3_open( const char *file, int fd, int writable, int gen )
{
  LSS_V3 *lss;
  struct LSSVolumeHeader vh;

  assert( (CR_SPACE + sizeof(struct LSSRecordHeader)) <= IO_BLOCK_SIZE );

  if (lssi_read_volume_header( fd, &vh ) < 0)
    {
      close( fd );
      return NULL;
    }

  lss = lssi_new();

  lssi_init_bufs( lss );

  lss->num_vols = vh.num_vols;
  lss->tip_vol = &lss->vol[ lss->num_vols - 1 ];
  lss->tip_base = MAKE_LOCATOR( lss->num_vols - 1, 0 );

  lss->outbuf[0].base_offset = ROUND_UP( lseek( fd, 0, SEEK_END ) );
  lss->outbuf[0].vol_base = lss->tip_base;

  lss->vh_last_cr_at = vh.last_cr_at;
  lss->vh_last_cr_generation = vh.last_cr_generation;

  {
    struct LSSVolume *v = lss->tip_vol;
    unsigned i;
    char *(vol_overrides[MAX_VOLUMES]);

    memset( vol_overrides, 0, sizeof( vol_overrides ) );
    
    v->flags = LSS_RDWR;
    v->filedes = fd;
    v->cr_offset = 0;

    if (strchr( file, ':' ))
      {
        const char *p, *k;
        assert( vh.num_vols <= MAX_VOLUMES );

        p = file;
        for (i=vh.num_vols; i>0;)
          {
            i--;
            if (!*p)
              {
#if DEBUG_PRINT
                printf( "Not enough volumes!\n" );
#endif /* DEBUG_PRINT */
                lssi_signal_error( &lss->com, 9999, "s", file );
              }
            k = strchr( p, ':' );
            if (!k)
              k = p + strlen(p);
            if (k == p)
              {
                vol_overrides[i] = NULL;
              }
            else
              {
                vol_overrides[i] = malloc( k - p + 1 );
                memcpy( vol_overrides[i], p, k-p );
                vol_overrides[i][k-p] = 0;
              }
            p = k+1;
          }
        v->file = vol_overrides[ vh.num_vols-1 ];
      }
    else
      {
        v->file = strdup( file );  /* ignore stored value */
      }
      
    v->serial_num = vh.vol_info[vh.num_vols-1].vol_serial_num;

    /* open older volumes */

    for (i=0; i<vh.num_vols-1; i++)
      {
	int fd;
	char *file;

        if (vol_overrides[i])
          {
            file = vol_overrides[i];
          }
        else
          {
            file = lssi_read_it( lss, 
                                 vh.vol_info[i].vol_file_at, 
                                 VOLFN_MAGIC );
          }

	/*  make sure we remember the ptr in case we bail,
	 *  so we don't leak
	 */
	lss->vol[i].file = file;
        lss->vol[i].auxbuf = NULL;

#if DEBUG_PRINT
	printf( "opening volume %d: %s\n", i, file );
#endif /* DEBUG_PRINT */
	fd = open( file, O_RDONLY );
	if (fd < 0)
	  {
	    lssi_sys_error( &lss->com, "s", file );
	  }

        lss->vol[i].flags = 0;
	lss->vol[i].filedes = fd;
	lss->vol[i].cr_offset = vh.vol_info[i].cr_offset;
	lss->vol[i].serial_num = vh.vol_info[i].vol_serial_num;

	/*  read the volume header to make sure the
	 *  serial numbers match
	 */
	{
	  struct LSSVolumeHeader backing_vh;

	  if (lssi_read_volume_header( fd, &backing_vh ) < 0)
	    {
	      lssi_close( &lss->com );
	      errno = LSSERR_BAD_VOLHDR;
	      return NULL;
	    }
	  /*
	  printf( "%s[%d]: expect: %Lx  got: %Lx\n",
		  file,
		  i,
		  vh.vol_info[i].vol_serial_num,
		  backing_vh.vol_info[i].vol_serial_num );
		  */
	  if (backing_vh.vol_info[i].vol_serial_num 
	      != vh.vol_info[i].vol_serial_num)
	    {
	      lssi_close( &lss->com );
	      errno = LSSERR_VOLSER_MISMATCH;
	      return NULL;
	    }
	}
      }
  }

  if (link_to_compression_algorithms( lss, &vh ) < 0)
    {
      lssi_close( &lss->com );
      return NULL;
    }


  /* find and read the commit record */

  {
    UINT_32 at = lssi_find_cr( lss, gen, vh.last_cr_at );
#if DEBUG_PRINT
    printf( "%#lx: commit record (gen %lu)\n", at, lss->cr.generation );
#endif /* DEBUG_PRINT */

    if (lss->cr.minor_version > (LSSV3_MINOR_VERSION + LSSV3_MINOR_WIGGLE)) {
      /* refuse to read minor versions (too much) newer than we are */
      
      lssi_close( &lss->com );
      errno = LSSERR_BAD_VER;
      return NULL;
    }
    /*  if there were minor versions we cared to read, we could
     *  detect and deal with them here...
     */
  }

  /* read in the master index (apply any diffs from the CR, too) */

  lssi_load_mindex( lss );

  return &lss->com;
}
                                  
LSS *lssv3_extend( const char *file, int filemode,
		   const char *from, int gen )
{
  LSS_V3 *lss;
  int back_fd;
  int fd = open( file, O_RDWR | O_CREAT, filemode );

  if (fd < 0)
    return NULL;

  ftruncate( fd, 0 );

  back_fd = open( from, O_RDONLY );
  if (back_fd < 0)
    {
      close( fd );
      unlink( file );
      return NULL;
    }

  lss = (LSS_V3 *)lssv3_open( from, back_fd, 0, gen );
  if (!lss)
    {
      int e = errno;

      close( fd );
      close( back_fd );
      unlink( file );
      errno = e;

      return NULL;
    }
  IOTRACE(( lss, "truncate %d v3extend", lss->num_vols ));

  /* push a new file onto the stack of volumes */

  lss->vol[ lss->num_vols ].flags = LSS_RDWR;
  lss->vol[ lss->num_vols ].filedes = fd;
  lss->vol[ lss->num_vols ].cr_offset = 0;
  lss->vol[ lss->num_vols ].serial_num = lssi_gen_serno( fd );
  lss->vol[ lss->num_vols ].file = strdup( file );
  lss->vol[ lss->num_vols ].auxbuf = NULL;
  
  lss->tip_vol = &lss->vol[ lss->num_vols ];
  lss->num_vols++;

  lssi_write_volume_header( lss );

  return &lss->com;
}


static void lssi_free_mindex( LSS_V3 *lss )
{
  unsigned i;

  for (i=0; i<lss->mindex_cap; i++)
    {
      if (lss->mindex[i].mem_ptr)
	free( lss->mindex[i].mem_ptr );
    }
  free( lss->mindex );
  lss->mindex = NULL;
}


static void lssi_close( LSS *generic_ptr )
{
  int i;
  LSS_V3 *lss = (LSS_V3 *)generic_ptr;

  for (i=0; i<lss->num_vols; i++)
    {
      if (lss->vol[i].file)
	free( lss->vol[i].file );

      close( lss->vol[i].filedes );
    }
  
  for (i=0; i<lss->num_outbufs; i++)
    {
      free( lss->outbuf[i].base );
    }
  free( lss->trailing_buf );
  lssi_free_mindex( lss );
  free( lss );
}

static void lssi_write_done_z( LSS_V3 *lss,
			       LSSAccess *a, 
			       UINT_32 z_length, UINT_32 unz_len,
			       int z_alg )
{
  struct LSSRecordHeader *h = (void *)((UINT_8 *)a->addr - HEADER_SIZE);
  struct IOBuf *in = ACCESS_IN(a);
  UINT_32 used_space = ROUND_UP( z_length + HEADER_SIZE );

  assert( in );

  if (in->last_access == a)
    {
      in->ptr = (UINT_8 *)a->addr - HEADER_SIZE + used_space;
      in->last_access = NULL;
    }

#if DEBUG_PRINT
  printf( "%s[%#lx]: done with write: %lu bytes (%lu unz) (space=%lu) @ %p\n",
	  lss->tip_vol->file, 
	  in->base_offset + ((UINT_8 *)a->addr - in->base) - HEADER_SIZE,
	  z_length, unz_len, used_space,
	  a->addr );
#endif /* DEBUG_PRINT */

  lss->cr.vh_fuel -= BLOCK_FUEL_COST;
  h->length = unz_len;
  h->space_word = MAKE_SPACE_WORD( used_space, z_alg );
  in->num_accesses--;
  ACCESS_IN(a) = NULL;
}


static UINT_32 write_alloc_inbuf( struct IOBuf *obuf,
                                  LSSAccess *a,
                                  UINT_32 real_space,
                                  UINT_32 type )
{
  struct LSSRecordHeader *h = (void *)obuf->ptr;
  UINT_32 off;

  /* more for debugging than anything, 
	 clear out the space being reserved */
      
  memset( obuf->ptr, 0, real_space );
      
  h->magic = type;
  h->space_word = MAKE_SPACE_WORD( real_space, 0 ); /*uncompressed so far*/
  h->recnum = 0;
  h->length = 0;

  off = obuf->base_offset + (obuf->ptr - obuf->base);
  ACCESS_IN(a) = obuf;
  a->addr = obuf->ptr + HEADER_SIZE;
  a->bytes = real_space - HEADER_SIZE;
  obuf->ptr += real_space;
  obuf->num_accesses++;
  obuf->last_access = a;
#if DEBUG_PRINT
  printf( "write alloc %lu bytes: @ %p\n", real_space, a->addr );
#endif /* DEBUG_PRINT */
  return MAKE_BASED_LOCATOR( obuf->vol_base, off );
}

static UINT_32 lssi_write_alloc_aux( LSS_V3 *lss, LSSAccess *a, UINT_32 space, 
                                     UINT_32 type, int vol )
{
  UINT_32 real_space = ROUND_UP( space + HEADER_SIZE );
  struct IOBuf *obuf = lss->vol[vol].auxbuf;
  UINT_8 *nxt_ptr;

  assert( obuf );

  nxt_ptr = obuf->ptr + real_space;
  if (nxt_ptr > obuf->limit)
    {
      lssi_flush_obufsv( lss, &lss->vol[vol], obuf, 1 );
      /*
       *  we are not set up to deal with an auxillary write being
       *  bigger than we can handle in a single output buffer
       */
      assert( (obuf->ptr + real_space) <= obuf->limit );
    }
  return write_alloc_inbuf( obuf, a, real_space, type );
}

static UINT_32 lssi_write_alloc( LSS_V3 *lss, LSSAccess *a, UINT_32 space, 
				 UINT_32 type )
{
  struct IOBuf *obuf = &lss->outbuf[ lss->num_outbufs - 1 ];
  UINT_32 real_space = ROUND_UP( space + HEADER_SIZE );
  UINT_8 *nxt_ptr = obuf->ptr + real_space;

  if (nxt_ptr > obuf->limit)
    {
      if (lss->num_outbufs < MAX_OUT_BUFS)
	{
	  struct IOBuf *no = &lss->outbuf[ lss->num_outbufs++ ];
	  UINT_32 w;

	  if (real_space < MIN_OBUF_SIZE)
	    w = MIN_OBUF_SIZE;
	  else
	    w = real_space;
	  
#if DEBUG_PRINT
	  printf( "adding new obuf -- %lu bytes\n", w );
#endif /* DEBUG_PRINT */
	  no->base = malloc( w );
	  no->ptr = no->base;
	  no->limit = no->base + w;
	  no->num_accesses = 0;
	  no->base_offset = obuf->base_offset + (obuf->ptr - obuf->base);
          no->vol_base = obuf->vol_base;
	  no->last_access = NULL;
	}
      else
	{
	  lssi_flush_obufs( lss );
	}
      return lssi_write_alloc( lss, a, space, type );
    }
  else
    {
      return write_alloc_inbuf( obuf, a, real_space, type );
    }
}

static int set_tip( LSS_V3 *lss, unsigned vol )
{
  UINT_32 tb;

  assert( vol < lss->num_vols );

  tb = MAKE_LOCATOR( vol, 0 );
  if (lss->tip_vol)
    {
      int rc;

      if (lss->tip_base == tb)
        {
          /* nothing to do... already set to this vol */
          return 0;
        }
      /* XXX<TODO> check for writable volume, signal error if not */
      lssi_flush_obufs( lss );
      if (lss->do_fsync) {
        rc = fsync( lss->tip_vol->filedes );
        if (rc < 0)
          {
            lssi_sys_error( &lss->com, "" );
          }
      }
    }

  assert( lss->vol[vol].filedes >= 0 );

  lss->tip_base = tb;
  lss->tip_vol = &lss->vol[vol];

  /*
   *  make sure the volume we're switching to doesn't have an
   *  active output (auxillary) buffer
   */
  if (lss->tip_vol->auxbuf)
    {
      lssi_flush_auxbufs( lss );
      free( lss->tip_vol->auxbuf );
      lss->tip_vol->auxbuf = NULL;
    }

  lss->outbuf[0].base_offset = ROUND_UP( lseek( lss->tip_vol->filedes, 0, SEEK_END ) );
  lss->outbuf[0].vol_base = tb;

  return 0;
}

static LSSAccess *lssi_read_access_inbuf( LSS_V3 *lss,
                                          struct IOBuf *buf,
                                          LSSAccess *a,
                                          UINT_32 offset_in_vol )
{
  struct LSSRecordHeader *hp;
  int alg;

  buf->last_access = a;
  buf->num_accesses++;

  hp = (void *)(buf->base + (offset_in_vol - buf->base_offset));
  a->addr = (void *)(hp + 1);
  a->bytes = hp->length;
  a->space = GET_SPACE_BYTES( hp->space_word );
  ACCESS_IN(a) = buf;

  alg = GET_WHICH_ZIP( hp->space_word );
  a->uses = lss->zip_algs[ alg ];
  if (!a->uses)
    {
      lssi_signal_error( &lss->com, LSSERR_INVALID_ZIP_ALGORITHM,
                         "l", (long)alg );
    }
  return a;
}

static LSSAccess *lssi_read_access_at( LSS_V3 *lss, 
                                       UINT_32 recnum,
                                       UINT_32 at )
{
  LSSAccess *a;
  struct LSSRecordHeader h;
  int alg;
  int v = EXTRACT_VOLUME( at );
  UINT_32 o = EXTRACT_OFFSET( at );
  
#if DEBUG_PRINT
  printf( "rec %lu ==> at: %#lx\n", recnum, at );
#endif /* DEBUG_PRINT */
  if (!at) {
    return NULL;
  }

  a = ALLOC( LSSAccess ); /* okay; expensive for now! */

  /*
   *  see if the data is currently in the auxbuf or obuf
   */
  if (&lss->vol[v] == lss->tip_vol)
    {
      /* check the obufs */
      struct IOBuf *b = &lss->outbuf[0];

      if (o >= b->base_offset)
        {
          int i;

          /*
           *  it should be in here somewhere...
           */
          for (i=0; i<lss->num_outbufs; i++, b++)
            {
              if (o < (b->base_offset + (b->ptr - b->base)))
                {
                  return lssi_read_access_inbuf( lss, b, a, o );
                }
            }
          assert( 0 ); /* should have found it! */
        }
    }
  else if (lss->vol[v].auxbuf)
    {
      struct IOBuf *b = lss->vol[v].auxbuf;

      /* check the auxbuf */
      if (o >= b->base_offset)
        {
          /* given our "write-extend-only" model, if the target
           * is after the base_offset, then either the data is
           * in this buf, or its nowhere
           */
          return lssi_read_access_inbuf( lss, b, a, o );
        }
    }

      
  /*  TODO -- please support reading back a record before 
   *  it has been flushed to disk!
   */
  a->addr = lssi_read_ith( lss, at, DATAREC_MAGIC, &h );
  a->bytes = h.length;
  a->space = GET_SPACE_BYTES( h.space_word );

  /*
    printf( "space word = %#08x, %lu unz\n", h.space_word, h.length );
    print_hex_data( a->addr, GET_SPACE_BYTES( h.space_word ) );
    printf( "\n" );
  */

  alg = GET_WHICH_ZIP( h.space_word );
  a->uses = lss->zip_algs[ alg ];
  if (!a->uses)
    {
      lssi_signal_error( &lss->com, LSSERR_INVALID_ZIP_ALGORITHM,
                         "l", (long)alg );
    }
#if DEBUG_PRINT
  printf( "using zip[%d] = %s\n", alg, a->uses->name );
#endif

  ACCESS_IN(a) = NULL;
  return a;
}

static LSSAccess *lssi_read_access( LSS *generic_ptr, UINT_32 recnum )
{
  LSS_V3 *lss = (LSS_V3 *)generic_ptr;
  UINT_32 at;

  at = lssi_index_load( lss, recnum );
  return lssi_read_access_at( lss, recnum, at );
}

static void lssi_read_release( LSS *generic_ptr, LSSAccess *a )
{
  /* LSS_V3 *lss = (LSS_V3 *)generic_ptr; */

  if (a)
    {
      struct IOBuf *in = ACCESS_IN(a);

      if (in)
        {
          assert( in->last_access == a );
          in->num_accesses--;
          in->last_access = NULL;
        }
      else if (a->bytes)
	{
	  assert( a->addr );
	  free( a->addr );
	}
      a->addr = NULL;
      free( a );
    }
}

static size_t lssi_copy_record( LSS *generic_ptr,
			        LSS *dest,
			        UINT_32 recnum )
{
  LSSAccess *a = lssi_read_access( generic_ptr, recnum );

  if (a)
    {
      size_t n = a->space;

      dest->fn->raw_write_meth( dest, recnum, a->addr, a->bytes, a->uses, n );
      lssi_read_release( generic_ptr, a );

      return n;
    }
  else
   {
     return 0;
   }
}

static unsigned count_used( IndexEntry *ip )
{
  unsigned i, n = 0;

  if (ip)
    {
      for (i=0; i<INDEX_LINE_SIZE; i++)
	{
	  if (ip->mem_ptr->entry[i])
	    n++;
	}
    }
  return n;
}

static UINT_32 *lssi_get_index( LSS *gp, UINT_32 *cnt )
{
  LSS_V3 *lss = (LSS_V3 *)gp;

  UINT_32 *ix;
  UINT_32 i, j, k, cap, n = 0;

  cap = lss->mindex_cap;

  for (i=0; i<cap; i++)
    {
      n += count_used( lssi_get_line( lss, i ) );
    }

  ix = ALLOCN( UINT_32, n );
  j = 0;

#if DEBUG_PRINT
  printf( "get-index: %lu entries\n", n );
#endif
  for (i=0; i<cap; i++)
    {
      IndexEntry *ip = lssi_get_line( lss, i );
      if (ip)
	{
	  /* first record number in index line */
	  UINT_32 frn = ip->m.line_key * INDEX_LINE_SIZE;

	  for (k=0; k<INDEX_LINE_SIZE; k++)
	    {
	      if (ip->mem_ptr->entry[k])
		{
		  ix[j++] = k + frn;
		}
	    }
	}
    }
  assert( j == n );
  *cnt = n;
  return ix;
}

static void lssi_get_info( LSS *gp, UINT_32 rec, struct LSSRecordInfo *info )
{
  LSS_V3 *lss = (LSS_V3 *)gp;
  UINT_32 at = lssi_index_load( lss, rec );

  info->record_num = rec;
  info->volume = EXTRACT_VOLUME(at);
  info->offset = EXTRACT_OFFSET(at);
}


static void lssi_readv( LSS *generic_ptr,
			zipbuf *dest,
			LSSAccess *a )
{
  /* LSS_V3 *lss = (LSS_V3 *)generic_ptr; */
  void *end_ptr;

  if (!a)
    return; /* nothing to do */

#if DEBUG_PRINT
  printf( " readv using %s\n", a->uses->name );
#endif

  end_ptr = a->uses->unzip_meth( a->uses, dest, a->addr );
  /*  should check something about the end_ptr, to make
   *  we ate exactly the input (as near as we can tell,
   *  because the compressed size was rounded off to the
   *  granularity)
   */
#if DEBUG_PRINT
  printf( " --> ate %u bytes\n", 
	  (unsigned)((char *)end_ptr - (char *)a->addr) );
#endif
}

static int get_zip_id( LSS *generic_ptr, zip_algorithm *use )
{
  LSS_V3 *lss = (LSS_V3 *)generic_ptr;
  if (use)
    {
      int zip_id;

      for (zip_id=0; lss->zip_algs[zip_id]; zip_id++)
	{
	  if (lss->zip_algs[zip_id] == use)
	    return zip_id;
	}
      if (zip_id >= MAX_ZIP_ALGORITHMS)
	{
	  lssi_signal_error( (LSS *)lss, 
			     LSSERR_TOO_MANY_ALGORITHMS, 
			     "s", use->name );
	}
      lss->zip_algs[zip_id] = use;
      lss->zip_algs[zip_id+1] = NULL; /* reinstall NULL end-marker */
      /* force a flush of the volume header */
      lss->cr.vh_fuel = -1;
      return zip_id;
    }
  else
    {
      use = &lss_null_zip;
      return 0;
    }
}

static void lssi_writev( LSS *generic_ptr, 
			 UINT_32 recnum,
			 zipbuf *datav,
			 zip_algorithm *use )
{
  LSS_V3 *lss = (LSS_V3 *)generic_ptr;
  int zip_id = get_zip_id( generic_ptr, use );
  struct LSSRecordHeader *h;
  UINT_32 at;
  size_t unz_len;
  LSSAccess a;
  void *end_ptr;

  /* allocate the record space */

  unz_len = zipbuf_len( datav );
  at = lssi_write_alloc( lss, &a, MAX_ZLEN(unz_len), DATAREC_MAGIC );
  h = HEADER_FOR(&a);
  h->recnum = recnum;
  lssi_index_store( lss, recnum, at );
  lssi_diff_store( lss, recnum, at );
  
  /* compress into the buffer */

  if (use)
    end_ptr = use->zip_meth( use, a.addr, datav );
  else
    end_ptr = lss_null_zip.zip_meth( &lss_null_zip, a.addr, datav );

#if DEBUG_PRINT
  printf( " --> wrote %u bytes\n", 
	  (unsigned)((char *)end_ptr - (char *)a.addr) );
#endif
  /* close it off */

  lssi_write_done_z( lss, &a, 
		     (char *)end_ptr - (char *)a.addr, 
		     unz_len,
		     zip_id );
}

static void lssi_raw_write( LSS *generic_ptr,
			    UINT_32 recnum,
			    void *data,
			    size_t unz_len,
			    zip_algorithm *used,
			    size_t z_len )
{
  LSS_V3 *lss = (LSS_V3 *)generic_ptr;
  int zip_id = get_zip_id( generic_ptr, used );
  struct LSSRecordHeader *h;
  UINT_32 at;
  LSSAccess a;
  void *end_ptr;

  /* allocate the record space */

  at = lssi_write_alloc( lss, &a, z_len, DATAREC_MAGIC );
  h = HEADER_FOR(&a);
  h->recnum = recnum;
  lssi_index_store( lss, recnum, at );
  lssi_diff_store( lss, recnum, at );

  /* compress into the buffer */

  memcpy( a.addr, data, z_len );

#if DEBUG_PRINT
  printf( " --> wrote %u bytes\n", 
	  (unsigned)((char *)end_ptr - (char *)a.addr) );
#endif
  /* close it off */

  lssi_write_done_z( lss, &a, z_len, unz_len, zip_id );
}

/* (vol_num == -1) => latest volume */

static const char *lssi_filename( LSS *generic_ptr, int vol_num )
{
  LSS_V3 *lss = (LSS_V3 *)generic_ptr;

  if (vol_num < 0)
    /* -1 => last, -2 => next to last, etc */
    vol_num = lss->num_vols + vol_num;

  if ((vol_num < 0) || (vol_num >= lss->num_vols))
    return NULL;
  else
    return lss->vol[vol_num].file;
}

static int lssi_delete( LSS *generic_ptr, unsigned rec )
{
  LSS_V3 *lss = (LSS_V3 *)generic_ptr;

  lssi_index_store( lss, rec, 0 );
  lssi_diff_store( lss, rec, 0 );
  return 0;
}

static int lssi_alloc_recs( LSS *generic_ptr, unsigned minr, unsigned cnt,
                            UINT_32 *rec )
{
  LSS_V3 *lss = (LSS_V3 *)generic_ptr;
  unsigned i, n, k;

  /*fmapPrint( &lss->freemap );*/

  /*
   *  Make sure our free map is accurate
   */
  n = lss->mindex_cap;
  for (i=0; i<n; i++) {
    if ((lss->mindex[i].m.vol_flags & VOLFLAG_HAS_FREE)
        && (lss->mindex[i].m.line_key >= (minr / INDEX_LINE_SIZE))
        && (lss->mindex[i].m.line_offset)
        && (!lss->mindex[i].mem_ptr)) {
      lssi_get_line( lss, i );
    }
  }

  /*fmapPrint( &lss->freemap );*/

  if (fmapFindAndDeleteRange( &lss->freemap, minr, cnt, &k ) == 0) {
    unsigned i;

    *rec = k;
    for (i=0; i<cnt; i++) {
      assert( lssi_index_load( lss, k+i ) == 0 );
    }
    return 0;
  } else {
    return -ENOENT;
  }
}

static UINT_32 lssi_find_vrecord( LSS *generic_ptr, unsigned mask, int pass )
{
  LSS_V3 *lss = (LSS_V3 *)generic_ptr;
  IndexEntry *ip;
  unsigned i;

  if (mask == 0)
    return ~(UINT_32)0;

 again:
  if (!((mask == lss->current_findr.mask)
        && (pass == lss->current_findr.pass)
        && (lss->current_findr.index < lss->mindex_cap)
        && (lss->mindex[lss->current_findr.index].mem_ptr)))
    {
      /*
       *  cache is invalid -- find another line
       */
      i = find_source_line( lss, mask, pass );
      /*printf( "find_source_line( %08x pass %d ) => %d\n", mask, pass, i );*/
      if (i == ~(UINT_32)0)
        return i;
      lss->current_findr.mask = mask;
      lss->current_findr.pass = pass;
      lss->current_findr.index = i;
    }
  
  ip = &lss->mindex[lss->current_findr.index];
  /* search in this index list */
  for (i=0; i<INDEX_LINE_SIZE; i++)
    {
      if (ip->mem_ptr->entry[i] &&
          (mask & VOLFLAG_VOL( EXTRACT_VOLUME( ip->mem_ptr->entry[i] ) )))
        {
          /*
           * found a record on a target volume
           */
          return (ip->m.line_key * INDEX_LINE_SIZE) + i;
        }
    }
  lss->current_findr.mask = 0;
  goto again;
}

static struct LSSMethods lss_v3 = {
  "V3",
  lssi_close,
  lssi_commit,
  lssi_read_access,
  lssi_readv,
  lssi_read_release,
  lssi_writev,
  lssi_get_index,
  lssi_get_info,
  lssi_copy_record,
  lssi_raw_write,
  lssi_filename,
  lssi_find_vrecord,
  lssi_alloc_recs,
  lssi_delete
};



static void init_xvol( LSS_V3 *lss,
                       int volnum, int fd, const char *file )
{
  struct LSSVolumeHeader vh;
  struct LSSVolume *vp = &lss->vol[volnum];

  vp->flags = LSS_RDWR;
  vp->filedes = fd;
  vp->file = strdup( file );
  vp->auxbuf = NULL;

  if (fd >= 0) 
    {
      if (lssi_read_volume_header( fd, &vh ) < 0)
        {
#if DEBUG_PRINT
          printf( "xvol[%d]: volume header read failed\n", volnum );
#endif /* DEBUG_PRINT */
          close( fd );
          vp->filedes = -1;
        }
      else
        {
          UINT_32 x;

          vp->cr_offset = vh.last_cr_at;
          vp->serial_num = vh.vol_info[volnum].vol_serial_num;

#if DEBUG_PRINT
          printf( "xvol[%d]: VH last cr at %x\n", volnum, vp->cr_offset );
#endif /* DEBUG_PRINT */

          x = lssi_find_last_cr( lss, volnum, vp->cr_offset );
          if (EXTRACT_VOLUME( x ) == volnum)
            {
              vp->cr_offset = EXTRACT_OFFSET( x );
            }
          else
            {
              vp->cr_offset = 0;
            }
#if DEBUG_PRINT
          printf( "xvol[%d]: last cr at %x\n", volnum, vp->cr_offset );
#endif /* DEBUG_PRINT */

          link_to_compression_algorithms( lss, &vh );
        }
    }
}

static UINT_32 find_most_favored_cr( LSS_V3 *lss, int flags );

LSS *lss_openx( const char **filev, unsigned gen, int flags )
{
  int i, N;
  int fdv[MAX_VOLUMES];
  LSS_V3 *lss;

  for (N=0; filev[N]; N++)
    {
      fdv[N] = open( filev[N], O_RDWR );
      if (fdv[N] < 0)
        {
          perror( filev[N] );
        }
    }

  lss = lssi_new();
  lssi_init_bufs( lss );

  lss->num_vols = N;
  /*
   *  Initialize each volume
   */
  for (i=0; i<N; i++)
    {
      init_xvol( lss, i, fdv[i], filev[i] );
    }

  find_most_favored_cr( lss, flags ); /* and initialize tip_vol */

  /*
   *  if we are looking for other than the most recent CR,
   *  go looking for it
   */
  if (gen)
    {
      UINT_32 at;

      at = lssi_rewind_to_gen( lss, gen, lss->cr.self_cr_at );
      set_tip( lss, EXTRACT_VOLUME( at ) );
    }
#if DEBUG_PRINT
  printf( "** using CR at %08lx (gen %lu => %lu)\n", 
          lss->cr.self_cr_at, 
          gen, 
          lss->cr.generation );
#endif /* DEBUG_PRINT */

  lss->outbuf[0].base_offset = ROUND_UP( lseek( lss->tip_vol->filedes, 0, SEEK_END ) );
  lss->outbuf[0].vol_base = MAKE_LOCATOR( (lss->tip_vol - &lss->vol[0]), 0 );
  
  lss->com.writable = 1;
  return (LSS *)lss;
}

static UINT_32 find_most_favored_cr( LSS_V3 *lss, int flags )
{
  UINT_32 best_gen, best_at;
  unsigned i, n;

  best_at = 0;

  for (i=0; i<lss->num_vols; i++)
    {
      if (lss->vol[i].filedes >= 0)
        {
          UINT_32 at;

          if (lss->vol[i].cr_offset)
            {
              struct LSSCommitRecord tmp;

              at = MAKE_LOCATOR( i, lss->vol[i].cr_offset );
#if DEBUG_PRINT
              printf( "xvol[%d] CR at %08x\n", i, at );
#endif /* DEBUG_PRINT */
              lssi_read_chk( lss, at, &tmp,
                             sizeof( struct LSSCommitRecord ),
                             COMMIT_MAGIC );
              if ((best_at == 0) || (tmp.generation > best_gen))
                {
                  best_gen = tmp.generation;
                  best_at = at;
                }
            }
        }
    }
  if (best_at)
    {
#if DEBUG_PRINT
      printf( "** best CR at %08lx (gen %lu)\n", best_at, best_gen );
#endif /* DEBUG_PRINT */
      lss->vh_last_cr_at = best_at;
      lss->vh_last_cr_generation = best_gen;

      lssi_read_chk( lss, best_at, &lss->cr,
                     sizeof( struct LSSCommitRecord ),
                     COMMIT_MAGIC );

      lssi_load_mindex( lss );

      set_tip( lss, EXTRACT_VOLUME( best_at ) );
    }
  else
    {
#if DEBUG_PRINT
      printf( "** no CRs found **\n" );
#endif /* DEBUG_PRINT */

      /* XXX ; if (!(mode & CREATE)) signal_error */

      for (i=0; i<lss->num_vols; i++)
        {
          /*
           *  init the filesystem file
           */
          if (lss->vol[i].filedes >= 0)
            {
              ftruncate( lss->vol[i].filedes, 0 );
              IOTRACE(( lss, "truncate %d nocr", i ));
            }
          else
            {
              int fd;

#if DEBUG_PRINT
              printf( "** create xvol[%d] '%s'\n", i, lss->vol[i].file );
#endif /* DEBUG_PRINT */
              fd = open( lss->vol[i].file, O_RDWR|O_CREAT, flags & 0777 );
              if (fd < 0)
                {
                  perror( lss->vol[i].file );
                }
              lss->vol[i].filedes = fd;
              lss->vol[i].flags = LSS_RDWR;
              IOTRACE(( lss, "create %d", i ));
            }
          /*
           *  populate it with some baseline data
           */
          set_tip( lss, i );
          lssi_write_volume_header( lss );
        }
      set_tip( lss, 0 );

      /*
       *  write out the initial commit record
       */
      lss->cr.magic = COMMIT_MAGIC;
      lss->cr.generation = 99; /* first commit will be generation 100 */
      lss->cr.cr_space_w = MAKE_SPACE_WORD( IO_BLOCK_SIZE, 0 );
      lss->cr.cr_length = 0;
      lss->cr.commit_time_ms = 0;
      lss->cr.prev_cr_at = 0;
      lss->cr.self_cr_at = 0;
      lss->cr.mix_cnt = 0;
      lss->cr.mix_offset = 0;  /* causes write at next commit */
      lss->cr.num_diffs = 0;
      lss->cr.vh_fuel = VOLUME_HEADER_FUEL;
      lss->cr.minor_version = LSSV3_MINOR_VERSION; /* latest minor version */

      for (i=0; i<NUM_BOOKMARKS; i++)
        lss->cr.bookmarks[i] = 0;

      n = lss->mindex_cap = cap_table[0];
      lss->mindex = ALLOCN( struct IndexEntry, n );
      for (i=0; i<n; i++)
        {
          lss->mindex[i].mem_ptr = NULL;
          lss->mindex[i].m.line_key = 0;
          lss->mindex[i].m.line_offset = 0;
          lss->mindex[i].m.vol_flags = 0;
        }
    }
  return best_at;
}

int lss_set_tip( LSS *generic_ptr, int vol )
{
  assert( generic_ptr->fn == &lss_v3 );
  return set_tip( (LSS_V3 *)generic_ptr, vol );
}

int lss_get_tip( LSS *generic_ptr )
{
  LSS_V3 *lss = (LSS_V3 *)generic_ptr;
  assert( generic_ptr->fn == &lss_v3 );
  return EXTRACT_VOLUME( lss->tip_base );
}

int lss_attach_vol( LSS *generic_ptr, int i, const char *file )
{
  LSS_V3 *lss = (LSS_V3 *)generic_ptr;
  int fd;

  assert( generic_ptr->fn == &lss_v3 );

  /* XXX<TODO> support attaching a totally new (extension) volume */

  if (file)
    {
      if (lss->vol[i].file)
        free( lss->vol[i].file );
      lss->vol[i].file = strdup( file );
    }

  fd = open( lss->vol[i].file, O_RDWR|O_CREAT, 0666 );
  if (fd < 0)
    {
      return -1;
    }
  lss->vol[i].filedes = fd;
  lss->vol[i].flags = LSS_RDWR;
  IOTRACE(( lss, "attach %d %d", i, lseek( fd, 0, SEEK_END ) ));

  set_tip( lss, i );
  lssi_write_volume_header( lss );

  return 0;
}

int detach_cr( LSS_V3 *lss, int det_vol, UINT_32 *cr )
{
  if (*cr) {
    if (EXTRACT_VOLUME( *cr ) == det_vol) {
      LSS_PANIC( "commit record on detached volume" );
    }
  }
  return 0;
}

int lss_detach_vol( LSS *generic_ptr, int vol )
{
  LSS_V3 *lss = (LSS_V3 *)generic_ptr;
  int i;

  assert( generic_ptr->fn == &lss_v3 );

  assert( vol >= 0 );
  assert( vol < lss->num_vols );

  /*
   *  the caller is obliged to ensure that no records are still on
   *  the detached volume, and that it is not the current tip
   */

  assert( find_source_line( lss, 1<<vol, 4 ) == ~(UINT_32)0 );
  assert( lss->tip_vol != &lss->vol[vol] );

  /*
   *  However, we will make sure to copy off important indexing
   *  structures
   */
  
  lss->cr.mix_offset = 0;
  for (i=0; i<lss->mindex_cap; i++)
    {
      if (EXTRACT_VOLUME( lss->mindex[i].m.line_offset ) == vol)
        {
          /*
           *  make sure its in memory, because we are going to 
           *  disable access to the old location
           */
          lssi_get_line( lss, i );
          lss->mindex[i].m.line_offset = 0;
        }
    }

  /*
   *  We need to do likewise for all the bookmarked commit
   *  records
   */
  detach_cr( lss, vol, &lss->cr.self_cr_at );
  /*
   *  Force an update of all the volume headers, so that
   *  lss->vh_last_cr_at reflects the latest CR location
   *  instead of possibly something on another volume...
   */ 
  lssi_update_volume_headers( lss );

  for (i=0; i<NUM_BOOKMARKS; i++)
    {
      detach_cr( lss, vol, &lss->cr.bookmarks[i] );
    }

  lss->cr.vh_fuel = 0;          /* force flush of volume header(s) */
  lssi_commit( generic_ptr, 0 );

  if (lss->vol[vol].filedes >= 0)
    {
      IOTRACE(( lss, "detach %d %d", vol, lseek( lss->vol[vol].filedes, 0, SEEK_END ) ));

      close( lss->vol[vol].filedes );
      lss->vol[vol].filedes = -1;
    }
  else
    {
      IOTRACE(( lss, "detach %d", vol ));
    }
  lss->vol[vol].cr_offset = 0;

  return 0;
}

size_t lss_move_record( LSS *generic_ptr,
                        int destvol,
                        UINT_32 recnum )
{
  LSS_V3 *lss = (LSS_V3 *)generic_ptr;
  LSSAccess *a;
  int i;
  size_t n;
  UINT_32 at;

  assert( generic_ptr->fn == &lss_v3 );

  at = lssi_index_load( lss, recnum );

  if (!at || (EXTRACT_VOLUME( at ) == destvol)) {
    return 0;
  }

  a = lssi_read_access_at( lss, recnum, at );

  if (!a)
    return 0;

  n = a->space;
  if (&lss->vol[destvol] == lss->tip_vol)
    {
      lssi_raw_write( generic_ptr, recnum, a->addr, a->bytes, a->uses, n );
    }
  else
    {
      int zip_id = get_zip_id( generic_ptr, a->uses );
      struct LSSRecordHeader *h;
      UINT_32 at;
      LSSAccess wa;
      void *end_ptr;

      /*
       * writing to a non-tip volume; use an auxillary output buffer
       */
      if (!lss->vol[destvol].auxbuf)
        {
          lssi_alloc_auxbuf( lss, destvol );
        }

      /* allocate the record space */
      
      at = lssi_write_alloc_aux( lss, &wa, n, DATAREC_MAGIC, destvol );
      h = HEADER_FOR(&wa);
      h->recnum = recnum;
      lssi_index_store( lss, recnum, at );
      lssi_diff_store( lss, recnum, at );
      
      memcpy( wa.addr, a->addr, n );

      lssi_write_done_z( lss, &wa, n, a->bytes, zip_id );
    }
  lssi_read_release( generic_ptr, a );
  return n;
}

int lss_set_generation( LSS *generic_ptr, unsigned gen )
{
  LSS_V3 *lss = (LSS_V3 *)generic_ptr;
  UINT_32 at;

  if (gen == lss->cr.generation) {
    /*  reload the CR, in case we clobbered some parts of it
     *  (e.g., we clear out mix_offset to force a master flush)
     *
     *  This isn't a problem if we actually rewind any, because
     *  all we need to be valid for the rewind to function is
     *  lss->cr.generation and lss->cr.prev_cr_at.
     */
    at = lss->cr.self_cr_at;
    lssi_read_chk( lss, at, &lss->cr, sizeof( struct LSSCommitRecord ),
                   COMMIT_MAGIC );
  } else {
    at = lssi_rewind_to_gen( lss, gen, lss->cr.self_cr_at );
    if (!at) {
      return -1;
    }
  }
  assert( lss->cr.self_cr_at == at );
  set_tip( lss, EXTRACT_VOLUME( at ) );

  lss->outbuf[0].base_offset = ROUND_UP( lseek( lss->tip_vol->filedes, 
                                                0, 
                                                SEEK_END ) );
  lss->outbuf[0].vol_base = MAKE_LOCATOR( (lss->tip_vol - &lss->vol[0]), 0 );
  lss->com.writable = 1;

  /*
   *  Delete and reload the master index
   */
  lssi_free_mindex( lss );
  lssi_load_mindex( lss );

  return 0;
}

unsigned lss_current_generation( LSS *generic_ptr )
{
  LSS_V3 *lss = (LSS_V3 *)generic_ptr;

  assert( generic_ptr->fn == &lss_v3 );

  return lss->cr.generation;
}

size_t lss_get_vol_size( LSS *generic, int vol )
{
  LSS_V3 *lss = v3chk( generic );
  struct IOBuf *buf;

  if (lss->vol[vol].filedes < 0)
    {
      return 0;
    }

  if (&lss->vol[vol] == lss->tip_vol)
    {
      buf = &lss->outbuf[ lss->num_outbufs - 1 ];
    }
  else
    {
      buf = lss->vol[vol].auxbuf;
      if (!buf)
        {
          return lseek( lss->vol[vol].filedes, 0, SEEK_END );
        }
    }
  return buf->base_offset + (buf->ptr - buf->base);
}

struct QR {
  UINT_32   recnum;
  UINT_32   location;
};

static int cmp_qr( const void *a, const void *b )
{
  if (((struct QR *)a)->location < ((struct QR *)b)->location)
    return -1;
  else
    return 1;
}

int lss_tune( LSS *generic, const char *key, const char *value )
{
  LSS_V3 *lss = v3chk( generic );

  if (strcmp( key, "fsync" ) == 0) {
    if (strcmp( value, "t" ) == 0) {
      lss->do_fsync = 1;
      return 0;
    } else if (strcmp( value, "f" ) == 0) {
      lss->do_fsync = 0;
      return 0;
    } else {
      return -EINVAL;
    }
  } else {
    return -EDOM;
  }
}

int lss_record_query( LSS *generic, 
                      UINT_32 from, UINT_32 to, 
                      UINT_32 **result )
{
  unsigned vmask;
  int i, m, n, nx;
  LSS_V3 *lss = v3chk( generic );
  UINT_32 *qr;
  IndexEntry **xlist;
  struct QR *qx;

  vmask = 0;
  for (i=EXTRACT_VOLUME(from); i<=EXTRACT_VOLUME(to); i++)
    {
      vmask |= (1 << i);
    }

  /*
   *  step 1.  Find the candidate mindex entries
   */

#if HAVE_ALLOCA
  xlist = alloca( sizeof( IndexEntry * ) * lss->mindex_cap );
#else
  xlist = malloc( sizeof( IndexEntry * ) * lss->mindex_cap );
#endif

  n = 0;
  nx = 0;
  for (i=0; i<lss->mindex_cap; i++)
    {
      if (!(lss->mindex[i].mem_ptr || lss->mindex[i].m.line_offset))
        continue;
        
      if (lss->mindex[i].m.vol_flags & VOLFLAG_DIRTY)
        {
          lssi_get_line( lss, i );
          recompute_volflags( &lss->mindex[i] );
        }

      if (lss->mindex[i].m.vol_flags & vmask)
        {
          UINT_32 *tab;
          int j;
          
          /*  make sure this index line is loaded */
          lssi_get_line( lss, i );
          tab = lss->mindex[i].mem_ptr->entry;

          xlist[nx++] = &lss->mindex[i];

          for (j=0; j<INDEX_LINE_SIZE; j++)
            {
              if (tab[j] && (tab[j] >= from) && (tab[j] < to))
                {
                  n++;
                }
            }
        }
    }
  
  /*
   *  step 2.  Build the final array
   */

  if (n == 0) {
    /* short circuit the case that the result is empty */
    *result = NULL;
#if !HAVE_ALLOCA
    free( xlist );
#endif
    return 0;
  }

  qr = malloc( sizeof(UINT_32) * n );
  if (!qr)
    {
#if !HAVE_ALLOCA
      free( xlist );
#endif
      return -1;
    }
  qx = (struct QR *)malloc( sizeof(struct QR) * n );
  if (!qx)
    {
#if !HAVE_ALLOCA
      free( xlist );
#endif
      free( qr );
      return -1;
    }

  m = n;
  n = 0;
  for (i=0; i<nx; i++)
    {
      UINT_32 *tab = xlist[i]->mem_ptr->entry;
      UINT_32 rbase = xlist[i]->m.line_key * INDEX_LINE_SIZE;
      int j;

      for (j=0; j<INDEX_LINE_SIZE; j++)
        {
          if (tab[j] && (tab[j] >= from) && (tab[j] < to))
            {
              qx[n].recnum = rbase + j;
              qx[n].location = tab[j];
              n++;
            }
        }
    }
  assert( m == n );

  qsort( qx, n, sizeof( struct QR ), cmp_qr );

  for (i=0; i<n; i++)
    {
      qr[i] = qx[i].recnum;
    }
#if !HAVE_ALLOCA
  free( xlist );
#endif
  free( qx );
  *result = qr;
  return n;
}

