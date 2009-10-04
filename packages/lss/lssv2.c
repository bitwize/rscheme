#include <unistd.h>
#include <fcntl.h>
#include <sys/file.h>
#include <stdio.h>
#include <assert.h>
#include <stdarg.h>
#include <errno.h>
#include <string.h>
#include <sys/uio.h>

#include "lsspriv.h"
#include "lssv2.h"
#include "alloc.h"

static struct LSSMethods lss_v2;

#define ACCESS_REC(a)   ((a)->rec)

#if HAVE_FLOCK
int flock( int fd, int style );
#endif

int fsync( int fd );

static struct LSS_Record *find_entry( LSS_V2 *lss, UINT_32 rec_num );

#define NUM_CAPACITIES  (9)

static UINT_32 index_capacities[NUM_CAPACITIES] =
	{11, 71, 541, 1223, 2741, 6133, 13499, 29443, 63809};


static UINT_32 next_higher_capacity( UINT_32 old_cap )
{
unsigned i;

    /* find the next higher capacity; Note that if
       the old capacity is taken from a different table,
       this may not increase the capacity by much! (but at least 1)
    */
    for (i=0; i<NUM_CAPACITIES; i++)
    {
	if (index_capacities[i] > old_cap)
	{
	    return index_capacities[i];
	}
    }
    
    /* make it 1.75 times bigger, and fairly odd */

    return ((old_cap * 3) / 2) | 15;
}

static void alloc_new_index( LSS_V2 *lss, UINT_32 new_cap )
{
  lss->index_capacity = new_cap;
  lss->index = ALLOCN( struct LSS_Record, new_cap );
  memset( lss->index, 0, sizeof(struct LSS_Record) * new_cap );
}


/* this is called when the population in the index meets or exceeds 80% */

static void increase_index_capacity( LSS_V2 *lss )
{
  struct LSS_Record *src, *old_index;
  UINT_32 i, old_count, spotted, new_cap, old_cap;

  /* save the old index */
  
  old_index = lss->index;
  old_cap = lss->index_capacity;
  
  new_cap = next_higher_capacity( lss->index_capacity );
  alloc_new_index( lss, new_cap );
  
  /* copy in stuff from old index
     since we're adding 'em one by one, reset the count to 0
     */
  old_count = lss->index_count;
  lss->index_count = 0;
  spotted = 0;
  
  for (i=0, src=old_index; i<old_cap; i++, src++)
    {
      if (src->number)
	{
	  *find_entry( lss, src->number ) = *src;
	  spotted++;
	}
    }
  /* now they should be the same */
  assert( old_count == lss->index_count );
  assert( spotted == lss->index_count );
  
  free( old_index );
}

static void lssi_close( LSS *l )
{
  LSS_V2 *lss = (LSS_V2 *)l;

  if (lss->num_accesses)
    {
      lssi_signal_error( &lss->com, LSSERR_UNRELEASED_ACCESS, "" );
    }

  close( lss->fd );
  /* HUGE HUGE HUGE RESOURCE LEAK HERE...
     we should free ALL the LSS structures 
     */
  free(lss);
}

static LSS_V2 *lss_open_existing( LSS_V2 *lss, off_t CR_offset )
{
  size_t n;
  commit_info_t *ci = &lss->last_commit;

  ci->lss_magic = 0;
  errno = 0;

  lseek( lss->fd, CR_offset, SEEK_SET );
  n = read( lss->fd, ci, sizeof(commit_info_t) );

  if ((n != sizeof(commit_info_t)) || (ci->lss_magic != LSS_MAGIC))
    {
      close(lss->fd);
      free(lss);
      errno = LSSERR_NOT_LSS;
      return NULL;
    }
  if (ci->lss_fmt_version != LSS_FMT_VERSION)
    {
      close(lss->fd);
      free(lss);
      errno = LSSERR_BAD_VER;
      return NULL;
    }
  lss->rec0_len = ci->client_info_len;
  lss->rec0 = malloc( lss->rec0_len );
  n = read( lss->fd, lss->rec0, lss->rec0_len );

  if (n != ci->client_info_len)
    {
      if (n < 0)
	lssi_sys_error( &lss->com, "" );
      else
	lssi_signal_error( &lss->com, LSSERR_SHORT_READ, "" );
    }
  
  lss->index_capacity = ci->index_capacity;
  lss->index_count = ci->index_count;
  lss->index = ALLOCN( struct LSS_Record, ci->index_capacity );
  
  if (lseek( lss->fd, ci->index_offset, SEEK_SET ) != ci->index_offset)
    lssi_sys_error( &lss->com, "u", ci->index_offset );

  n = read( lss->fd, lss->index,
	    sizeof( struct LSS_Record ) * lss->index_capacity );
  if (n != sizeof( struct LSS_Record ) * lss->index_capacity)
    lssi_sys_error( &lss->com, "" );

  /*  
   *  this commit record was copied just after the index 
   */
  lss->spare_commit_at = lseek( lss->fd, 0, SEEK_CUR );
  return lss;
}

/* a random permutation with the 0 values replaced w/255 */

static UINT_8 hash_permutation[256] = {
240, 235, 36, 105, 218, 102, 186, 24, 61, 255, 252, 65, 16, 177, 48,
120, 32, 88, 234, 150, 178, 176, 229, 154, 33, 41, 30, 130, 137, 163,
107, 98, 93, 126, 58, 171, 106, 147, 192, 115, 132, 129, 180, 230,
124, 217, 231, 221, 156, 44, 118, 8, 225, 99, 42, 140, 17, 182, 172,
91, 158, 179, 103, 239, 63, 9, 6, 233, 54, 157, 159, 162, 169, 86, 95,
175, 104, 210, 2, 117, 57, 201, 167, 134, 82, 125, 74, 119, 241, 143,
25, 114, 75, 181, 62, 78, 53, 165, 136, 64, 66, 67, 109, 80, 247, 246,
245, 21, 213, 5, 168, 222, 148, 188, 4, 23, 145, 212, 244, 15, 43,
242, 227, 116, 208, 141, 71, 3, 52, 135, 139, 26, 85, 14, 40, 205,
133, 243, 149, 110, 216, 146, 34, 128, 152, 204, 37, 68, 214, 73, 198,
200, 27, 142, 174, 11, 122, 197, 87, 164, 203, 127, 96, 166, 50, 100,
19, 153, 251, 184, 215, 12, 108, 144, 20, 151, 22, 113, 121, 29, 72,
191, 255, 237, 35, 10, 94, 59, 236, 223, 253, 89, 238, 155, 211, 31,
224, 131, 185, 193, 49, 56, 38, 249, 79, 70, 160, 47, 228, 219, 97,
170, 45, 161, 55, 226, 39, 46, 101, 76, 1, 207, 112, 250, 220, 84, 81,
206, 232, 173, 183, 51, 199, 196, 190, 248, 209, 7, 111, 90, 202, 13,
189, 69, 195, 60, 28, 92, 254, 83, 187, 138, 194, 123, 77, 18 };

static struct LSS_Record *find_entry( LSS_V2 *lss, UINT_32 rec_num )
{
struct LSS_Record *p;
UINT_32 i;

    /* hash */

    i = hash_permutation[ rec_num & 0xFF ];
    i *= hash_permutation[ (rec_num >> 8) & 0xFF ];
    i *= hash_permutation[ (rec_num >> 16) & 0xFF ];
    i *= hash_permutation[ (rec_num >> 24) & 0xFF ];

    /* max value of i is 4,228,250,625 (0xFC05FC01),
       but with a non-flat distribution 
       */

    i %= lss->index_capacity;

    p = &lss->index[i];

    /* from then on, keep looking sequentially
       (because we're never at 100% capacity, we're guaranteed
       to either find it or find an empty record) */

    while (1)
    {
	if (p->number == rec_num)
	    return p;
	else if (!p->number)
	{
	    /* check for overflow at 80% */
	    if (((lss->index_count * 10) / 8) >= lss->index_capacity)
	    {
		increase_index_capacity( lss );
		/* all our pointers are dangling, so
		   start over again, looking for this entry
		*/
		return find_entry( lss, rec_num );
	    }
	    /* adding a new entry */
	    lss->index_count++;
	    p->number = rec_num;
	    return p;
	}
	p++;
	i++;
	if (i >= lss->index_capacity)
	{
	    /* wrapped around... go back to beginning */
	    i = 0;
	    p = lss->index;
	}
    }
}

static void do_writev( LSS_V2 *lss, struct iovec *vec, int nv, UINT_32 rec,
		       UINT_32 tot_len )
{
  int n;
  
  n = writev( lss->fd, vec, nv );
  if (n != tot_len)
    {
      if (n < 0)
	{
	  if (errno == ENOSPC)
	    {
	      lssi_signal_error( &lss->com, LSSERR_DISK_FULL, "" );
	    }
	  else
	    {
	      lssi_sys_error( &lss->com, "u", rec );
	    }
	}
      else
	{
	  lssi_signal_error( &lss->com, LSSERR_SHORT_WRITE, "llu", 
			     (long)n, (long)tot_len, (unsigned)rec );
	}
    }
}

static void do_write( LSS_V2 *lss, void *data, UINT_32 bytes, UINT_32 rec )
{
  struct iovec v[1];

  v[0].iov_base = data;
  v[0].iov_len = bytes;
  do_writev( lss, v, 1, rec, bytes );
}

static void lssi_writev( LSS *gp, UINT_32 rec, zipbuf *vec, 
			 zip_algorithm *use )
{
  LSS_V2 *lss = (LSS_V2 *)gp;

  if (use && use != &lss_null_zip)
    {
      lssi_signal_error( &lss->com, LSSERR_INVALID_ZIP_ALGORITHM, "" );
    }

  if (rec == 0)
    {
      int i;
      char *d;

      /* record 0 ==> client commit info */
      lss->rec0_len = zipbuf_len( vec );
      lss->rec0 = malloc( lss->rec0_len );
      d = lss->rec0;

      for (i=0; vec[i].ptr; i++)
	{
	  int n = (char *)vec[i].limit - (char *)vec[i].ptr;
	  memcpy( d, vec[i].ptr, n );
	  d += n;
	}
    }
  else
    {
      struct iovec temp[20];
      struct LSS_Record *r = find_entry( lss, rec );
      int n, fill;
      UINT_32 bytes = 0;
      static char zeros[3] = { 0, 0, 0 };

      r->offset = lseek( lss->fd, 0, SEEK_END );

      for (n=0; vec[n].ptr; n++)
	{
	  temp[n].iov_base = vec[n].ptr;
	  temp[n].iov_len = (char *)vec[n].limit - (char *)vec[n].ptr;
	  bytes += temp[n].iov_len;
	}
      r->length = bytes;

      fill = (4 - bytes) & 3;
      if (fill)
	{
	  temp[n].iov_base = zeros;
	  temp[n].iov_len = fill;
	  bytes += fill;
	  n++;
	}
      assert( n <= 20 );

      do_writev( lss, temp, n, r->number, bytes );
    }
}

static LSSAccess *lssi_read_access( LSS *gp, UINT_32 record_num )
{
  LSS_V2 *lss = (LSS_V2 *)gp;
  LSSAccess *a = ALLOC( LSSAccess ); /* could make this cheaper ... */
  struct LSS_Record *r;

  if (record_num == 0)
    {
      a->addr = lss->rec0;
      a->bytes = lss->rec0_len;
    }
  else
    {
      int nb;

      r = find_entry( lss, record_num );
      if (!r->offset)
	{
	  lssi_signal_error( &lss->com, LSSERR_NO_RECORD, 
			     "l", (long)record_num );
	}
	  
      lseek( lss->fd, r->offset, SEEK_SET );
      a->bytes = r->length;
      a->addr = malloc( a->bytes );
      nb = read( lss->fd, a->addr, a->bytes );
      if (nb != a->bytes)
	{
	  if (nb < 0)
	    lssi_sys_error( &lss->com, "u", record_num );
	  else
	    lssi_signal_error( &lss->com, LSSERR_SHORT_READ, 
			       "l", (long)record_num );
	}
      
      lss->num_accesses++;
    }
  return a;
}

static void lssi_readv( LSS *generic_ptr,
			zipbuf *dest,
			LSSAccess *a )
{
  /*  actualy, we could `readv' right into the buffer, now that
   *  we opacified LSSAccess
   */
  lss_null_zip.unzip_meth( &lss_null_zip, dest, a->addr );
}

static void lssi_read_release( LSS *gp, LSSAccess *a )
{
  LSS_V2 *lss = (LSS_V2 *)gp;
  if (a->addr != lss->rec0)
    {
      assert( lss->num_accesses > 0 );
      lss->num_accesses--;
      free( (char *)a->addr );
    }
  free( a );
}


static UINT_32 lssi_commit( LSS *gp, int flag )
{
  LSS_V2 *lss = (LSS_V2 *)gp;

  void *client_info = lss->rec0;
  UINT_32 client_len = lss->rec0_len;

  char temp[512];
  
  if (flag >= 0)
    {
      lssi_signal_error( &lss->com, LSSERR_NOT_IMPL, "l", (long)flag );
    }

  if (client_len + sizeof(commit_info_t) > 512)
    {
      lssi_signal_error( &lss->com, LSSERR_TOO_BIG, "" );
    }
  
  lss->last_commit.lss_magic = LSS_MAGIC;
  lss->last_commit.index_capacity = lss->index_capacity;
  lss->last_commit.index_count = lss->index_count;
  lss->last_commit.index_offset = lseek( lss->fd, 0, SEEK_END );
  lss->last_commit.client_info_len = client_len;


  do_write( lss, 
	    lss->index, 
	    sizeof(struct LSS_Record) * lss->index_capacity,
	    0 );

  lss->last_commit.commit_version++;
  lss->last_commit.commit_time = time(NULL);
  lss->last_commit.prev_commit_at = lss->spare_commit_at;

  lss->spare_commit_at = lseek( lss->fd, 0, SEEK_CUR );

  /*
   * write out a copy of the commit record, in case the first
   * block is lost or you want to roll back multiple versions
   */
  do_write( lss,
	    &lss->last_commit, 
	    sizeof(commit_info_t),
	    0 );
  do_write( lss,
	    client_info,
	    client_len,
	    0 );

  lseek( lss->fd, 0, SEEK_SET );
  if (fsync( lss->fd ) < 0)
    {
      lssi_sys_error( &lss->com, "" );
    }
    
  memcpy( temp, &lss->last_commit, sizeof( commit_info_t ) );
  memcpy( temp + sizeof( commit_info_t ), client_info, client_len );

  do_write( lss, 
	    temp,
	    sizeof(commit_info_t) + client_len,
	    0 );

  if (fsync( lss->fd ) < 0)
    {
      lssi_sys_error( &lss->com, "" );
    }
  return lss->spare_commit_at;
}


#if 0
void *lss_get_client_commit_info( LSS *lss, UINT_32 *len )
{
  if (len)
    *len = lss->last_commit.client_info_len;
  return lss->client_commit_info;
}

void lss_get_lss_commit_info( LSS *lss, commit_info_t *ci )
{
    *ci = lss->last_commit;
}
#endif


static UINT_32 *lssi_get_index( LSS *gp, UINT_32 *cnt )
{
  LSS_V2 *lss = (LSS_V2 *)gp;

  UINT_32 *ix = ALLOCN( UINT_32, 1 + (lss->index_count) );
  UINT_32 i, j = 0;
  
  for (i=0; i<lss->index_capacity; i++)
    {
      if (lss->index[i].offset)
	{
	  ix[j++] = lss->index[i].number;
	}
    }
  assert( j == lss->index_count );

  ix[j++] = 0; /* always have record zero, aka ``client_commit_info'' */

  *cnt = j;
  return ix;
}

static void lssi_get_info( LSS *gp, UINT_32 rec, struct LSSRecordInfo *info )
{
  LSS_V2 *lss = (LSS_V2 *)gp;
  if (rec == 0)
    {
      info->record_num = 0;
      info->volume = 0;
      info->offset = 0; /* bogus offset */
    }
  else
    {
      struct LSS_Record *e = find_entry( lss, rec );

      info->record_num = rec;
      info->volume = 0;
      info->offset = e->offset;
    }
}


static void lssi_raw_write( LSS *generic_ptr,
			    UINT_32 recnum,
			    void *data,
			    size_t unz_len,
			    zip_algorithm *used,
			    size_t z_len )
{
  lssi_signal_error( generic_ptr, LSSERR_NOT_IMPL, "" );
}

static size_t lssi_copy_record( LSS *generic_ptr,
			        LSS *dest,
			        UINT_32 recnum )
{
  lssi_signal_error( generic_ptr, LSSERR_NOT_IMPL, "" );
  return 0;
}

/*
LSS *lss_open( const char *path, int mode,
	       lss_error_handler_t *handler, 
	       void *info,
	       off_t CR_offset )
*/
LSS *lssv2_open( const char *file, int fd, int writable, UINT_32 CR_offset )
{
  LSS_V2 *s;
  commit_info_t x;

  if (CR_offset && writable)
    {
      errno = EINVAL;
      return NULL;
    }

  s = ALLOC(LSS_V2);
  s->fd = fd;
  s->lock_held = writable;

  s->num_accesses = 0;

  s = lss_open_existing( s, CR_offset );

  if (!s)
    return NULL;

  LSS_INIT_COM( s->com, &lss_v2 );

  return &s->com;
}

/* (vol_num == -1) => latest volume */

static const char *lssi_filename( LSS *lss, int vol_num )
{
  return "unknown:lss-v2";
}

static UINT_32 lssi_find_vrecord( LSS *generic_ptr, unsigned mask, int pass )
{
  return ~(UINT_32)0;
}

static int lssi_delete( LSS *generic_ptr, unsigned rec )
{
  return -EINVAL;
}

static int lssi_alloc_recs( LSS *generic_ptr, unsigned minr, unsigned cnt,
                            UINT_32 *rec )
{
  return -EINVAL;
}


static struct LSSMethods lss_v2 = {
  "V2",
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
