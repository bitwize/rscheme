#include <stdio.h>
#include <ctype.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <setjmp.h>
#include <errno.h>
#include <time.h>

#include "lsspriv.h"
#include "lssv3.h"

typedef UINT_32 UINT_W;

int RS_LPGC( unsigned cat_id, unsigned msg_id, const char *fmt, ... )
{
  return -1;
}

static void (*more_verbose)( struct LSSRecordHeader *h, void *data );
static void *loaded_data;

extern int verbose;
#define VVP "          : -- "


static char *line_base = NULL;
static char *line_limit = NULL;
static char *line_ptr;

static jmp_buf line_full;

static void lputs( char *str )
{
  while (*str)
    {
      if (line_ptr < line_limit)
	{
	  *line_ptr++ = *str++;
	}
      else
	{
	  /* back-fill the "..." */
	  line_ptr[-3] = '.';
	  line_ptr[-2] = '.';
	  line_ptr[-1] = '.';
	  /* we reserved space for this, plus the NUL, in linit() */
	  *line_ptr++ = '\n';
	  longjmp( line_full, 1 );
	}
    }
}

static void lputc( char ch )
{
  char temp[2];
  temp[0] = ch;
  temp[1] = 0;
  lputs( temp );
}

static void linit( int width )
{
  if ((line_limit - line_base) < (width + 2))
    {
      if (line_base)
	free( line_base );
      line_base = malloc( width + 2 );
      line_limit = line_base + width;
    }
  line_ptr = line_base;
}

static void lprintf( char *fmt, ... )
{
  va_list va;
  char temp[100];
  int i, n;

  va_start( va, fmt );
  n = vsprintf( temp, fmt, va );
  va_end( va );

  lputs( temp );
}

static void lprint_hex_data( UINT_8 *addr, unsigned len )
{
  int i;

  lputs( "<" );
  for (i=0; i<len; i++)
    {
      if (i > 0)
	lputs( " " );
      lprintf( "%02x", addr[i] );
    }
  lputs( ">" );
}

static void lprint_data( UINT_8 *addr, unsigned len )
{
  unsigned i;

  lprintf( " %u <", len );

  for (i=0; i<len; i++)
    {
      switch (addr[i])
	{
	case '\0':
	  lputs( "\\0" );
	  break;

	case '\n':
	  lputs( "\\n" );
	  break;

	case '\t':
	  lputs( "\\t" );
	  break;

	case '\r':
	  lputs( "\\r" );
	  break;

	default:
	  if (isprint(addr[i]))
	    {
	      lputc( addr[i] );
	    }
	  else
	    {
	      lprintf( "\\%03o", addr[i] );
	    }
	}
    }
  lprintf( ">" );
}

static void lprint_volh( void *data )
{
  struct LSSVolumeHeader *h = data;
  lprintf( " (volume ~d of ~d)", h->vol_number, h->num_vols );
}

static void vv_volheader( struct LSSRecordHeader *rh, void *data )
{
  unsigned i, n;
  struct LSSVolumeHeader *h = data;
  struct tm *t;
  time_t tv;
  char buf1[30], buf2[30];

  printf( VVP"volume number: %lu\n", h->vol_number );
  printf( VVP"number of volumes: %lu\n", h->num_vols );
  printf( VVP"volume header generation: %lu\n", h->vh_generation );
  printf( VVP"last CR: at v%1x + %08x , generation %lu\n",
          EXTRACT_VOLUME( h->last_cr_at ),
          EXTRACT_OFFSET( h->last_cr_at ),
          h->last_cr_generation );

  tv = h->vol_create_time_ms / 1000;
  t = localtime( &tv );
  strftime( buf1, sizeof(buf1), "%Y-%m-%d %H:%M:%S", t );
  strftime( buf2, sizeof(buf2), "%Z", t );

  printf( VVP"volume ctime: %llu ms : %s.%03u %s\n",
          h->vol_create_time_ms,
          buf1,
          (unsigned)(h->vol_create_time_ms % 1000),
          buf2 );
  n = 0;
  for (i=1; i<MAX_ZIP_ALGORITHMS; i++) {
    if (h->zip_alg_at[i-1]) {
      n++;
    }
  }
  printf( VVP"compression algorithms: %u\n", n );
  for (i=1; i<MAX_ZIP_ALGORITHMS; i++) {
    if (h->zip_alg_at[i-1]) {
      printf( VVP"    algorithm[%u] at v%1x + %08x\n",
              i,
              EXTRACT_VOLUME( h->zip_alg_at[i-1] ),
              EXTRACT_OFFSET( h->zip_alg_at[i-1] ) );
    }
  }
  
  n = 0;
  for (i=0; i<MAX_VOLUMES; i++) {
    if (h->vol_info[i].vol_file_at) {
      n++;
    }
  }
  printf( VVP"total volumes: %u\n", n );
  for (i=0; i<MAX_VOLUMES; i++) {
    if (h->vol_info[i].vol_file_at) {
      printf( VVP"    volume[%u]:\n", i );
      printf( VVP"        volume file name at: v%1x + %08x\n",
              EXTRACT_VOLUME( h->vol_info[i].vol_file_at ),
              EXTRACT_OFFSET( h->vol_info[i].vol_file_at ) );
      printf( VVP"        commit record at: v%1x + %08x\n",
              EXTRACT_VOLUME( h->vol_info[i].cr_offset ),
              EXTRACT_OFFSET( h->vol_info[i].cr_offset ) );
      printf( VVP"        volume serial: %016llx\n",
              h->vol_info[i].vol_serial_num );
    }
  }
}

static void vv_commit( struct LSSRecordHeader *h, void *data )
{
  unsigned i, nbook;
  struct LSSCommitRecord c;
  struct tm *t;
  time_t tv;
  char buf1[30], buf2[30];

  memcpy( ((char *)&c), h, sizeof( struct LSSRecordHeader ) );
  memcpy( ((char *)&c) + sizeof( struct LSSRecordHeader ), data, 
          sizeof( struct LSSCommitRecord ) 
          - sizeof( struct LSSRecordHeader ) );

  tv = c.commit_time_ms / 1000;
  t = localtime( &tv );
  strftime( buf1, sizeof(buf1), "%Y-%m-%d %H:%M:%S", t );
  strftime( buf2, sizeof(buf2), "%Z", t );

  printf( VVP"commit time: %llu ms : %s.%03u %s\n", 
          c.commit_time_ms,
          buf1,
          (unsigned)(c.commit_time_ms % 1000),
          buf2 );

  printf( VVP"minor version: %lu\n", c.minor_version );
  printf( VVP"previous cr at: v%1x + %08x\n", 
          EXTRACT_VOLUME( c.prev_cr_at ),
          EXTRACT_OFFSET( c.prev_cr_at ) );
  printf( VVP"this cr at: v%1x + %08x\n",
          EXTRACT_VOLUME( c.self_cr_at ),
          EXTRACT_OFFSET( c.self_cr_at ) );
  printf( VVP"master index:\n" );
  printf( VVP"    mindex count: %lu\n", c.mix_cnt );
  printf( VVP"    mindex at: v%1x + %08x\n",
          EXTRACT_VOLUME( c.mix_offset ),
          EXTRACT_OFFSET( c.mix_offset ) );
  printf( VVP"volume header fuel: %d\n", c.vh_fuel );
  printf( VVP"diff list: %lu diffs\n", c.num_diffs );
  for (i=0; i<c.num_diffs; i++) {
    printf( VVP"        diff[%u] : record %08x is at v%1x + %08x\n",
            i,
            c.diffs[i].diff_recnum,
            EXTRACT_VOLUME( c.diffs[i].diff_offset ),
            EXTRACT_OFFSET( c.diffs[i].diff_offset ) );
  }
  nbook = 0;
  for (i=0; i<NUM_BOOKMARKS; i++) {
    if (c.bookmarks[i]) {
      nbook++;
    }
  }
  printf( VVP"bookmark set: %u bookmarks\n", nbook );
  for (i=0; i<NUM_BOOKMARKS; i++) {
    if (c.bookmarks[i]) {
      printf( VVP"    bookmark[%u] : at v%1x + %08x\n",
              i,
              EXTRACT_VOLUME( c.bookmarks[i] ),
              EXTRACT_OFFSET( c.bookmarks[i] ) );
    }
  }
}

static void lprint_commit( struct LSSRecordHeader *h, void *data )
{
  struct LSSCommitRecord c;
  struct tm *t;
  time_t tv;
  char buf[30];

  memcpy( ((char *)&c), h, sizeof( struct LSSRecordHeader ) );
  memcpy( ((char *)&c) + sizeof( struct LSSRecordHeader ), data, 
          sizeof( struct LSSCommitRecord ) 
          - sizeof( struct LSSRecordHeader ) );

  tv = c.commit_time_ms / 1000;
  t = localtime( &tv );
  strftime( buf, sizeof(buf), "%Y-%m-%d %H:%M:%S", t );
  lprintf( " gen %u <%s>", c.generation, buf );
}

static void lprint_words( void *data, unsigned len )
{
  UINT_W *array = data;
  int left = 49;
  unsigned i, count;

  count = len / sizeof( UINT_W );
  
  lprintf( " %u <", len );

  for (i=0; i<count; i++)
    {
      if (i)
	lputs( " " );

      lprintf( "%#lx", array[i] );
    }
  lputs( ">" );
}

static void vv_index( struct LSSRecordHeader *h, void *data )
{
  struct LSSIndexEntries *ix = data;
  unsigned i;
  UINT_32 base;

  base = h->recnum * INDEX_LINE_SIZE;
  printf( VVP"index line for %08x...\n", base );
  for (i=0; i<INDEX_LINE_SIZE; i++) {
    if (ix->entry[i]) {
      printf(VVP"    %08x at v%1x + %08x\n",
             base + i,
             EXTRACT_VOLUME( ix->entry[i] ),
             EXTRACT_OFFSET( ix->entry[i] ) );
    }
  }
}

static void vv_mindex( struct LSSRecordHeader *h, void *data )
{
  unsigned i, n;
  struct MasterIndexEntry *mindex = data;

  n = h->length / sizeof(struct MasterIndexEntry);
  printf( VVP"master index: %u entries\n", n );
  for (i=0; i<n; i++) {
    unsigned j, fl = mindex[i].vol_flags;
    char *ff = "";

    printf( VVP"   line[%u] for %08x... at v%1x + %08x , flags %08x (",
            i,
            mindex[i].line_key * INDEX_LINE_SIZE,
            EXTRACT_VOLUME( mindex[i].line_offset ),
            EXTRACT_OFFSET( mindex[i].line_offset ),
            fl );
    for (j=0; j<LSS_MAX_VOLUMES; j++) {
      if (fl & VOLFLAG_VOL(j)) {
        printf( "%s%1x", ff, j );
        ff = "";
      }
    }
    if (fl & VOLFLAG_DIRTY) {
      printf( "%sD", ff );
      ff = "";
    }
    if (fl & VOLFLAG_HAS_FREE) {
      printf( "%sF", ff );
      ff = "";
    }
    printf( ")\n" );
  }
}


static void ldump_line( int fd, struct LSSRecordHeader *h, long at )
{
  char type[5];
  UINT_8 *data;
  int space, n;

  type[0] = (h->magic >> 24) & 0xFF;
  type[1] = (h->magic >> 16) & 0xFF;
  type[2] = (h->magic >>  8) & 0xFF;
  type[3] = (h->magic >>  0) & 0xFF;
  type[4] = 0;

  space = GET_SPACE_BYTES( h->space_word );

  lprintf( "%#010lx: %s %6d ", at, type, space );

  /*  we don't want to count the header in the space,
   *  since we aren't going to read() it, and there is 
   *  no need to print it
   */
  space -= sizeof( struct LSSRecordHeader );

  if (space == 0)
    {
      lprintf( "\n" );
      return;
    }

  data = malloc( space );
  assert( data );

  n = read( fd, data, space );
  if (n != space)
    {
      if (n < 0)
	{
	  lprintf( "--!-- %s (%d)\n", strerror( errno ), errno );
	}
      else
	{
	  lprintf( "--!-- short read (only %d)\n", n );
	}
      free( data );
      return;
    }
  loaded_data = data;

  if (GET_WHICH_ZIP( h->space_word ))
    {
      if (h->magic == DATAREC_MAGIC)
	lprintf( " %08x", h->recnum );
      
      lprintf( " %lu (z%d) ", 
	      h->length, GET_WHICH_ZIP( h->space_word ) );
      lprint_hex_data( data, space );
    }
  else if (h->length > 0)
    {
      switch (h->magic)
	{
	case MINDEX_MAGIC:
          more_verbose = vv_mindex;
	  lprint_words( data, h->length );
          break;
	case INDEX_MAGIC:
          more_verbose = vv_index;
	  lprint_words( data, h->length );
	  break;
	case DATAREC_MAGIC:
	  lprintf( " %lu", h->recnum );
	default:
	  lprint_data( data, h->length );
	  break;
	case LSS_MAGIC:
          more_verbose = vv_volheader;
	  break;
	}
    } else {
      switch (h->magic) {
      case LSS_MAGIC:
        more_verbose = vv_volheader;
        lprint_volh( data );
        break;
      case COMMIT_MAGIC:
        more_verbose = vv_commit;
        lprint_commit( h, data );
        break;
      }
    }
  lputc( '\n' );
}

static void dump_line( int fd, struct LSSRecordHeader *h, long at )
{
  linit( 79 );

  more_verbose = NULL;
  loaded_data = NULL;

  if (setjmp( line_full ) == 0)
    {
      ldump_line( fd, h, at );
    }
  fwrite( line_base, 1, line_ptr - line_base, stdout );
  if (more_verbose && verbose > 1 && loaded_data) {
    more_verbose( h, loaded_data );
  }
  if (loaded_data) {
    free( loaded_data );
  }
}

void print_hex_data( UINT_8 *addr, unsigned len )
{
  linit( 79 );
  if (setjmp( line_full ) == 0)
    {
      lprint_hex_data( addr, len );
    }
  fwrite( line_base, 1, line_ptr - line_base, stdout );
}

void print_record( unsigned long recno, 
		   unsigned char *addr, 
		   unsigned len )
{
  linit( 79 );
  if (setjmp( line_full ) == 0)
    {
      lprintf( "%lu: ", recno );
      lprint_data( addr, len );
      lputs( "\n" );
    }
  fwrite( line_base, 1, line_ptr - line_base, stdout );
}


void low_level_dump( const char *file )
{
  struct LSSRecordHeader h;
  int n;
  int fd;

  fd = open( file, O_RDONLY );
  if (fd < 0)
    {
      perror( file );
      exit(1);
    }

  while (1)
    {
      long at = lseek( fd, 0, SEEK_CUR );

      n = read( fd, &h, sizeof h );
      if (n != sizeof h)
	{
	  if (n < 0)
	    {
	      perror( file );
	    }
	  else if (n == 0)
	    {
	      printf( "%#010lx:\n", at );
	      close( fd );
	      return;
	    }
	  else
	    {
	      fprintf( stderr, "%s: short read (%d)\n", file, n );
	    }
	  exit(1);
	}
      dump_line( fd, &h, at );
      lseek( fd, at + GET_SPACE_BYTES(h.space_word), SEEK_SET );
    }
}
