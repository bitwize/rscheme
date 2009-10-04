#include <stdio.h>
#include <ctype.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <errno.h>
#include <sys/stat.h>

#include <rscheme/pkgs/lss/lss.h>
#include <rscheme/pkgs/lss/lsserrno.h>
#include "alloc.h"
#include "timebase/timebase.h"

static zip_algorithm *use_zip = NULL;
int verbose = 1;
static char *override_ancestor = NULL;

static void plsserror( const char *file )
{
  if ((errno >= LSSERR_MIN) && (errno <= LSSERR_MAX))
    fprintf( stderr, "%s: lss error %d\n", file, errno );
  else
    perror( file );
  exit(1);
}

void print_record( unsigned long recno, 
		   unsigned char *addr, 
		   unsigned len );

char *slurp( FILE *strm, unsigned *len )
{
  char *data, *p, *lim;
  int ch;

  data = malloc( 65536 );
  lim = data + 65536;

  p = data;
 
  while (1)
    {
      int cnt;

      cnt = fread( p, 1, lim - p, strm );
      if (cnt <= 0)
	break;
      p += cnt;

      if (p >= lim)
	{
	  unsigned new_size = (lim - data) * 2;
	  unsigned old_used = p - data;

	  data = realloc( data, new_size );
	  lim = data + new_size;
	  p = data + old_used;
	}
    }

  *len = p - data;

  *p = 0;
  return data;
}

/*
 *   `=' -> chop trailing
 *   `-' -> eat everything
 *   '<FILE' -> file
 *  otherwise, the string itself
 */

void parse_input_arg( zipbuf *b, const char *str )
{
  if ((strcmp( str, "-" ) == 0) || (strcmp( str, "=" ) == 0))
    {
      char *data;
      unsigned n;

      data = slurp( stdin, &n );

      if (strcmp( str, "=" ) == 0)
	{
	  if (data[n-1] == '\n')
	    n--;
	}
      if (verbose)
	printf( "stdin: %u bytes\n", n );

      b[0].ptr = data;
      b[0].limit = data + n;
    }
  else if (str[0] == '<')
    {
      FILE *f = fopen( str + 1, "r" );
      unsigned n;
      char *data;

      if (!f)
	{
	  perror( str + 3 );
	  exit(1);
	}
      data = slurp( f, &n );
      if (verbose)
	printf( "%s: %u bytes\n", str + 1, n );

      b[0].ptr = data;
      b[0].limit = data + n;
      fclose( f );
    }
  else
    {
      b[0].ptr = (char *)str;
      b[0].limit = (char *)str + strlen( str );
    }
}

void cmd_write( LSS *l, int k, const char *str )
{
  zipbuf b[2];

  parse_input_arg( &b[0], str );
  b[1].ptr = b[1].limit = NULL;

  lss_writev( l, k, b, use_zip );
  if (b[0].ptr != str)
    free( b[0].ptr );
}


static int cmp_rec_offset( const void *a, const void *b )
{
  const struct LSSRecordInfo *ai = a, *bi = b;
  
  if (ai->volume < bi->volume)
    {
      return -1;
    }
  else if (ai->volume > bi->volume)
    {
      return 1;
    }
  else
    {
      if (ai->offset < bi->offset)
	{
	  return -1;
	}
      else if (ai->offset > bi->offset)
	{
	  return 1;
	}
      else
	{
	  return 0;
	}
    }
}

enum copy_mode {
  COPY_FULL,
  COPY_CLONE,
  COPY_CLONE_VOLUME
};

static void cmd_copylike( const char *from, 
			  const char *to, 
			  enum copy_mode mode )
{
  LSS *src, *dst;
  UINT_32 *recs, i, n, n_all;
  zipbuf buf[2];
  struct LSSRecordInfo *infos;
  size_t have_buf;
  int hashmark_per, hashmark_left, last_vol = 0;

  src = lss_open( from, LSS_OPEN );
  if (!src)
    plsserror( from );

  if (mode == COPY_CLONE_VOLUME)
    {
      const char *ca;

      last_vol = 0;
      while (lss_filename( src, last_vol+1 ))
	last_vol++;

      if (last_vol > 0)
	{
	  ca = lss_filename( src, -2 );
	  
	  if (verbose)
            {
              if (override_ancestor)
                {
                  printf( "clone v.%d, common ancestor was: %s now: %s\n", 
                          last_vol, 
                          ca, override_ancestor );
                  ca = override_ancestor;
                }
              else
                {
                  printf( "clone v.%d, common ancestor: %s\n", last_vol, ca );
                }
            }
	  
	  dst = lss_open( to, LSS_EXTEND, 0666, ca );
	}
      else
	{
	  /* `-clonev' on the oldest volume is the same as `-clone' */
	  mode == COPY_CLONE;
	  dst = lss_open( to, LSS_CREATE, 0666 );
	}
    }
  else
    {
      dst = lss_open( to, LSS_CREATE, 0666 );
    }
  if (!dst)
    plsserror( to );

  have_buf = 500;
  buf[0].ptr = malloc( have_buf );
  buf[1].ptr = buf[1].limit = NULL;

  recs = lss_get_record_index( src, &n );

  /* sort it by file offset */

  n_all = n;
  infos = ALLOCN( struct LSSRecordInfo, n );

  for (i=0; i<n; i++)
    {
      lss_get_record_info( src, recs[i], &infos[i] );
    }
  free( recs );

  /*  if we are only cloning the volume, then filter
   *  the record list to pick out only those on the
   *  most recent volume
   */
  if (mode == COPY_CLONE_VOLUME)
    {
      int j = 0;

      for (i=0; i<n; i++)
	{
	  /*  use `>=' in case we ever let last_vol be some
	   *  earlier volume instead of the last
	   */
	  if (infos[i].volume >= last_vol)
	    {
	      infos[j++] = infos[i];
	    }
	}
      n = j;
    }

  qsort( infos, n, sizeof( struct LSSRecordInfo ), cmp_rec_offset );

  i = 0;
  if (verbose == 1)
    {
      if (mode == COPY_CLONE_VOLUME)
	i = printf( "%d/%d records: ", n, n_all );
      else
	i = printf( "%d records: ", n );
      fflush( stdout );
    }

  hashmark_per = (n + (76-i)) / (77 - i);
  if (hashmark_per < 1)
    hashmark_per = 1;
  hashmark_left = hashmark_per;

  for (i=0; i<n; i++)
    {
      UINT_32 rec = infos[i].record_num;

      if (verbose > 1)
	{
	  printf( "%10lu (at %d:%lu): ", 
		  rec,
		  infos[i].volume,
		  infos[i].offset );
	}
	
      hashmark_left--;
      if (hashmark_left <= 0)
	{
	  hashmark_left = hashmark_per;
	  if (verbose == 1)
	    {
	      printf( "#" );
	      fflush( stdout );
	    }
	}

      if ((mode == COPY_CLONE) || (mode == COPY_CLONE_VOLUME))
	{
	  size_t len;

	  len = lss_copy_record( dst, src, rec );
	  if (verbose > 1)
	    printf( "%lu\n", len );
	}
      else
	{
	  LSSAccess *r;
	  size_t len;
	  
	  r = lss_read_access( src, rec );
	  len = lss_access_bytes( r );
	  if (verbose > 1)
	    printf( "%lu bytes\n", len );
	  
	  /* see if we need a bigger buffer than we had */
	  if (len > have_buf)
	    {
	      size_t new_buf_len = 100+(len*3)/2;
	      buf[0].ptr = realloc( buf[0].ptr, new_buf_len );
	      have_buf = new_buf_len;
	    }
	  buf[0].limit = (char *)buf[0].ptr + len;
	  lss_readv( src, buf, r );
	  lss_writev( dst, rec, buf, use_zip );
	  lss_read_release( src, r );
	}
    }
  if (verbose == 1)
    {
      printf( "." );
      fflush( stdout );
    }
  lss_commit( dst, 0 );
  if (verbose == 1)
    {
      printf( "\n" );
    }
  lss_close( src );
  lss_close( dst );
  free( infos );
}

static void cmd_copy( const char *from, const char *to )
{
  cmd_copylike( from, to, COPY_FULL );
}

static void cmd_clone( const char *from, const char *to )
{
  cmd_copylike( from, to, COPY_CLONE );
}

static void cmd_clonev( const char *from, const char *to )
{
  cmd_copylike( from, to, COPY_CLONE_VOLUME );
}

static void cmd_read( LSS *l, UINT_32 r )
{
  LSSAccess *a;
  size_t len;
  zipbuf b[2];

  a = lss_read_access( l, r );

  len = lss_access_bytes( a );
  b[0].ptr = malloc( len );
  b[0].limit = (char *)b[0].ptr + len;
  b[1].ptr = NULL;

  lss_readv( l, b, a );

  if (verbose)
    {
      print_record( r, b[0].ptr, len );
    }
  else
    {
      fwrite( b[0].ptr, 1, len, stdout );
    }
  free( b[0].ptr );
  lss_read_release( l, a );
}

void cmd_write_append( LSS *l, const char *str )
{
  UINT_32 *index, cnt, i, rec;

  index = lss_get_record_index( l, &cnt );
  rec = 1;

  for (i=0; i<cnt; i++)
    {
      if (index[i] > rec)
	rec = index[i];
    }
  rec++;
  cmd_write( l, rec, str );
  printf( "append %lu\n", rec );
}


void list_index( LSS *l )
{
  UINT_32 i, *index, cnt;
  struct LSSRecordInfo info;
  
  index = lss_get_record_index( l, &cnt );
  for (i=0; i<cnt; i++)
    {
      lss_get_record_info( l, index[i], &info );
      printf( "%lu %#lx %d %lu\n", index[i], index[i], 
	      info.volume, info.offset );
    }
  free( index );
}

void load_from_ascii( LSS *l, const char *file )
{
  FILE *in;
  char temp[16000];

  if (strcmp( file, "-" ) == 0)
    in = stdin;
  else
    in = fopen( file, "r" );

  while (fgets( temp, 16000, in ))
    {
      int n = strlen( temp );
      char *c;

      if (temp[n-1] == '\n')
	temp[--n] = 0;

      c = strchr( temp, ':' );
      if (c)
	{
	  *c++ = 0;
	  cmd_write( l, atoi( temp ), c );
	}
    }
}

static void ensure_empty( const char *file )
{
  struct stat sb;
  
  if ((stat( file, &sb ) == 0) && (sb.st_size != 0))
    {
      fprintf( stderr, 
	       "%s: I refuse to target an existing, non-empty file\n",
	       file );
      exit(1);
    }
}

static void display_help( void )
{
  printf( "usage: lssctl [-f] [-q] {opt} {open} {action}...   ; LSS i/o\n" );
  printf( "       lssctl [-f] [-q] {opt} {copy} SOURCE DEST   ; copy LSS\n" );
  printf( "       lssctl -P LSS        ; low-level dump, v3 LSS only\n" );
  printf( "where:\n" );
  printf( " {open} is one of:\n" );
  printf( "    -[r]o[b] LSS [TAG]     ; open [r=RO] [b=backlevel to TAG]\n" );
  printf( "    -c LSS                 ; create\n" );
  printf( "    -cx[b] LSS FROM [TAG]  ; create extension volume [b=backevel to TAG]\n" );
  printf( " {action} is one of:\n" );
  printf( "    -w RECNUM {text}       ; write record\n" );
  printf( "    -wa {text}             ; write next record\n" );
  printf( "    -la FILE               ; load from ascii (recnum:value lines)\n" );
  printf( "    -r RECNUM              ; read record\n" );
  printf( "    -z ZIPTYPE             ; set zip type\n" );
  printf( "    -L                     ; list record index\n" );
  printf( "    -.                     ; commit\n" );
  printf( " {text} is one of:\n" );
  printf( "    string                 ; literal string\n" );
  printf( "    {-|=}                  ; read from stdin (= --> chop NL)\n" );
  printf( "    <FILE                  ; input from file (escape the '<'!)\n" );
  printf( " {copy} is one of:\n" );
  printf( "    [-z ZIPTYPE] -cp       ; copy\n" );
  printf( "    -clone                 ; clone\n" );
  printf( "    [-O override] -clonev  ; clone volume\n" );
}

int main( int argc, const char **argv )
{
  LSS *l = NULL;
  int i;
  int force = 0;

  if (argc == 1)
    {
      display_help();
      return 1;
    }

  for (i=1; i<argc;)
    {
      if ((strcmp(argv[i],"-o") == 0) || (strcmp(argv[i],"-ro") == 0))
	{
	  l = lss_open( argv[i+1], 
			LSS_OPEN 
			| ((argv[i][1] == 'r') ? LSS_RDONLY : LSS_RDWR) );
	  if (!l)
	    {
	      plsserror( argv[i+1] );
	    }
	  i += 2;
	}
      else if ((strcmp(argv[i],"-ob") == 0) || (strcmp(argv[i],"-rob") == 0))
	{
	  int g = atoi( argv[i+2] );
	  l = lss_open( argv[i+1], 
			LSS_OPEN | LSS_BACKLEVEL
			| ((argv[i][1] == 'r') ? LSS_RDONLY : LSS_RDWR),
			g );
	  if (!l)
	    {
	      plsserror( argv[i+1] );
	    }
	  i += 3;
	}
      else if (strcmp(argv[i],"-c") == 0)
	{
	  if (!force)
	    ensure_empty( argv[i+1] );
	  l = lss_open( argv[i+1], LSS_CREATE, 0666 );
	  if (!l)
	    {
	      plsserror( argv[i+1] );
	    }
	  i += 2;
	}
      else if (strcmp(argv[i],"-cx") == 0)
	{
	  if (!force)
	    ensure_empty( argv[i+1] );
	  l = lss_open( argv[i+1], LSS_EXTEND, 0666, argv[i+2] );
	  if (!l)
	    {
	      plsserror( argv[i+1] );
	    }
	  i += 3;
	}
      else if (strcmp(argv[i],"-cxb") == 0)
	{
	  int g = atoi( argv[i+3] );

	  if (!force)
	    ensure_empty( argv[i+1] );
	  l = lss_open( argv[i+1], LSS_EXTEND|LSS_BACKLEVEL, 0666, argv[i+2], 
			g );
	  if (!l)
	    {
	      plsserror( argv[i+1] );
	    }
	  i += 4;
	}
      else if (strcmp(argv[i],"-.") == 0)
	{
	  assert( l );
	  printf( "commit-handle = %lu\n", lss_commit( l, 0 ) );
	  i++;
	}
      else if (strcmp(argv[i],"-P") == 0)
	{
	  low_level_dump( argv[i+1] );
	  i += 2;
	}
      else if (strcmp(argv[i],"-O") == 0)
	{
          override_ancestor = (char *)argv[i+1];
	  i += 2;
	}
      else if (strcmp(argv[i],"-L") == 0)
	{
	  list_index( l  );
	  i += 1;
	}
      else if (strcmp(argv[i],"-w") == 0)
	{
	  assert( l );
	  cmd_write( l, atoi( argv[i+1] ), argv[i+2] );
	  i += 3;
	}
      else if (strcmp(argv[i],"-wa") == 0) /* write-append */
	{
	  assert( l );
	  cmd_write_append( l, argv[i+1] );
	  i += 2;
	}
      else if (strcmp(argv[i],"-la") == 0) /* lines of recnum:value... */
	{
	  assert( l );
	  load_from_ascii( l, argv[i+1] );
	  i += 2;
	}
      else if (strcmp(argv[i],"-r") == 0)
	{
	  assert( l );
	  cmd_read( l, atoi( argv[i+1] ) );
	  i += 2;
	}
      else if (strcmp(argv[i],"-z") == 0)
	{
	  use_zip = lss_find_zip_algorithm( argv[i+1] );
	  if (!use_zip)
	    {
	      fprintf( stderr, "lss zip algorithm `%s' not defined\n",
		       argv[i+1] );
	      exit(1);
	    }
	  i += 2;
	}
      else if (strcmp(argv[i],"-cp") == 0)
	{
	  if (!force)
	    ensure_empty( argv[i+2] );
	  cmd_copy( argv[i+1], argv[i+2] );
	  i += 3;
	}
      else if (strcmp(argv[i],"-clone") == 0)
	{
	  if (!force)
	    ensure_empty( argv[i+2] );
	  cmd_clone( argv[i+1], argv[i+2] );
	  i += 3;
	}
      else if (strcmp(argv[i],"-clonev") == 0)
	{
	  if (!force)
	    ensure_empty( argv[i+2] );
	  cmd_clonev( argv[i+1], argv[i+2] );
	  i += 3;
	}
      else if (strcmp(argv[i],"-q") == 0)
	{
	  verbose = 0;
	  i++;
	}
      else if (strcmp(argv[i],"-vv") == 0)
	{
	  verbose = 2;
	  i++;
	}
      else if (strcmp(argv[i],"-v") == 0)
	{
	  verbose = 1; /* this is the default value, BTW */
	  i++;
	}
      else if (strcmp(argv[i],"-f") == 0)
	{
	  force = 1;
	  i++;
	}
      else if (strcmp(argv[i],"-h") == 0)
	{
	  display_help();
	  exit(0);
	}
      else
	{
	  fprintf( stderr, "bad option: `%s'\n", argv[i] );
	  exit(1);
	}
    }
  if (l)
    lss_close( l );

return 0;
}
