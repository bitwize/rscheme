/**********************************************
THIS FILE WAS AUTOMATICALLY COPIED FROM THE
RSCHEME SOURCE TREE, AND THE ORIGINAL MAY CHANGE.
HENCE, DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

#line 1 "modules/iolib/stdiox.c"
/*-----------------------------------------------------------------*-C-*---
 * File:    modules/iolib/stdiox.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.24
 * File mod date:    2006-04-07 10:17:15
 * System build:     v0.7.3.4-b7u, 2007-05-30
 * Owned by module:  iolib
 *
 * Purpose:          bytecode extensions to provide stdio interface
 *------------------------------------------------------------------------*/

#include <stdio.h>
#include <string.h>
#include <rscheme/scheme.h>
#include <rscheme/smemory.h>
#include <rscheme/bcextend.h>
#include <rscheme/osglue.h>
#include "iolib.h"
#include <rscheme/stdiox.h>
#if FCANGET_DO_SELECT
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#endif

/* 
 *  hack so I don't have to modify the unixy platform.h's
 */
#ifndef DOESNT_HAVE_POPEN
#define HAS_POPEN
#endif

static obj file_as_fx( FILE *f )
{
  if (f)
    return FILE_AS_FX(f);
  else
    return FALSE_OBJ;
}

obj rs_fgetln( FILE *strm );

/*
   The STDIO BCI Extension
*/

UINT_8 *bc_stdio_extension( UINT_8 *pc, RS_bc_datum **args )
{
  RS_bc_datum *bot = *args;
  FILE *f;
  size_t n;

  switch (*pc++)
    {
    case 0: 
      bot->obj_val = FILE_AS_FX( stdin );
      bot++;
      break;
    case 1: 
      bot->obj_val = FILE_AS_FX( stdout );
      bot++;
      break;
    case 2: 
      bot->obj_val = FILE_AS_FX( stderr );
      bot++;
      break;
    case 3: /* fopen */
      bot -= 2;
      f = os_fopen( bot[0].raw_str_val, bot[1].raw_str_val );
      if (f)
	bot->obj_val = FILE_AS_FX(f);
      else
	bot->obj_val = FALSE_OBJ;
      bot++;
      break;
    case 4: /* fclose */
      bot[-1].raw_int_val = fclose( FX_AS_FILE( bot[-1].obj_val ) );
      break;
    case 5: /* fwrite/str */
      bot -= 2;
      n = stdiox_fwrite_str( bot[0].obj_val, bot[1].obj_val );
      bot[0].raw_int_val = n;
      bot++;
      break;
    case 6: /* fwrite/dec */
      bot -= 2;
      fprintf( FX_AS_FILE(bot[0].obj_val), 
	       "%ld", 
	       (long)fx2int( bot[1].obj_val ) );
      bot[0].raw_int_val = 0;
      bot++;
      break;
    case 7: /* fflush */
      bot[-1].raw_int_val = fflush( FX_AS_FILE( bot[-1].obj_val ) );
      break;
    case 8: /* ftell */
      bot[-1].raw_int_val = ftell( FX_AS_FILE( bot[-1].obj_val ) );
      break;
    case 9: /* fseek */
      bot -= 2;
      bot[-1].raw_int_val = fseek( FX_AS_FILE( bot[-1].obj_val ),
				   bot[0].raw_int_val,
				   bot[1].raw_int_val );
      break;
    case 10: /* fread-fill */
      bot -= 3;
      bot[-1].raw_int_val = fread( (char *)PTR_TO_DATAPTR( bot[0].obj_val )
				   + bot[1].raw_int_val,
				   1, bot[2].raw_int_val,
				   FX_AS_FILE( bot[-1].obj_val ) );
      break;
    case 11: /* fputc */
      bot -= 2;
      fputc( GET_IMMEDIATE_VALUE(bot[1].obj_val),
	     FX_AS_FILE(bot[0].obj_val));
      break;
    case 12: /* fgetc */
      { int ch = fgetc( FX_AS_FILE(bot[-1].obj_val) );
	if (ch < 0)
	  bot[-1].obj_val = FALSE_OBJ;
	else
	  bot[-1].obj_val = MAKE_ASCII_CHAR(ch);
      }
      break;
    case 13: /* fgets */
      { char *s, temp[1024];
	s = fgets( temp, 1024, FX_AS_FILE(bot[-1].obj_val) );
	if (s)
	  bot[-1].obj_val = make_string(temp);
	else
	  bot[-1].obj_val = FALSE_OBJ;
      }
      break;
    case 14: /* popen */
      bot -= 2;
#ifdef HAS_POPEN
      f = (FILE *)popen( bot[0].raw_str_val, bot[1].raw_str_val );
      if (f)
	bot->obj_val = FILE_AS_FX(f);
      else
	bot->obj_val = FALSE_OBJ;
#else
      scheme_error( "popen: not implemented on this platform", 0 );
#endif /* HAS_POPEN */
      bot++;
      break;

    case 15: /* fgetln */
      bot[-1].obj_val = rs_fgetln( FX_AS_FILE(bot[-1].obj_val) );
      break;

    case 16: /* pclose */
#ifdef HAS_POPEN
      bot[-1].raw_int_val = pclose( FX_AS_FILE( bot[-1].obj_val ) );
#else
      scheme_error( "pclose: not implemented on this platform", 0 );
#endif
      break;


    case 17: /* ferror */
      bot[-1].raw_int_val = ferror( FX_AS_FILE( bot[-1].obj_val ) );
      break;

    case 18: /* clearerr */
      bot--;
      clearerr( FX_AS_FILE( bot[0].obj_val ) );
      break;

    case 19: /* feof */
      bot[-1].raw_bool_val = feof( FX_AS_FILE( bot[-1].obj_val ) ) ? YES : NO;
      break;

    case 20: /* fcanget */
      bot[-1].raw_int_val = stdiox_fcanget( bot[-1].obj_val );
      break;

    case 21: /* fpeekc */
      bot[-1].obj_val = stdiox_fpeekc( bot[-1].obj_val );
      break;
    }
  *args = bot;
  return pc;
}

obj stdiox_fpeekc( obj wf )
{
  FILE *f = FX_AS_FILE( wf );
  int ch = fgetc( f );

  if (ch < 0)
    {
      return FALSE_OBJ;
    }
  else
    {
      ungetc( ch, f );
      return MAKE_ASCII_CHAR( ch );
    }
}

#define QUICK_LEN  (1000)
#define EXTEND_LEN  (2000)

struct str_buf {
  char    *ptr, *limit, *base;
  UINT_32 length;
  obj     overflow;
  char    quick[QUICK_LEN];
};

static void str_buf_init( struct str_buf *b )
{
  b->ptr = b->base = b->quick;
  b->limit = b->base + QUICK_LEN;
  b->overflow = NIL_OBJ;
  b->length = 0;
}

static obj str_buf_flush( struct str_buf *b )
{
  UINT_32 N, len;
  obj result, over;
  char *dst;

  N = b->ptr - b->base;
  len = b->length + N;

  result = bvec_alloc( len+1, string_class );
  
  dst = ((char *)PTR_TO_DATAPTR(result)) + b->length;

  over = b->overflow;
  if (!EQ(over,NIL_OBJ))
    {
      do {
	memcpy( dst, (char *)PTR_TO_DATAPTR(pair_car(over)), N );

	over = pair_cdr(over);
	dst -= EXTEND_LEN;
	N = EXTEND_LEN;
      } while (!EQ(over,NIL_OBJ));
      N = QUICK_LEN;
      dst = ((char *)PTR_TO_DATAPTR(result));
    }
  memcpy( dst, b->quick, N );
  return result;
}

static void str_buf_addch( struct str_buf *b, char ch )
{
  if (b->ptr >= b->limit)
    {
      obj chunk;

      /* flush this buffer and add another */
      b->length += b->limit - b->base;
      chunk = bvec_alloc( EXTEND_LEN, byte_vector_class );
      b->base = b->ptr = (char *)PTR_TO_DATAPTR(chunk);
      b->limit = b->base + EXTEND_LEN;
      b->overflow = cons( chunk, b->overflow );
    }
  *(b->ptr)++ = ch;
}

#if HAVE_FGETLN /* BDS 4.4 and later? (Rhapsody, at least) */

obj rs_fgetln( FILE *strm )
{

  size_t n;
  char *p;

  p = fgetln( strm, &n );
  if (p)
    {
      obj str;

      if (p[n-1] == '\n') /* strip trailing newline, if present */
	n--;

      str = bvec_alloc( n+1, string_class ); /* sets str[n] = 0 */
      memcpy( (char *)PTR_TO_DATAPTR(str), p, n );
      return str;
    }
  else
    {
      clearerr( strm );
      return FALSE_OBJ;
    }
}

#else /* !HAVE_FGETLN */

/* an implementation on top of stdio */

obj rs_fgetln( FILE *strm )
{
  struct str_buf B;
  int ch;

  ch = fgetc( strm );
  if (ch == EOF)
    {
      clearerr( strm );
      return FALSE_OBJ;
    }
  else if (ch == '\n')
    {
      return bvec_alloc( 1, string_class );
    }
  else
    {
      str_buf_init( &B );
      do {
	str_buf_addch( &B, ch );
	ch = fgetc( strm );
      } while (ch != EOF && ch != '\n');
      if (ch == EOF)
	clearerr( strm );
      return str_buf_flush( &B );
    }
}
#endif /* HAVE_FGETLN */


obj stdiox_fopen( char *path, char *mode )
{
  return file_as_fx( os_fopen( path, mode ) );
}

obj stdiox_popen( char *proc, char *mode )
{
#ifdef HAS_POPEN
  return file_as_fx( (FILE *)popen( proc, mode ) );
#else
  scheme_error( "popen: not implemented on this platform", 0 );
  return FALSE_OBJ;
#endif /* HAS_POPEN */
}

int stdiox_pclose( obj f )
{
#ifdef HAS_POPEN
  return pclose( FX_AS_FILE( f ) );
#else
  scheme_error( "pclose: not implemented on this platform", 0 );
  return -1;
#endif
}

int stdiox_fwrite_str( obj f, obj str )
{
  ensure_string_mapped( str );
  return fwrite( string_text(str), 1, string_length(str), FX_AS_FILE(f) );
}

obj stdiox_fgetc( obj f )
{
  int ch = fgetc( FX_AS_FILE(f) );

  if (ch < 0)
    return FALSE_OBJ;
  else
    return MAKE_ASCII_CHAR(ch);
}

obj stdiox_fgets( obj f )
{
  char *s, temp[1024];

  s = fgets( temp, 1024, FX_AS_FILE(f) );
  if (s)
    return make_string(temp);
  else
    return FALSE_OBJ;
}

obj stdiox_fgetln( obj f )
{
  return rs_fgetln( FX_AS_FILE(f) );
}

/*
 *  return an approximation of the number of bytes we can read
 *  from the stream without blocking.  Used to implement
 *  `char-ready?'.
 *
 *  Unfortunately, as you can tell from the #ifdefs, there is
 *  very little support for this kind of functionality in stdio.
 *  Hence, we build in knowledge of how some systems work, and
 *  for the rest, we may return 0 when some characters really are
 *  ready. (causing `char-ready?' to return #f, BTW)
 */

int stdiox_fcanget( obj f )
{
  FILE *strm = FX_AS_FILE(f);
  int in_buf;

#if FCANGET_USE_IO_READ_PTR
  /* GNU libc - linux, hurd? */
  in_buf = (strm->_IO_read_end) - (strm->_IO_read_ptr);
#else
#if FCANGET_USE_CNT
  /* AIX and many others */
  in_buf = strm->_cnt;
#else
#if FCANGET_USE_R
  /* BSD */
  in_buf = strm->_r;
#else
  /* default to 0 */
  in_buf = 0;
#endif
#endif
#endif

#if FCANGET_DO_SELECT
  if (in_buf == 0)
    {
      /* use select() to see if anything is ready in the kernel */
      fd_set r, w, x;
      int n, fd;
      struct timeval nodelay;
      
      nodelay.tv_sec = 0;
      nodelay.tv_usec = 0;

      fd = fileno( strm );

      FD_ZERO( &r );
      FD_SET( fd, &r );

      n = select( fd+1, &r, NULL, NULL, &nodelay );
      if (n > 0)
	in_buf = 1;  /* at least one char to be read */
    }
#endif
  return in_buf;
}
