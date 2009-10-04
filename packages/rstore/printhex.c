#include <stdio.h>
#include <ctype.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <setjmp.h>
#include <errno.h>

#include "rstoret.h"

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

void rstore_print_hex_data( UINT_8 *addr, unsigned len )
{
  linit( 79 );
  if (setjmp( line_full ) == 0)
    {
      lprint_hex_data( addr, len );
    }
  fwrite( line_base, 1, line_ptr - line_base, stdout );
}
