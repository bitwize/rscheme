/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/_debug.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.15
 * File mod date:    2003-06-12 21:53:54
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          Low-level (C) debugging hooks
 *------------------------------------------------------------------------*/

#include <rscheme/runtime.h>
#include <rscheme/hashmain.h>
#include <setjmp.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include <stdarg.h>
#include <rscheme/scheme.h>
#include <rscheme/smemory.h>

/*
    a BoundedStringPort is a device used for writing structures to
    a string that only has so much space.  The jmp_buf "escape"
    is the C equivalent to an exit continuation that would be used
    in implementing "with-output-to-truncated-string", because we
    have to actually ABORT any write operations that try to make
    it to big (you can't just ignore them, because you may be trying
    to a write a circular structure, or something like that)
    
    Eventually, [almost] everything in this file should be brought up
    to the Scheme level.
*/

typedef struct _BoundedStringPort {
    char	*ptr;
    char	*limit;
    char	*base;
    jmp_buf	escape;
} BSP;

static void pdisplay( BSP *dest, obj item );

static void BSPWriteChar( BSP *port, char ch )
{
    if (port->ptr >= port->limit)
	longjmp( port->escape, 1 );
    *(port->ptr)++ = ch;
}

static void BSPWriteStr( BSP *port, const char *str )
{
char *dest = port->ptr;
char *limit = port->limit;

    while (*(UINT_8 *)str)
    {
	if (dest >= limit)
	    longjmp( port->escape, 2 );
	*dest++ = *str++;
    }
    port->ptr = dest;
}

static void BSPWriteInt( BSP *port, int value )
{
char temp[20];

    sprintf( temp, "%d", value );
    BSPWriteStr( port, temp );
}

static void BSPWriteHex( BSP *port, unsigned value )
{
char temp[20];

    sprintf( temp, "%x", value );
    BSPWriteStr( port, temp );
}

static void BSPWritev( BSP *port, const char *fmt, va_list a )
{
    while (*fmt)
    {
	if (*fmt == '%')
	{
	    switch (*++fmt)
	    {
		case 's':	BSPWriteStr( port, va_arg( a, char * ) );
				break;
		case 'c':	BSPWriteChar( port, va_arg( a, int ) );
				break;
		case 'd':	BSPWriteInt( port, va_arg( a, int ) );
				break;
		case 'x':	BSPWriteHex( port, va_arg( a, unsigned ) );
				break;
		case 'p':	BSPWriteStr( port, "0x" );
				BSPWriteHex( port, va_arg( a, unsigned ) );
				break;
		case 'o':	pdisplay( port, va_arg( a, obj ) );
				break;
		default:
				BSPWriteChar( port, *fmt );
				break;
	    }
	    fmt++;
	}
	else
	    BSPWriteChar( port, *fmt++ );
    }
}

static void BSPWrite( BSP *port, const char *fmt, ... )
{
va_list a;

    va_start( a, fmt );
    BSPWritev( port, fmt, a );
    va_end( a );
}

static void BSPInit( BSP *port, char *buffer, int size )
{
    port->base = port->ptr = buffer;
    port->limit = buffer + size - 1;
}

static char *BSPClose( BSP *port )
{
    *port->ptr = 0;
    return port->base;
}

static void fixnum_display( BSP *dest, obj item )
{
    BSPWriteInt( dest, FIXNUM_TO_RAWINT(item) );
}

static void vector_display( BSP *dest, obj item )
{
  unsigned i, n;

  BSPWriteStr( dest, "#(" );
  n = SIZEOF_PTR(item) / SLOT(1);
  i = 0;
  
  for (i=0; i<n; i++)
    {
      if (i > 0)
	BSPWriteChar( dest, ' ' );
      pdisplay( dest, gvec_read( item, SLOT(i) ) );
    }
  BSPWriteStr( dest, ")" );
}

static void ascii_char_display( BSP *dest, obj item )
{
unsigned char ch = GET_IMMEDIATE_VALUE(item);

    if (isgraph(ch))
	BSPWrite( dest, "#\\%c", ch );
    else if (ch == '\n')
	BSPWrite( dest, "#\\newline" );
    else if (ch == '\t')
	BSPWrite( dest, "#\\tab" );
    else if (ch == ' ')
	BSPWrite( dest, "#\\space" );
    else
	BSPWrite( dest, "#/%d", ch );
}


static void class_display( BSP *dest, obj the_class )
{
    BSPWrite( dest, "#[<<Class>> %s", 
	symbol_text(class_name(the_class)) );

    if (!class_is_gvec(the_class))
	BSPWriteStr( dest, " (bvec)" );
    BSPWriteStr( dest, "]" );
}

static enum secondary_tag get_secondary_tag( obj item )
{
    return (enum secondary_tag)
    		((VAL(item) & SECONDARY_TAG_MASK) >> PRIMARY_TAG_SIZE);
}

static void BSPWriteTemplateName( BSP *dest, obj tmpl )
{
  obj l;

  l = template_scope( tmpl );

  if (!PAIR_P(l)) {
    BSPWriteStr( dest, " ???" );
  }

  while (PAIR_P( l )) {
    BSPWriteChar( dest, ' ' );
    pdisplay( dest, pair_car( l ) );
    l = pair_cdr( l );
  }
}

static void pdisplay( BSP *dest, obj item )
{
    if (OBJ_ISA_FIXNUM(item))
	fixnum_display( dest, item );
    else if (OBJ_ISA_IMMOB(item))
    {
	switch (get_secondary_tag(item))
	{
	    case BOOLEAN_TAG:
				BSPWriteStr( dest, 
					     EQ(item,FALSE_OBJ) 
						    ? "#f" 
						    : "#t" );
				break;
	    case NIL_TAG:
				BSPWriteStr( dest, "()" );
				break;
	    case ASCII_CHAR_TAG:
				ascii_char_display( dest, item );
				break;
	    case UNICODE_CHAR_TAG:
				BSPWrite( dest, 
					  "#\\U%d", 
					  GET_IMMEDIATE_VALUE(item) );
				break;
	    case UNIQUE_OBJ_TAG:
	     switch (GET_IMMEDIATE_VALUE(item))
	       {
	       case UNDEFINED_TAG:
		 BSPWriteStr( dest, "#undef" );
		 break;
	       case UNINITIALIZED_TAG:
		 BSPWriteStr( dest, "#uninit" );
		 break;
	       case NOVALUE_TAG:
		 BSPWriteStr( dest, "#none" );
		 break;
	       case UNBOUND_TAG:
		 BSPWriteStr( dest, "#unbound" );
		 break;
	       default:
		 BSPWrite( dest, "#[huh? %d.%d.%d]", 
			  GET_IMMEDIATE_VALUE(item),
			  get_secondary_tag(item),
			  VAL(item) & PRIMARY_TAG_MASK );	 
	       }
		break;
	    default:
              BSPWrite( dest, "#[huh? %d.%d.%d]", 
                        GET_IMMEDIATE_VALUE(item),
                        get_secondary_tag(item),
                        VAL(item) & PRIMARY_TAG_MASK );
	}
    }
    else if (OBJ_ISA_PTR(item))
    {
	assert( CLASS_P(CLASSOF_PTR(item)) );
	if (PAIR_P(item))
	{
	    BSPWriteStr( dest, "(" );
	    pdisplay( dest, pair_car(item) );
	    while (PAIR_P(pair_cdr(item)))
	    {
		BSPWriteChar( dest, ' ' );
		item = pair_cdr(item);
		pdisplay( dest, pair_car(item) );
	    }
	    if (!NULL_P(pair_cdr(item)))
	    {
		BSPWriteStr( dest, " . " );
		pdisplay( dest, pair_cdr(item) );
	    }
	    BSPWriteStr( dest, ")" );
	}
	else if (SYMBOL_P(item))
	{
	    BSPWriteStr( dest, symbol_text(item) );
	}
	else if (STRING_P(item))
	{
	const char *s = string_text(item);
	
            BSPWriteChar( dest, '\"' );    /* a " */
	    while (*s)
	    {
		if (*s == '\n')
		    BSPWriteStr( dest, "\\n" );
		else if (*s == '\t')
		    BSPWriteStr( dest, "\\t" );
		else
		    BSPWriteChar( dest, *s );
		s++;
	    }
	    BSPWriteChar( dest, '\"' );         /* a close " */
	}
	else if (FUNCTION_P(item))
	{
	    BSPWriteStr( dest, "#[<Closure>" );
            BSPWriteTemplateName( dest, gvec_read( item, SLOT(0) ) );
            BSPWriteStr( dest, "]" );
	}
	else if (TEMPLATE_P(item))
	{
	    BSPWriteStr( dest, "#[<Template>" );
            BSPWriteTemplateName( dest, item );
            BSPWriteStr( dest, "]" );
	}
	else if (LONGFLOAT_P(item))
	{
	char temp[50];
	
	    sprintf( temp, "%g", extract_float(item) );
	    BSPWriteStr( dest, temp );
	}
	else if (VECTOR_P(item))
	{
	    vector_display( dest, item );
	}
#if 0
	else if (HASHTABLE_P(item))
	{
	    hash_table_display( dest, item );
	}
#endif
	else if (CLASS_P(item))
	{
	    class_display( dest, item );
	}
	else
	{
	int i, n;
	obj c = object_class(item);
	
	    n = SIZEOF_PTR(item) / sizeof(obj);
	    BSPWrite( dest, "#[%s", symbol_text(class_name(c)) );
	    if (EQ(gvec_read(c,SLOT(1)),FALSE_OBJ))
	    {
		for (i=0; i<n; i++)
		{
		    BSPWriteChar( dest, ' ' );
		    pdisplay( dest, gvec_read(item,SLOT(i)) );
		}
	    }
	    else
		BSPWrite( dest, " %d bytes", (unsigned)SIZEOF_PTR(item) );
	    BSPWriteStr( dest, "]" );
	}
    }
    else
	BSPWrite( dest, "#[unknown:%x.%x]", 
			VAL(item) >> PRIMARY_TAG_SIZE,
			VAL(item) & PRIMARY_TAG_MASK );
}

int snprinto( char *buf, obj item, unsigned n )
{
  BSP port;
  char *volatile p = buf;

  BSPInit( &port, p, n-4 );
  if (!setjmp(port.escape)) {
    pdisplay( &port, item );
    BSPClose( &port );
    return port.ptr - port.base;
  } else {
    *port.ptr++ = '.';
    *port.ptr++ = '.';
    *port.ptr++ = '.';
    BSPClose( &port );
    return port.ptr - port.base;
  }
}

void fnprinto( FILE *dest, obj item, unsigned n )
{
BSP port;
char *volatile buffer;
char buff[1000];

    if (n > 1000)
      n = 1000;

    buffer = buff;

    BSPInit( &port, buffer, n-4 );
    if (!setjmp(port.escape))
    {
	pdisplay( &port, item );
	fputs( BSPClose( &port ), dest );
    }
    else
    {
        fputs( BSPClose(&port), dest );
	fputs( "...", dest );
    }
}

void fprinto( FILE *dest, obj item )
{
  fnprinto( dest, item, 1000 );
}

void debug( obj item )
{
    printf( "debug ::= " );
    fnprinto( stdout, item, 200 );
    putchar( '\n' );
}

static void slot_line( FILE *f, obj t )
{
  if (OBJ_ISA_PTR(t))
    {
      fprintf( f, "<%#lx> = ", VAL(t) );
    }
  fprinto( f, t );
  fputc( '\n', f );
}

void fdebug_slots( FILE *f, obj item )
{
  fprintf( f, "-- <%#lx>. --\n ==> ", VAL(item) );
  fprinto( f, item );
  fprintf( f, "\n" );
  if (GVEC_P(item))
    {
      UINT_32 k, len;

      len = SIZEOF_PTR(item) / SLOT(1);
      
      fprintf( f, " class = " );
      slot_line( f, CLASSOF_PTR(item) );
      
      for (k=0; k<len; k++)
	{
	  obj t;

	  fprintf( f, "   [%d] = ", (unsigned)k );
	  slot_line( f, gvec_ref( item, SLOT(k) ) );
	}
      fputc( '\n', f );
    }
}

void debug_slots( obj item )
{
  fdebug_slots( stdout, item );
}

