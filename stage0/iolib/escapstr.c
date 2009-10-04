/**********************************************
THIS FILE WAS AUTOMATICALLY COPIED FROM THE
RSCHEME SOURCE TREE, AND THE ORIGINAL MAY CHANGE.
HENCE, DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

#line 1 "modules/iolib/escapstr.c"
/*-----------------------------------------------------------------*-C-*---
 * File:    modules/iolib/escapstr.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.13
 * File mod date:    1998-12-07 03:04:09
 * System build:     v0.7.3.4-b7u, 2007-05-30
 * Owned by module:  iolib
 *
 * Purpose:          quickly escape strings as for `write'
 *------------------------------------------------------------------------*/

#include <rscheme/scheme.h>
#include <rscheme/vinsns.h>
#include <ctype.h>
#include <string.h>
#include "cports.h"

#if UTF_PROCESS_CODE
#define OR_HIGH_BIT_IF_UTF || (*(UINT_8 *)s >= 0x80)
#else
#define OR_HIGH_BIT_IF_UTF /* empty */
#endif

#define TEMP_LENGTH (200)

/* stores it's results directly into REG0 and REG1 */
/* (called from writers.scm, string->printable) */

void printablize_string( obj str, obj str_index )
{
char ec, *d, *limit, temp[TEMP_LENGTH+6];
const char *s0, *s, *slimit;

    s0 = s = string_text(str) + fx2int(str_index);
    slimit = string_text(str) + string_length(str);

    d = temp;
    limit = d + TEMP_LENGTH;
    while (s<slimit && d < limit)
    {
	switch (*s)
	{
	    case '\"':  ec = '\"'; goto use_escaped;
	    case '\\':  ec = '\\'; goto use_escaped;
	    case '\n':  ec = 'n'; goto use_escaped;
	    case '\t':  ec = 't'; goto use_escaped;
	    case  7:    ec = 'a'; goto use_escaped;
	    case  8:    ec = 'b'; goto use_escaped;
	    case 11:    ec = 'v'; goto use_escaped;
	    case 12:    ec = 'f'; goto use_escaped;
	    case 13:    ec = 'r'; goto use_escaped;
	    use_escaped:
			*d++ = '\\';
			*d++ = ec;
			break;
	    default:    
			if (isprint(*(unsigned char *)s) OR_HIGH_BIT_IF_UTF)
			    *d++ = *s;
			else
			  {
#if SPRINTF_RETURNS_INT
			    d += sprintf( d, "\\%o", *(UINT_8 *)s );
#else
			    sprintf( d, "\\%o", *(UINT_8 *)s );
			    d += strlen(d);
#endif
			  }
			break;
	}
	s++;
    }
    *d++ = 0;
    if (s < slimit)
    {
      REG0 = bvec_alloc( d - temp, string_class );
      memcpy( PTR_TO_DATAPTR(REG0), temp, d - temp );
      REG1 = FX_ADD(str_index,int2fx(s-s0));
    }
    else if (EQ(str_index,ZERO) && ((d - temp) == SIZEOF_PTR(str)))
    {
      REG0 = str;
      REG1 = FALSE_OBJ;
    }
    else
    {
      REG0 = bvec_alloc( d - temp, string_class );
      memcpy( PTR_TO_DATAPTR(REG0), temp, d - temp );
      REG1 = FALSE_OBJ;
    }
}
