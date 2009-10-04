/**********************************************
THIS FILE WAS AUTOMATICALLY COPIED FROM THE
RSCHEME SOURCE TREE, AND THE ORIGINAL MAY CHANGE.
HENCE, DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

#line 1 "modules/iolib/op_bstr.c"
/*-----------------------------------------------------------------*-C-*---
 * File:    modules/iolib/op_bstr.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.7
 * File mod date:    2003-03-03 01:20:23
 * System build:     v0.7.3.4-b7u, 2007-05-30
 * Owned by module:  iolib
 *
 * Purpose:          <bounded-string-output-port> low-level implementation
 *------------------------------------------------------------------------*/

#include <rscheme/scheme.h>
#include <rscheme/smemory.h>
#include <stdio.h>
#include "cports.h"

obj BSOP_flush( obj port, int closeq )
{
const char *src;
int len;
obj result;

    src = string_text( gvec_read( port, BSOP_BUFFER ) );
    len = fx2int( gvec_read( port, BSOP_INDEX ) );

    result = bvec_alloc( len+1, string_class );
    memcpy( PTR_TO_DATAPTR(result), src, len );
    /*
        We don't need to set the last byte to NUL because
	bvec_alloc sets the whole last UINT_32 to 0,
	even if (len+1) is a multiple of 4 bytes.
	
        PTR_TO_DATAPTR(result)[len] = NUL;
    */
    if (closeq) {
      gvec_write( port, BSOP_BUFFER, FALSE_OBJ );
    }
    
    return result;
}

rs_bool BSOP_write( obj port, const char *src, UINT_32 len )
{
obj buf, fxpos;
char *ptr;
UINT_32 n, max, pos;

    buf = gvec_read( port, BSOP_BUFFER );
    fxpos = gvec_read( port, BSOP_INDEX );
    max = string_length(buf);

    assert( STRING_P(buf) );
    assert( OBJ_ISA_FIXNUM(fxpos) );

    pos = fx2int(fxpos);
    
    ptr = (char *)string_text(buf);

    if (pos + len > max)
    {
	n = max - pos;
	memcpy( ptr + pos, src, n );
	gvec_write_non_ptr( port, BSOP_INDEX, int2fx(max) );
	return NO;
    }

    memcpy( ptr + pos, src, len );
    pos += len;
    gvec_write_non_ptr( port, BSOP_INDEX, int2fx(pos) );
    return YES;
}

