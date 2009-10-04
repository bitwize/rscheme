/**********************************************
THIS FILE WAS AUTOMATICALLY COPIED FROM THE
RSCHEME SOURCE TREE, AND THE ORIGINAL MAY CHANGE.
HENCE, DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

#line 1 "modules/iolib/op_str.c"
/*-----------------------------------------------------------------*-C-*---
 * File:    modules/iolib/op_str.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.8
 * File mod date:    2003-03-03 01:20:25
 * System build:     v0.7.3.4-b7u, 2007-05-30
 * Owned by module:  iolib
 *
 * Purpose:          <string-output-port> low-level implementation
 *------------------------------------------------------------------------*/

#include <rscheme/scheme.h>
#include <rscheme/smemory.h>
#include <stdio.h>
#include "cports.h"

/***********************************************************************
 *
 *    A string output port is structured as a buffer
 *    (a <byte-vector>), plus a chain of overflow
 *    buffers.  The overflow chain is structured as a list
 *    in REVERSE order.  All the buffers in the overflow
 *    list are completely full.
 *
 *
 *  Having written out:
 *
 *     The quick brown fox
 *
 *  a string-output-port might look something like:
 *
 *
 *	<string-output-port>
 *     +------------------------+
 *     | current-buffer       *-+-----> [ b r o w n   f o x . . . . ]
 *     +------------------------+	 0 1 2 3 4 5 6 7 8 9
 *     | current-buffer-index  9|
 *     +------------------------+
 *     | buffer-overflows     *-+---> ( [uick ] [The q] )
 *     +------------------------+
 *
 *
 *  SOP_BLOCK_SIZE specifies the minimum size of an output buffer.
 *  In general, buffers will only be larger when a large write takes
 *  place.
 ***********************************************************************/

#define SOP_BLOCK_SIZE (128-16)

obj SOP_flush( obj port, int closeq )
{
  int len;
  obj dst, overflow;
  char *endptr;
  const char *src;
  
  len = fx2int( gvec_read( port, SOP_INDEX ) );
  overflow = gvec_read( port, SOP_OVERFLOW );
  
  while (!EQ( overflow, NIL_OBJ ))
    {
      len += SIZEOF_PTR( pair_car( overflow ) );
      overflow = pair_cdr( overflow );
    }
  
  dst = bvec_alloc( len+1, string_class );
  endptr = ((char *)string_text( dst )) + len;
  *endptr = 0;
  
  src = (const char *)PTR_TO_DATAPTR( gvec_read( port, SOP_BUFFER ) );
  len = fx2int( gvec_read( port, SOP_INDEX ) );
  overflow = gvec_read( port, SOP_OVERFLOW );
  
  while (1)
    {
      endptr -= len;
      memcpy( endptr, src, len );
      if (EQ( overflow, NIL_OBJ ))
	break;
      
      src = (const char *)PTR_TO_DATAPTR( pair_car( overflow ) );
      len = SIZEOF_PTR( pair_car( overflow ) );
      overflow = pair_cdr( overflow );
    }
  if (closeq) {
    gvec_write( port, SOP_BUFFER, FALSE_OBJ );
    gvec_write( port, SOP_OVERFLOW, FALSE_OBJ );
  }
  return dst;
}

void SOP_write( obj port, const char *src, UINT_32 len )
{
  obj buf, fxpos;
  char *ptr;
  UINT_32 n, max, pos;

  buf = gvec_read( port, SOP_BUFFER );
  fxpos = gvec_read( port, SOP_INDEX );

  assert( BYTE_VECTOR_P( buf ) );
  assert( OBJ_ISA_FIXNUM( fxpos ) );

  max = SIZEOF_PTR( buf );
  pos = fx2int( fxpos );

  ptr = (char *)PTR_TO_DATAPTR( buf );

  if (pos + len >= max)
    {
      UINT_32 newbuflen;

      /*  if this write does not fit entirely within the current
       *  buffer, then we need to fill out this buffer and
       *  push it on the overflow list.
       */

      n = max - pos;
      memcpy( ptr + pos, src, n );
      src += n;
      len -= n;
      gvec_write( port,
		  SOP_OVERFLOW,
		  cons( buf, gvec_read( port, SOP_OVERFLOW ) ) );

      /*  Now we need to allocate another buffer, do the
       *  rest of the write in there, and leave it current.
       *
       *  We allocate the new buffer at least big enough to hold
       *  what's needed.
       */
      if (len > SOP_BLOCK_SIZE)
	newbuflen = len + SOP_BLOCK_SIZE;
      else
	newbuflen = SOP_BLOCK_SIZE;

      buf = alloc( newbuflen, byte_vector_class );
      gvec_write( port, SOP_BUFFER, buf );
      pos = 0;
      ptr = (char *)PTR_TO_DATAPTR( buf );
    }

  memcpy( ptr + pos, src, len );
  pos += len;

  gvec_write_non_ptr( port, SOP_INDEX, int2fx( pos ) );
}
