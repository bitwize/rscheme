/**********************************************
THIS FILE WAS AUTOMATICALLY COPIED FROM THE
RSCHEME SOURCE TREE, AND THE ORIGINAL MAY CHANGE.
HENCE, DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

#line 1 "modules/imageio/refsload.c"
/*-----------------------------------------------------------------*-C-*---
 * File:    modules/imageio/refsload.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.6
 * File mod date:    1997-11-29 23:10:51
 * System build:     v0.7.3.4-b7u, 2007-05-30
 * Owned by module:  imageio
 *
 *------------------------------------------------------------------------*/

#include <string.h>
#include <rscheme.h>
#include <rscheme/scheme.h>
#include <rscheme/hashmain.h>
#include <rscheme/allocns.h>
#include <rscheme/linktype.h>

#include "imaglue.h"

static UINT_16 get_16( UINT_8 **s )
{
  UINT_16 x;
  x = ((*s)[0] << 8)
      + ((*s)[1]);
  (*s) += 2;
  return x;
}

static UINT_32 get_32( UINT_8 **s )
{
  UINT_32 x;
  x = ((*s)[0] << 24)
      + ((*s)[1] << 16)
      + ((*s)[2] << 8)
      + ((*s)[3]);
  (*s) += 4;
  return x;
}

#define GET_8(s) (*s++)
/*
#define GET_16(s) (s += 2, (s[-2] << 8) + s[-1])
#define GET_32(s) (s += 4, (s[-4] << 24) + (s[-3] << 16) + (s[-2] << 8) + s[-1])
*/
#define GET_16(x) get_16(&x)
#define GET_32(x) get_32(&x)

static UINT_8 *intern_module_refs( UINT_8 *src, obj **dst );
static UINT_8 *intern_symbol_refs( UINT_8 *src, obj **dst );
static UINT_8 *intern_class_refs( UINT_8 *src, obj **dst, obj ct, 
				  obj *misses );
static UINT_8 *extern_module_refs( UINT_8 *src, obj **dstp, 
				   obj fd_anchor_class, 
				   obj cp_anchor_class );
static UINT_8 *intern_dict_classes( UINT_8 *src, obj **dstp, 
				    obj ct, obj cnamev, obj *misses );
static UINT_8 *intern_dict_symbols( UINT_8 *src, obj **dstp, obj namev );

/*
 *   internalize a reference vector prior to image loading
 */

obj parse_refs( obj class_table,
	        UINT_8 **srcp, unsigned src_cnt,
	        rs_bool do_link,
	        obj fdac, obj cpac, 
	        obj class_name_vec,
	        obj symbol_vec,
	        obj *miss_list )
{
  obj *dst_start, *dst, result;
  UINT_8 *src = *srcp, *limit = *srcp + src_cnt;
  UINT_16 num_refs;

  num_refs = GET_16(src);

  result = alloc( SLOT(num_refs), vector_class );
  dst = (obj *)PTR_TO_DATAPTR(result);
  dst_start = dst;

  while (1)
    {
      assert (src < limit);
#ifdef DEBUG_IMAGEIO
      printf( "parse_refs: class '%c'\n", *src );
#endif /* DEBUG_IMAGEIO */
      switch (*src++)
	{
	case 'm':  /* module refs */
	  if (do_link)
	    src = intern_module_refs( src, &dst );
	  else
	    src = extern_module_refs( src, &dst, fdac, cpac );
	  break;

	case 'S':  /* dict symbols */
	  src = intern_dict_symbols( src, &dst, symbol_vec );
	  break;

	case 's':  /* symbols */
	  src = intern_symbol_refs( src, &dst );
	  break;

	case 'C':  /* dict classes */
	  src = intern_dict_classes( src, &dst, class_table, class_name_vec,
				     miss_list );
	  break;

	case 'c':  /* classes */
	  src = intern_class_refs( src, &dst, class_table, miss_list );
	  break;
	  
	case '!':  /* stop */
	  *srcp = src;
	  if ((dst - dst_start) != num_refs)
	    {
	      scheme_error("parse-refs: failed to parse ~d refs (parsed ~d)",
			   2, int2fx( num_refs ), int2fx(dst-dst_start) );
	    }
	  return result;

	default:
	  assert(0);
	}
    }
}

/*  */


static UINT_8 *intern_module_refs( UINT_8 *src, obj **dstp )
{
UINT_8 flag, k, len;
struct module_descr *m;
struct part_descr **expect_part, *part;
UINT_16 num_parts, i;
UINT_32 part_tag;
obj *dst = *dstp;

  while ((len = GET_8(src)))
    {
      m = find_module( (char *)src );
#ifdef DEBUG_IMAGEIO
      printf( "   intern module '%s'\n", src );
#endif /* DEBUG_IMAGEIO */
      src += len+1;
      num_parts = GET_16(src);

      expect_part = m->parts;
      for (i=0; i<num_parts; i++)
	{
	  part_tag = GET_32(src);
#ifdef DEBUG_IMAGEIO
	  printf( "      part %u\n", part_tag );
#endif /* DEBUG_IMAGEIO */
	  if (!*expect_part || (*expect_part)->tag != part_tag)
	    {
	      for (expect_part=m->parts; *expect_part; expect_part++) 
		{
		  if ((*expect_part)->tag == part_tag)
		    break;
		}
	      if (!*expect_part)
		scheme_error( "module ~a has no part with tag ~d",
			      2, make_string(m->name), int2fx(part_tag) );
	    }
	  part = *expect_part++;
	  flag = GET_8(src);
	  if (flag == 255) {
	    /* non-compact */
	    UINT_8 m;
	    
	    flag = GET_8(src);
	    while (flag > 0) {
	      flag--;
	      k = GET_8(src);
	      *dst++ = RAW_PTR_TO_OBJ( part->functions[k] );
	      m = GET_8(src);
	      while (m > 0) {
		UINT_16 x = GET_16(src);
		m--;
		*dst++ = JUMP_ADDR_TO_OBJ( part->functions[k]->monotones[x] );
	      }
	    }
	  } else if (flag == 254) {
	    /* semi-compact */
	    flag = GET_8(src);
	    while (flag > 0) {
	      flag--;
	      k = GET_8(src);
	      *dst++ = RAW_PTR_TO_OBJ( part->functions[k] );
	      *dst++ = JUMP_ADDR_TO_OBJ( part->functions[k]->monotones[0] );
	    }
	  } else {
	    /* compact */
	    for (k=0; k<flag; k++) {
	      assert (part->functions[k]);
	      *dst++ = RAW_PTR_TO_OBJ( part->functions[k] );
	      *dst++ = JUMP_ADDR_TO_OBJ( part->functions[k]->monotones[0] );
	    }
	  }
	}
    }
    *dstp = dst;
    return src;
}

static obj make_strdup( UINT_8 *src, UINT_32 len )
{
  obj r;
  r = bvec_alloc( len + 1, string_class );
  memcpy( PTR_TO_DATAPTR(r), src, len );
  return r;
}

static UINT_8 *intern_symbol_refs( UINT_8 *src, obj **dstp )
{
  obj *dst = *dstp;
  UINT_8 len;

  while ((len = GET_8(src)))
    {
#ifdef DEBUG_IMAGEIO
      printf( "   symbol of %u bytes:", len );
#endif /* DEBUG_IMAGEIO */
      *dst++ = intern( make_strdup( src, len ) );
#ifdef DEBUG_IMAGEIO
      printf( " %s\n", symbol_text( dst[-1] ) );
#endif /* DEBUG_IMAGEIO */
      src += len;
    }
  *dstp = dst;
  return src;
}

static UINT_8 *intern_class_refs( UINT_8 *src, obj **dstp, obj ct, 
				  obj *missed )
{
  obj *dst = *dstp;
  UINT_8 flag, len;
  obj cname;

  while ((len = GET_8(src)))
    {
      cname = intern( make_strdup( src, len ) );
      src += len;

      *dst = objecttable_lookup( ct, symbol_hash(cname), cname );
      if (EQ(*dst,FALSE_OBJ))
	{
	  *missed = cons( cname, *missed );
	  *dst = vector_class;
	}

#ifdef DEBUG_IMAGEIO
      printf( "   class name of %u bytes: %s => %#x\n",
	      len, symbol_text(cname), *dst );
#endif /* DEBUG_IMAGEIO */
      dst++;
    }
  *dstp = dst;
  return src;
}

static UINT_8 *intern_dict_classes( UINT_8 *src, obj **dstp, 
				    obj ct, obj cnamev, obj *missed )
{
  obj *dst = *dstp;
  UINT_8 num, index;
  obj cname;

  for (num = GET_8(src); num>0; num--)
    {
      index = GET_8(src);
      cname = gvec_read( cnamev, SLOT(index) );

      *dst = objecttable_lookup( ct, symbol_hash(cname), cname );
      if (EQ(*dst,FALSE_OBJ))
	{
	  *missed = cons( cname, *missed );
	  *dst = vector_class;
	}

#ifdef DEBUG_IMAGEIO
      printf( "   class name[%u] bytes: %s => %#x\n",
	      index, symbol_text(cname), *dst );
#endif /* DEBUG_IMAGEIO */
      dst++;
    }
  *dstp = dst;
  return src;
}

static UINT_8 *intern_dict_symbols( UINT_8 *src, obj **dstp, obj namev )
{
  obj *dst = *dstp;
  UINT_8 num, index;
  obj cname;

  for (num = GET_8(src); num>0; num--)
    {
      index = GET_8(src);
      *dst++ = gvec_read( namev, SLOT(index) );
    }
  *dstp = dst;
  return src;
}


static obj mk_fd_anchor( obj fd_anchor_class, 
		 	 obj m_name, UINT_32 part_num, UINT_8 fn_num )
{
    return make4( fd_anchor_class,
    		  m_name, int2fx(part_num), int2fx(fn_num),
		  NIL_OBJ );
}

static obj mk_cp_anchor( obj cp_anchor_class,  
		         obj fd_anchor, UINT_16 monotone_num )
{
    return make2( cp_anchor_class,
    		  fd_anchor, int2fx(monotone_num) );
}


static UINT_8 *extern_module_refs( UINT_8 *src, obj **dstp, 
				   obj fd_anchor_class, 
				   obj cp_anchor_class )
{
UINT_8 flag, k, len;
UINT_16 num_parts, i;
UINT_32 part_tag;
obj *dst = *dstp;
obj m_name;

  while ((len = GET_8(src)))
    {
      m_name = make_strdup( src, len );
#ifdef DEBUG_IMAGEIO
      printf( "   extern module '%s'\n", src );
#endif /* DEBUG_IMAGEIO */
      src += len+1;
      num_parts = GET_16(src);
#ifdef DEBUG_IMAGEIO
      printf( " (%u parts [0x%04x])\n", num_parts, num_parts );
#endif /* DEBUG_IMAGEIO */

      for (i=0; i<num_parts; i++)
	{
	  obj fpa;
	  part_tag = GET_32(src);
#ifdef DEBUG_IMAGEIO
	  printf( "\tpart[%u/%u] tag=%u flag=", i, num_parts, part_tag);
#endif /* DEBUG_IMAGEIO */

	  flag = GET_8(src);
	  if (flag == 255) {
	    /* non-compact */
	    UINT_8 m;

	    flag = GET_8(src);
#ifdef DEBUG_IMAGEIO
	    printf( "non-compact (%u fns)\n", flag );
#endif /* DEBUG_IMAGEIO */
	    while (flag > 0) {
	      flag--;
	      k = GET_8(src);
	      *dst++ = fpa = mk_fd_anchor( fd_anchor_class,
	      				m_name, part_tag, k );
	      m = GET_8(src);
	      while (m > 0) {
		UINT_16 x = GET_16(src);
		m--;
		*dst++ = mk_cp_anchor( cp_anchor_class, fpa, x );
	      }
	    }
	  } else if (flag == 254) {
	    /* semi-compact */
	    flag = GET_8(src);
#ifdef DEBUG_IMAGEIO
	    printf( "semi-compact (%u fns)\n", flag );
#endif /* DEBUG_IMAGEIO */
	    while (flag > 0) {
	      flag--;
	      k = GET_8(src);
	      *dst++ = fpa = mk_fd_anchor( fd_anchor_class,
	      				m_name, part_tag, k );
	      *dst++ = mk_cp_anchor( cp_anchor_class, fpa, 0 );
	    }
	  } else {
	    /* compact */
#ifdef DEBUG_IMAGEIO
	    printf( "compact (%u fns)\n", flag );
#endif /* DEBUG_IMAGEIO */
	    for (k=0; k<flag; k++) {
	      /* assert (part->functions[k]); */
	      *dst++ = fpa = mk_fd_anchor( fd_anchor_class,
	      				m_name, part_tag, k );
	      *dst++ = mk_cp_anchor( cp_anchor_class, fpa, 0 );
	    }
	  }
	}
    }
    *dstp = dst;
    return src;
}

