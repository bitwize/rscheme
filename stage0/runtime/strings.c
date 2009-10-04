/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/strings.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.10
 * File mod date:    2003-12-12 14:27:33
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          Primitive implementations for basic string operations
 *------------------------------------------------------------------------*/

#include <ctype.h>
#include <string.h>
#include <rscheme/scheme.h>
#include <rscheme/smemory.h>

rs_bool string_eq( obj str1, obj str2 )
{
INT_32 n = SIZEOF_PTR(str1);
const UINT_32 *s1 = (const UINT_32 *)PTR_TO_DATAPTR(str1);
const UINT_32 *s2 = (const UINT_32 *)PTR_TO_DATAPTR(str2);

    if (n != SIZEOF_PTR(str2))
	return NO;

    while (n > 0)
    {
	if (*s1++ != *s2++)
	    return NO;
	n -= SLOT(1);
    }
    return YES;
}

rs_bool string_ci_eq( obj str1, obj str2 )
{
    return (string_ci_cmp( str1, str2 ) == 0) ? YES : NO;
}

int string_cmp( obj str1, obj str2 )
{
  UINT_32 len1, len2;
  const unsigned char *s1, *s2;

  s1 = (const unsigned char *)string_text(str1);
  len1 = string_length(str1);

  s2 = (const unsigned char *)string_text(str2);
  len2 = string_length(str2);

  while (1)
    {
      if (len1 && len2)
	{
	  if (*s1 != *s2)
	    {
	      if (*s1 < *s2)
		return -1;
	      else
		return 1;
	    }
	  s1++;
	  s2++;
	  len1--;
	  len2--;
	}
      else
	{
	  if (len1) /* still some str1 left, so str1 is bigger */
	    return 1;
	  else if (len2) /* still some str2 left, so str2 is bigger */
	    return -1;
	  else
	    return 0;
	}
    }
}

int string_ci_cmp( obj str1, obj str2 )
{
  const unsigned char *s1 = (const unsigned char *)string_text(str1);
  const unsigned char *s2 = (const unsigned char *)string_text(str2);
  unsigned char c1, c2;
  UINT_32 len1 = string_length(str1);
  UINT_32 len2 = string_length(str2);

  while (1)
    {
      if (len1 && len2)
	{
	  c1 = *s1++;
	  c2 = *s2++;
	  if (isupper(c1))
	    c1 = tolower(c1);
	  if (isupper(c2))
	    c2 = tolower(c2);
	  if (c1 != c2)
	    {
	      if (c1 < c2)
		return -1;
	      else
		return 1;
	    }
	  len1--;
	  len2--;
	}
      else
	{
	  if (len1)
	    return 1;
	  else if (len2)
	    return -1;
	  else
	    return 0;
	}
    }
}

obj make_string( const char *text )
{
size_t len;
obj str;

    if (text)
    {
	len = strlen(text);
	str = bvec_alloc( len+1, string_class );
	memcpy( PTR_TO_DATAPTR(str), text, len );
	return str;
    }
    else
	return FALSE_OBJ;
}

/* we cast the string text pointer to an 'unsigned char *'
   so that characters we extract do not get sign extended
                                          -- dmk 94.06.21 */

obj string_ref( obj str, UINT_32 index )
{
    assert( index < string_length(str) );
    return MAKE_ASCII_CHAR( ((unsigned char *)string_text(str))[index] );
}

void string_set( obj str, UINT_32 index, obj ch )
{
    assert( index < string_length(str) );
    assert( OBJ_ISA_ASCII_CHAR(ch) );
    ((char *)string_text(str))[index] = GET_IMMEDIATE_VALUE(ch);
}

void bvec_copy(obj dst, INT_32 dst_offset,
	       obj src, INT_32 src_offset, INT_32 len )
{
  char *dst_p;
  const char *src_p;

  assert( (len >= 0) && (dst_offset >= 0) && (src_offset >= 0) );
  assert( (dst_offset + len) <= SIZEOF_PTR(dst) );
  assert( (src_offset + len) <= SIZEOF_PTR(src) );

  dst_p = (char *)PTR_TO_DATAPTR(dst) + dst_offset;
  src_p = (const char *)PTR_TO_DATAPTR(src) + src_offset;

  memmove( dst_p, src_p, len );
}

obj bvec_hash( obj bvec, INT_32 offset, INT_32 len )
{
  const char *p;

  assert( len >= 0 );
  assert( (offset + len) <= SIZEOF_PTR(bvec) );
  p = (char *)PTR_TO_DATAPTR(bvec);

  return raw_bytes_hash( p + offset, len );
}

obj bvec_ci_hash( obj bvec, INT_32 offset, INT_32 len )
{
  const char *p;

  assert( len >= 0 );
  assert( (offset + len) <= SIZEOF_PTR(bvec) );
  p = (char *)PTR_TO_DATAPTR(bvec);

  return raw_ci_bytes_hash( p + offset, len );
}

#define rs_str_search(name,str_type,seek_type)        \
static obj name( str_type *str,			      \
		 UINT_32 str_cnt,		      \
		 seek_type *seek,		      \
		 UINT_32 seek_cnt,		      \
		 INT_32 skipn )			      \
{						      \
  seek_type first_ch;  			              \
  str_type *p, *lim;			              \
  UINT_32 i;					      \
						      \
  first_ch = *seek++;				      \
  seek_cnt--;					      \
  p = str + skipn;				      \
  lim = str + str_cnt;				      \
						      \
  if (seek_cnt == 0)				      \
    {						      \
      while (p < lim)				      \
	{					      \
	  if (*p == first_ch)			      \
	    {					      \
	      return int2fx( p - str );		      \
	    }					      \
	  p++;					      \
	}					      \
    }						      \
  else						      \
    {						      \
      while (p < lim)				      \
	{					      \
	  if (*p == first_ch)			      \
	    {					      \
	      for (i=0; i<seek_cnt; i++)	      \
		{				      \
		  if (p[i+1] != seek[i])	      \
		    goto skip;			      \
		}				      \
	      return int2fx( p - str );               \
	    skip: /* oh well */;		      \
	    }					      \
	  p++;					      \
	}					      \
    }						      \
  return FALSE_OBJ;				      \
}

rs_str_search(rs_bb_search,UINT_8,UINT_8)
rs_str_search(rs_bu_search,UINT_8,UINT_16)
rs_str_search(rs_ub_search,UINT_16,UINT_8)
rs_str_search(rs_uu_search,UINT_16,UINT_16)

obj rs_string_search( obj str, obj seek, INT_32 skipn )
{
  if (BYTE_STRING_P(str))
    {
      UINT_8 *str_p = byte_string_text(str);
      UINT_32 str_cnt = byte_string_length(str);

      if ((skipn < 0) || (skipn >= str_cnt))
	{
	  if (skipn == str_cnt)
	    return FALSE_OBJ;
	  else
	    scheme_error( "string-search: offset ~d out of range for ~s",
			  2, int2fx(skipn), str );
	}

      if (BYTE_STRING_P(seek))
	return rs_bb_search( str_p,
			     str_cnt,
			     byte_string_text(seek),
			     byte_string_length(seek),
			     skipn );
      else if (UNICODE_STRING_P(seek))
	return rs_bu_search( str_p,
			     str_cnt,
			     unicode_string_text(seek),
			     unicode_string_length(seek),
			     skipn );
      else if (BYTE_CHAR_P(seek))
	{
	  UINT_8 ch = ASCII_CHAR_VALUE(seek);
	  return rs_bb_search( str_p,
			       str_cnt,
			       &ch, 1,
			       skipn );
	}
      else if (UNICODE_CHAR_P(seek))
	{
	  UINT_16 ch = UNICODE_CHAR_VALUE(seek);
	  return rs_bu_search( str_p,
			       str_cnt,
			       &ch, 1,
			       skipn );
	}
      else
	{
	  scheme_error( "string-search: seek value of ~s is invalid", 
			1, seek );
	  return FALSE_OBJ;
	}
    }
  else if (UNICODE_STRING_P(str))
    {
      UINT_16 *str_p = unicode_string_text(str);
      UINT_32 str_cnt = unicode_string_length(str);

      if ((skipn < 0) || (skipn >= str_cnt))
	{
	  if (skipn == str_cnt)
	    return FALSE_OBJ;
	  else
	    scheme_error( "string-search: offset ~d out of range for ~s",
			  2, int2fx(skipn), str );
	}

      if (BYTE_STRING_P(seek))
	return rs_ub_search( str_p,
			     str_cnt,
			     byte_string_text(seek),
			     byte_string_length(seek),
			     skipn );
      else if (UNICODE_STRING_P(seek))
	return rs_uu_search( str_p,
			     str_cnt,
			     unicode_string_text(seek),
			     unicode_string_length(seek),
			     skipn );
      else if (BYTE_CHAR_P(seek))
	{
	  UINT_8 ch = ASCII_CHAR_VALUE(seek);
	  return rs_ub_search( str_p,
			       str_cnt,
			       &ch, 1,
			       skipn );
	}
      else if (UNICODE_CHAR_P(seek))
	{
	  UINT_16 ch = UNICODE_CHAR_VALUE(seek);
	  return rs_uu_search( str_p,
			       str_cnt,
			       &ch, 1,
			       skipn );
	}
      else
	{
	  scheme_error( "string-search: seek value of ~s is invalid",
			1, seek );
	  return FALSE_OBJ;
	}
     }
  else
    {
      scheme_error( "string-search: string value of ~s is invalid",
		    1, str );
      return FALSE_OBJ;
    }
}

/* make sure that the pages of a string's content are
   mapped into memory.  Normally, this is not an issue,
   but when the persistent store is being used, some pages
   may be mapped on-demand, and the kernel isn't able to
   do that.  Hence, before passing the content of a string
   to a system call, call this function.
*/

void ensure_memory_mapped( void *ptr, UINT_32 len )
{
  unsigned char *p = ptr;
  volatile unsigned char x;

  if (len > 0) {
    x = p[len-1];
    while (len > 4096) {
      x = p[0];
      p += 4096;
      len -= 4096;
    }
  }
}

void ensure_string_mapped( obj str )
{
  ensure_memory_mapped( string_text( str ), string_length( str ) );
}
