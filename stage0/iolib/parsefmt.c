/**********************************************
THIS FILE WAS AUTOMATICALLY COPIED FROM THE
RSCHEME SOURCE TREE, AND THE ORIGINAL MAY CHANGE.
HENCE, DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

#line 1 "modules/iolib/parsefmt.c"
/*-----------------------------------------------------------------*-C-*---
 * File:    modules/iolib/parsefmt.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.8
 * File mod date:    1999-02-12 09:26:51
 * System build:     v0.7.3.4-b7u, 2007-05-30
 * Owned by module:  iolib
 *
 * Purpose:          parse format strings
 *------------------------------------------------------------------------*/

#include <rscheme/scheme.h>
#include <rscheme/smemory.h>
#include <rscheme/allocns.h>
#include <ctype.h>
#include "cports.h"

obj parse_format_string( obj str )
{
  obj entry, substr, prev, first, next;
  const char *begin, *s, *limit;
  int sharp_flag, star_flag, negative_flag;
  int pre_dot_lead_zero, pre_dot_num;
  int post_dot_digits, post_dot_num;
  obj at_flag, braced;

  prev = first = cons( FALSE_OBJ, NIL_OBJ );
  begin = s = string_text(str);
  limit = begin + string_length(str);
  while (s < limit)
    {
      if (s[0] == '~' && (s+1 < limit))
	{
	  if (begin != s)
	    {
	      /* flush the chars we've seen so far... */
	      substr = bvec_alloc( s - begin + 1, string_class );
	      memcpy( PTR_TO_DATAPTR(substr), (void*)begin, s - begin );
	      next = cons( substr, NIL_OBJ );
	      gvec_write_fresh_ptr( prev, SLOT(1), next );
	      prev = next;
	    }
	  begin = ++s;

	  pre_dot_lead_zero = 0;
	  post_dot_digits = -1;
	  pre_dot_num = -1;
	  post_dot_num = -1;

	  sharp_flag = 0;
	  star_flag = 0;
	  at_flag = FALSE_OBJ;
	  braced = FALSE_OBJ;

	another:
	  switch (*s)
	    {
	    case '#': 
	      sharp_flag = 1;
	      s++;
	      goto another;
	    case '*':
	      star_flag = 1;
	      s++;
	      goto another;
	    case '@':
	      at_flag = TRUE_OBJ;
	      s++;
	      goto another;
	    case '{':
	      {
		const char *sb = s;
		unsigned n;

		while ((s < limit) && (*s != '}'))
		  s++;

		n = s - sb - 1;

		braced = bvec_alloc( n+1, string_class );
		memcpy( string_text( braced ), sb+1, n );

		if (s < limit)
		  s++; /* skip the brace itself */
		goto another;
	      }
	    }

	  if (*s == '-')
	    {
	      s++;
	      negative_flag = 1;
	    }
	  else
	    negative_flag = 0;
	  if (isdigit(*(unsigned char *)s))
	    {
	      pre_dot_num = 0;
	      if (*s == '0')
		{
		  s++;
		  pre_dot_lead_zero = 1;
		}
	      while (isdigit(*(unsigned char *)s))
		{
		  pre_dot_num = (pre_dot_num * 10) + *s++ - '0';
		}
	    }
	  if (*s == '.')
	    {
	      s++;
	      post_dot_num = 0;
	      post_dot_digits = 0;
	      while (isdigit(*(unsigned char *)s))
		{
		  post_dot_digits++;
		  post_dot_num = (post_dot_num * 10) + *s++ - '0';
		}
	    }
	  if (begin == s)
	    {
	      entry = MAKE_ASCII_CHAR( *s );
	    }
	  else
	    {
	      entry = maken( vector_class,
			     10,
			     MAKE_ASCII_CHAR( *s ),
			     sharp_flag ? TRUE_OBJ : FALSE_OBJ,
			     star_flag ? TRUE_OBJ : FALSE_OBJ,
			     at_flag,
			     negative_flag ? TRUE_OBJ : FALSE_OBJ,
			     pre_dot_lead_zero ? TRUE_OBJ : FALSE_OBJ,
			     (pre_dot_num < 0) ? FALSE_OBJ 
			     : int2fx(pre_dot_num),
			     (post_dot_digits < 0) ? FALSE_OBJ 
			     : int2fx(post_dot_digits),
			     (post_dot_num < 0) ? FALSE_OBJ 
			     : int2fx(post_dot_num),
			     braced );
	    }
	  next = cons( entry, NIL_OBJ );
	  gvec_write_fresh_ptr( prev, SLOT(1), next );
	  prev = next;
	  begin = ++s;
	}
      else
	s++;
    }
  if (begin != s)
    {
      substr = bvec_alloc( s - begin + 1, string_class );
      memcpy( PTR_TO_DATAPTR(substr), (void*)begin, s - begin );
      next = cons( substr, NIL_OBJ );
      gvec_write_fresh_ptr( prev, SLOT(1), next );
    }
  return pair_cdr(first);
}
