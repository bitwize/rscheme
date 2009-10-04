/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/function.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.5
 * File mod date:    2003-06-12 21:53:54
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          Basic operations on Scheme functions
 *------------------------------------------------------------------------*
 * Notes:
 *      Starting early in 0.6, procedures (templates, actually), have
 *      function-descr slots which holds a (possibly empty) property list.
 *      
 *      One such property is function-scope, which, if present, describes
 *      the "name" of the function, such as it is.
 *------------------------------------------------------------------------*/

#include <string.h>
#include <rscheme/scheme.h>
#include <rscheme/smemory.h>

/*------------------------------------------------------------------------*
 * Function:	template_scope
 * Purpose:	return the value of the function-scope procedure property
 * Arguments:	(obj)tmpl -- the template from which to retrieve the FS
 * Result:	(obj)value -- '() if not found present, else the FS
 * Notes:
 *	Starting early in 0.6, procedures (templates, actually), have
 *	function-descr slots which holds a (possibly empty) property list.
 *
 *	One such property is function-scope, which, if present, describes
 *	the "name" of the function, such as it is.
 *------------------------------------------------------------------------*/

obj template_scope( obj tmpl )
{
  static obj fss = ZERO;
  obj p;

  if (EQ(fss,ZERO))
    fss = lookup_symbol( "function-scope" );

  if (!TEMPLATE_P( tmpl )) {
    return FALSE_OBJ;
  }

  for (p=gvec_read(tmpl,SLOT(2)); !OBJ_ISA_NIL(p); p=pair_cdr(p))
    {
      if (EQ(fss,pair_car(pair_car(p))))
	{
	  return pair_cdr( pair_car(p) );
	}
    }
  return NIL_OBJ;
}

static char *sprocedure_name( char *buf, unsigned len, obj tmpl )
{
  char *d;
  obj a, x;

  d = buf;
  for (a = template_scope(tmpl); !OBJ_ISA_NIL(a); a=pair_cdr(a))
    {
      if (d != buf)
	{
	  *d++ = ' ';
	  len--;
	}
      x = pair_car(a);
      if (SYMBOL_P(x))
	{
	  strcpy( d, symbol_text(x) );
	  d += strlen(d);
	}
      else if (OBJ_ISA_FIXNUM(x))
	{
#if SPRINTF_RETURNS_INT
	  d += sprintf( d, "%ld", (long)fx2int(x) );
#else
	  sprintf( d, "%ld", (long)fx2int(x) );
	  d += strlen(d);
#endif
	}
      else
	*d++ = '@';
    }
  *d++ = 0;
  return buf;
}

char *procedure_name( obj tmpl )
{
  static char temp[200];
  return sprocedure_name( temp, 200, tmpl );
}
