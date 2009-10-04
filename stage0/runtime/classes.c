/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/classes.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.3
 * File mod date:    1997-11-29 23:10:50
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          Runtime system support for <<class>> objects
 *------------------------------------------------------------------------*/

#include <rscheme/scheme.h>
#include <rscheme/smemory.h>

/* helper function... called by `object_class' when the
   given instance does not have a PTR tag
*/

obj immob_class( obj thing )
{
  if (OBJ_ISA_FIXNUM(thing))
    return fixnum_class;
  else if (OBJ_ISA_IMMOB(thing))
    {
      switch (SECONDARY_TAG(thing))
	{
	case BOOLEAN_TAG:		return boolean_class;
	case NIL_TAG:		return nil_class;
	case ASCII_CHAR_TAG:	return ascii_char_class;
	case UNICODE_CHAR_TAG:	return unicode_char_class;
	case UNIQUE_OBJ_TAG:	return unique_obj_class;
	  
	case SPARE_1_TAG:	return spare_1_class;
	case SPARE_2_TAG:	return spare_2_class;
	case SPARE_3_TAG:	return spare_3_class;
	}
    }
  return unique_obj_class;
}

unsigned class_image_mode( obj a_class )
{
    return fx2int( gvec_read( a_class, SLOT(2) ) );
}

rs_bool class_is_gvec( obj a_class )
{
    assert( CLASS_P(a_class) );
    return EQ(gvec_read(a_class,SLOT(1)),ZERO);
}

obj class_name( obj a_class )
{
    assert( CLASS_P(a_class) );
    return gvec_read( a_class, SLOT(0) );
}

/* returns YES iff class1 is a SUBCLASS of class2
   (improper subclassing *NOT* permitted, having already
    been checked by subclass_p).  Also, this function
   will crash if class_supers() returns something besides
   a <list>
*/

rs_bool indirect_subclass_p( obj class1, obj class2 )
{
  obj sup;
  
  while (1)
    {
      sup = class_supers(class1);
      if (NULL_P(sup))
	return NO;

      class1 = pair_car( sup );
      if (EQ(class1, class2))
	return YES;

      /* unrolled by hand once... */

      sup = class_supers(class1);
      if (NULL_P(sup))
	return NO;

      class1 = pair_car( sup );
      if (EQ(class1, class2))
	return YES;
    }
}


