#include <rscheme/scheme.h>
#include <stdarg.h>
#include <errno.h>
#include "struct.h"
#include "util.h"

char *thread_name( obj t )
{
  return string_text( gvec_ref( t, THREAD_NAME ) );
}

obj thunkify_1( obj proc, obj arg )
{
  return make3( thunkifier_class,
		thunkifier_template,
		proc,
		cons( arg, NIL_OBJ ) );
}

obj thunkify_2( obj proc, obj arg1, obj arg2 )
{
  return make3( thunkifier_class,
		thunkifier_template,
		proc,
		cons( arg1, cons( arg2, NIL_OBJ ) ) );
}

obj thunkify_3( obj proc, obj arg1, obj arg2, obj arg3 )
{
  return make3( thunkifier_class,
		thunkifier_template,
		proc,
		cons( arg1, cons( arg2, cons( arg3, NIL_OBJ ) ) ) );
}
