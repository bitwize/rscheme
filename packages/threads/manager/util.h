#include <rscheme/platform.h>

obj make_os_error( const char *fn, int num_args, ... );
char *thread_name( obj t );

obj thunkify_1( obj proc, obj arg );
obj thunkify_2( obj proc, obj arg1, obj arg2 );
obj thunkify_3( obj proc, obj arg1, obj arg2, obj arg3 );
