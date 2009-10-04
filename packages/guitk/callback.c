#include <tcl.h>
#include <tk.h>
#include <rscheme/scheme.h>
#include <rscheme/api.h>

obj queued_callbacks;

int the_delayed_callback( ClientData data, Tcl_Interp *interp,
			  int argc, const char **argv )
{
  obj item, info = NIL_OBJ;
  
  while (argc > 1)
    {
      const char *a = argv[--argc];
      if (isdigit(a[0]) || a[0] == '-')
	item = int2fx( atoi(a) );
      else
	item = make_string(a);
      info = cons( item, info );
    }
  queued_callbacks = cons( info, queued_callbacks );
  return TCL_OK;
}

#if 0 

obj vcall_scheme( obj closure, unsigned num_args, obj *args )
{
int n;

    /* manufacture a continuation which, when resumed,
	terminates the current run.  */
    
    envt_reg = NIL_OBJ;
    literals_reg = FALSE_OBJ;
    dynamic_state_reg = NIL_OBJ;
    continuation_reg = FALSE_OBJ;
    push_cont( finish_run_descr.monotones[0], 0 );
    
    va_start( ap, num_args );
    vpush_call_continuation( closure, num_args, ap );
    va_end( ap );

    n = run(continuation_reg);

    return temp_space[0];
}
#endif
