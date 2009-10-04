#include "rs_sys_threads_manager_p.h"

#if SUPPORT_X
#include <string.h>
#include <X11/Xlib.h>

static Display *target_to_display( obj target )
{
  return (Display *)OBJ_TO_RAW_PTR(gvec_ref(target,XQUEUE_DISPLAY));
}


obj handle_read_x_event( struct sys_event *e, obj to_do )
{
  obj event_obj;
  XEvent *event;
  Display *dsp = target_to_display( EVENT_TARGET(e) );
  obj mbox = EVENT_TARGET(e);

  while (1)
    {
      event_obj = alloc( sizeof( XEvent ), x_event_class );
      event = (XEvent *)PTR_TO_DATAPTR(event_obj);
      
      memset( event, 0, sizeof(XEvent) );
      
      if (!XCheckMaskEvent( dsp, ~0L, event ))
	return to_do;

      if (DEBUG_THREAD_EVENTS)
	printf( "got X event type = %d, display {%p}, window {%#x}\n",
		event->xany.type,
		event->xany.display,
		(unsigned)event->xany.window );
      ksend_mailbox( mbox, event_obj );
    }
}

struct sys_event *make_dpy_x_event( obj target )
{
  Display *d = target_to_display(target);

  return make_read_x_event( ConnectionNumber(d), target );
}

#else

obj handle_read_x_event( struct sys_event *e, obj to_do )
{
  return to_do;
}

struct sys_event *make_x_event( void *dpy, obj mbox )
{
  scheme_error( "X events not supported", 0 );
  return NULL;
}

#endif
