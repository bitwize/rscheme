#ifndef _H_OUTPUT
#define _H_OUTPUT

rs_bool qout_write_str( obj port, obj str );
rs_bool qout_writev( obj port, obj vec );
rs_bool qout_flush( obj port );
rs_bool qout_write_bytes( obj port, obj bvec, INT_32 offset, INT_32 len );

obj handle_std_write_event( struct sys_event *e, obj to_do );

#endif /* _H_OUTPUT */

