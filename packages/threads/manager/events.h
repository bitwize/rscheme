#ifndef _H_EVENTS
#define _H_EVENTS

enum sys_event_type {
  SYS_EVENT_FILE_READ,
  SYS_EVENT_FILE_WRITE,
  SYS_EVENT_FILE_EXCEPTION,
  SYS_EVENT_REALTIME,
  SYS_EVENT_REALTIME_INTERVAL,
  SYS_EVENT_THREADTIME,
  SYS_EVENT_THREADTIME_INTERVAL,
  SYS_EVENT_FILE_X_READ,
  SYS_EVENT_FILE_ACCEPT,
  SYS_EVENT_FILE_CONNECT,
  SYS_EVENT_FILE_RECV,
  SYS_EVENT_FILE_SELECT
};
  

struct sys_event {
  UINT_32 event_vec_ix;    /* fixed for a given hunk o' sys_event storage */
  struct sys_event *next;
  struct sys_event *prev;
  enum sys_event_type type;
  obj target;
  union {
    int fd;
    struct {
      struct sys_time next_time;
      struct sys_time delta_time;
    } time;
    struct {
      unsigned flags;
      struct sys_time expire_time;
      fd_set r, w, x;
    } select;
  } data;
};

#define OBJ_TO_SYS_EVENT(p) ((struct sys_event *)(OBJ_TO_RAW_PTR(p)))
#define SYS_EVENT_TO_OBJ(e) RAW_PTR_TO_OBJ(e)
#define EVENT_TARGET(e) (gvec_ref(target_vec,e->event_vec_ix))

extern struct sys_time thread_start;
extern int time_slice;

int compute_time_slice( obj t );

struct sys_event *make_read_event( int fd, obj target );
struct sys_event *make_connect_event( int fd, obj target );
struct sys_event *make_accept_event( int fd, obj target );
struct sys_event *make_write_event( int fd, obj target );
struct sys_event *make_exception_event( int fd, obj target );
struct sys_event *make_read_x_event( int fd, obj target );
struct sys_event *make_dpy_x_event( obj target );
struct sys_event *make_recv_event( int fd, obj target );
struct sys_event *make_select_event( fd_set *sets,
                                     struct sys_time *expire,
                                     obj mboxes,        /* not implemented */
                                     obj target );

struct sys_event *make_time_event( struct sys_time next_time, obj target );
struct sys_event *make_time_interval_event( struct sys_time next_time, 
					    struct sys_time interval,
					    obj target );

struct sys_event *make_thread_time_event( obj thread,
					  struct sys_time next_time, 
					  obj target );

struct sys_event *make_thread_time_interval_event( obj thread,
						   struct sys_time next_time, 
						   struct sys_time interval,
						   obj target );

void free_read_event( struct sys_event *e );
void free_connect_event( struct sys_event *e );
void free_accept_event( struct sys_event *e );
void free_write_event( struct sys_event *e );
void free_exception_event( struct sys_event *e );
void free_time_event( struct sys_event *e );
void free_thread_time_event( struct sys_event *e );

obj check_for_events( rs_bool block_q );

obj handle_read_x_event( struct sys_event *e, obj to_do );

void init_events( void );

#endif /* _H_EVENTS */
