#ifndef _H_SYSTIME
#define _H_SYSTIME

#include <rscheme/obj.h>
#include <sys/time.h>

struct sys_time {
    INT_32  sec;	/* seconds */
    INT_32  usec;	/* microseconds */
};

#define get_sys_time(p) gettimeofday( (struct timeval *)(p), NULL )
#define PTR_TO_SYS_TIME(o) ((struct sys_time *)PTR_TO_DATAPTR(o))

/* (*accum) += (t2 - t1) */

void accum_time( struct sys_time *accum, 
		 struct sys_time t1,
		 struct sys_time t2 );

/* t2 - t1 : max 27 hr */

int diff_time_ms( struct sys_time t1, struct sys_time t2 );
int diff_time_us( struct sys_time t1, struct sys_time t2 );


int time_le( struct sys_time ta, struct sys_time tb );  /* ta <= tb ? */
int time_lt( struct sys_time ta, struct sys_time tb );  /* ta <= tb ? */

#endif /* _H_SYSTIME */
