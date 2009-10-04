#include "rs_sys_threads_manager_p.h"

static void adj_time( struct sys_time *t )
{
 again:
    if (t->usec >= 1000000)
    {
	t->usec -= 1000000;
	t->sec++;
	goto again;
    }
    else if (t->usec < 0)
    {
	t->usec += 1000000;
	t->sec--;
	goto again;
    }
}

void accum_time( struct sys_time *accum, 
		 struct sys_time t1,
		 struct sys_time t2 )
{
  accum->usec += t2.usec - t1.usec;
  accum->sec += t2.sec - t1.sec;
  adj_time( accum );
}

int diff_time_ms( struct sys_time t1, struct sys_time t2 )
{
  int dt = t2.sec - t1.sec;
  if (dt > 100000)
    return 100000000;  /* overflow at 100,000 seconds, abt. 27 hours */
  else if (dt < -100000)
    return -100000000;
  else
    return (dt * 1000) + (t2.usec - t1.usec) / 1000;
}

int diff_time_us( struct sys_time t1, struct sys_time t2 )
{
  int dt = t2.sec - t1.sec;

  if (dt > 1000)
    return 1000000000;  /* overflow at 1000 seconds */
  else if (dt < -1000)
    return -1000000000;
  else if (dt == 0)
    return t2.usec - t1.usec;
  else
    return (dt * 1000000) + (t2.usec - t1.usec);
}

int time_le( struct sys_time ta, struct sys_time tb )
{
  if (ta.sec == tb.sec)
    {
      return (ta.usec <= tb.usec);
    }
  else
    return (ta.sec < tb.sec);
}

int time_lt( struct sys_time ta, struct sys_time tb )
{
  if (ta.sec == tb.sec)
    {
      return (ta.usec < tb.usec);
    }
  else
    return (ta.sec < tb.sec);
}
