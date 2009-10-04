#ifndef _H_PROCESS
#define _H_PROCESS

#include <rscheme/obj.h>

#define PROCESS_NAME           SLOT(0)
#define PROCESS_PID            SLOT(1)
#define PROCESS_EXIT_STATUS    SLOT(2)
#define PROCESS_STATUS_WAITERS SLOT(3)

obj rs_fork_and_exec( obj process_obj, obj path, obj argv, obj envv, obj fdv,
                      obj dir, obj new_pgrp );

#endif /* _H_PROCESS */

