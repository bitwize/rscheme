#include <time.h>
#include <sys/time.h>

#if defined(PLATFORM_LINUX) || defined(PLATFORM_NEXT)
#include <sys/resource.h>
#ifndef RUSAGE_SELF
#define RUSAGE_SELF (0)
#endif
#endif

int rscheme_file_mode_to_os( int mode );
void install_handler( int signum, int reinstall_q );
obj get_sig_buffer( void );

/*  this function is in the `syscalls' package, but we declare it here
 *   so we can use it
 */

obj os_time( struct timeval *tv, obj t_class );
