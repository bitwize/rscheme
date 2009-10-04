/*------------------------------------------------------------------*-C-*-*
 * %Z%1.6  %G% 18:14:02 %W%
 *
 * Purpose:	Common Header for `syscalls' module
 *
 *------------------------------------------------------------------------*
 * Notes:
 *------------------------------------------------------------------------*/

#ifndef _H_SYSCALLS_SYSTEMH
#define _H_SYSCALLS_SYSTEMH

#ifdef PLATFORM_MAC

/* GUSI library */
#include "GUSI.h"

#else

#include <string.h>
#include <math.h>
#include <dirent.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <netdb.h>
#include <netinet/in.h>
#include <unistd.h>
#include <ctype.h>
#include <utime.h>

#include <arpa/inet.h>
#include <sys/socketvar.h>

#if !defined(PLATFORM_LINUX) &&  !defined(PLATFORM_NEXT)
#include <sys/select.h>
#endif

#if defined(PLATFORM_NEXT)
#define CLOCKS_PER_SEC CLK_TCK

#define S_ISDIR(m) (((m)&S_IFMT)==S_IFDIR)
#define S_ISREG(m) (((m)&S_IFMT)==S_IFREG)
#define S_ISFIFO(m) (((m)&S_IFMT)==S_IFDIR)
#define S_ISCHR(m) (((m)&S_IFMT)==S_IFCHR)
#define S_ISBLK(m) (((m)&S_IFMT)==S_IFBLK)
#endif

#include <fcntl.h>

#endif

#include <errno.h>
#include <rscheme/obj.h>

obj rs_scandir( const char *path );

#endif /* _H_SYSCALLS_SYSTEMH */
