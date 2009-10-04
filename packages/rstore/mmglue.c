#include <stdio.h>
#include <errno.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <rscheme/platform.h>
#include <rscheme/scheme.h>
#include "mmglue.h"
#include <string.h>

#if defined(PLATFORM_NEXT)
#include "mmglue.mch"
#else

/********* common stuff ************/

static void *page_buff = NULL;
static unsigned num_bytes_in_buff = 0;

static char *(protname[3]) = { "NO_ACCESS", "READ_ONLY", "READ_WRITE" };
static int prot[3] = { PROT_NONE, PROT_READ, PROT_READ|PROT_WRITE };
static void *raw_mm_alloc( size_t bytes, enum mm_mode mode );
static void raw_mm_free( void *base, size_t bytes );

#define NUM_PAGES_TO_GRAB  (100)

void mm_free( void *base, size_t bytes )
{
  raw_mm_free( base, bytes );
}

void *mm_alloc( size_t bytes, enum mm_mode mode )
{
void *pg;

  if (bytes > num_bytes_in_buff)
    {
      if (bytes == MM_PAGE_SIZE)
	{
	  num_bytes_in_buff = NUM_PAGES_TO_GRAB * MM_PAGE_SIZE;
	  page_buff = raw_mm_alloc( num_bytes_in_buff, MM_MODE_NO_ACCESS );
	  if (!page_buff)
	    goto failed;
	}
      else
	{
	  pg = raw_mm_alloc( bytes, mode );
	  if (!pg)
	    goto failed;
	  return pg;
	}
    }
 ok:
  pg = page_buff;
  page_buff = (void *)((char *)page_buff + bytes);
  num_bytes_in_buff -= bytes;

  if (mode != MM_MODE_NO_ACCESS)
    mm_set_prot( pg, bytes, mode );

  return pg;

failed:
  scheme_error( "mm_alloc: couldn't alloc ~d pages (~a)",
		2,
		int2fx((bytes + MM_PAGE_MASK) / MM_PAGE_SIZE),
		make_string( strerror(errno) ) );
  return NULL;
}

void mm_unload( void *base, size_t bytes )
{
  if (munmap( base, bytes ) < 0) {
    perror( "mm_unload:munmap" );
  }
  if (mmap( base, bytes, PROT_NONE, MAP_ANON|MAP_PRIVATE, -1, 0 ) != base) {
    perror( "mm_unload:mmap" );
  }
  mm_set_prot( base, bytes, PROT_NONE );
}

void mm_set_prot( void *base, size_t bytes, enum mm_mode new_mode )
{
  int rc;
  
  rc = mprotect( (caddr_t)base, bytes, prot[new_mode] );
  if (rc < 0)
    {
      scheme_error( "mm_set_prot: at #x~04x_~04x for ~d bytes, to ~a: ~a",
		    5,
		    int2fx( ((UINT_32)base)>>16 ),
		    int2fx( ((UINT_32)base)&0xFFFF ),
		    int2fx( bytes ),
		    make_string( protname[new_mode] ),
		    make_string( strerror(errno) ) );
    }
}

/*********** platform-specific implementations **************/

#ifdef PLATFORM_AIX
#include "mmglue.aix"
#endif

#ifdef PLATFORM_LINUX
#include "mmglue.lnx"
#endif

#ifdef PLATFORM_SUNOS
#include "mmglue.sun"
#endif

#ifdef __FreeBSD__
#include "mmglue.bsd"
#endif

#ifdef PLATFORM_RHAPSODY
#include "mmglue.bsd"
#endif

#ifdef PLATFORM_DARWIN
#define PAGE_SIZE (4096)
#include "mmglue.bsd"
#endif

#ifdef __OpenBSD__
#include "mmglue.openbsd"
#endif

#ifdef PLATFORM_IRIX
#include "mmglue.irx"
#endif
#endif /* Mach */
