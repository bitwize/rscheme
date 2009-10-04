/*-----------------------------------------------------------------*-C-*---
 * File:    handc/platform/dos/platform.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.3
 * File mod date:    1997-11-29 23:10:47
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 *------------------------------------------------------------------------*/

#ifndef _H_RSCHEME_PLATFORM
#define _H_RSCHEME_PLATFORM

/* platform-independence layer */

/*  DOS (i86)  */

#ifdef USE_HW_REGS
#warn "Hardware regs not supported on x86 -- too few!"
#endif

#define PLATFORM_DOS

typedef unsigned long int UINT_32;
typedef unsigned short UINT_16;
typedef unsigned char UINT_8;

typedef long int INT_32;
typedef short INT_16;
typedef signed char INT_8;

typedef double IEEE_64;
typedef float IEEE_32;

#define PLATFORM_IS_LITTLE_ENDIAN

IEEE_32 ntohf( IEEE_32 flt );
IEEE_32 htonf( IEEE_32 flt );

IEEE_64 ntohd( IEEE_64 dbl );
IEEE_64 htond( IEEE_64 dbl );

#define HOST_TO_BIG_ENDIAN_16(x)      htons(x)
#define HOST_TO_BIG_ENDIAN_32(x)      htonl(x)
#define HOST_TO_BIG_ENDIAN_IEEE_64(x) htonf(x)

#define BIG_ENDIAN_TO_HOST_16(x)      ntohs(x)
#define BIG_ENDIAN_TO_HOST_32(x)      ntohl(x)
#define BIG_ENDIAN_TO_HOST_IEEE_64(x) ntohf(x)

#endif /* _RSCHEME_PLATFORM */
