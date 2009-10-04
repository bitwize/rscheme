/*-----------------------------------------------------------------*-C-*---
 * File:    handc/platform/mac/platform.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.10
 * File mod date:    1997-11-29 23:10:47
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 *------------------------------------------------------------------------*/

#ifndef _H_RSCHEME_PLATFORM
#define _H_RSCHEME_PLATFORM

/* platform-independence layer */

/*  Mac (CodeWarrior)  */

#ifdef USE_HW_REGS
#error "HW Registers not supported on the Mac"
#endif

#define PLATFORM_MAC

#ifdef __MWERKS__
#define PLATFORM_MAC_CODEWARRIOR
#endif

typedef unsigned long UINT_32;
typedef unsigned short UINT_16;
typedef unsigned char UINT_8;

typedef signed long INT_32;
typedef signed short INT_16;
typedef signed char INT_8;

typedef double IEEE_64;
typedef float IEEE_32;

#define PLATFORM_IS_BIG_ENDIAN

#define HOST_TO_BIG_ENDIAN_16(x) (x)
#define HOST_TO_BIG_ENDIAN_32(x) (x)
#define HOST_TO_BIG_ENDIAN_IEEE_64(x) (x)

#define BIG_ENDIAN_TO_HOST_16(x) (x)
#define BIG_ENDIAN_TO_HOST_32(x) (x)
#define BIG_ENDIAN_TO_HOST_IEEE_64(x) (x)

#ifdef PLATFORM_MAC_CODEWARRIOR
#define SPRINTF_RETURNS_INT 1
#define HAVE_STRERROR 1
#define HAVE_MEMCPY 1
#define DOESNT_HAVE_POPEN 1

#define USE_COMPUTED_GOTO 0
#define HAVE_NTOHL 0
#define HAVE_NTOHS 0
#endif

#endif
