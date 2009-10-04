/**********************************************
THIS FILE WAS AUTOMATICALLY COPIED FROM THE
RSCHEME SOURCE TREE, AND THE ORIGINAL MAY CHANGE.
HENCE, DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

#line 1 "modules/iolib/stdiox.h"
/*-----------------------------------------------------------------*-C-*---
 * File:    modules/iolib/stdiox.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.8
 * File mod date:    2000-12-23 12:50:25
 * System build:     v0.7.3.4-b7u, 2007-05-30
 * Owned by module:  iolib
 *
 *------------------------------------------------------------------------*/

#ifndef _H_STDIOX
#define _H_STDIOX

#include <stdio.h>

#define FX_AS_FILE(x) ((FILE *)OBJ_TO_RAW_PTR(x))
#define FILE_AS_FX(f) (RAW_PTR_TO_OBJ(f))

obj stdiox_fopen( char *path, char *mode );
obj stdiox_popen( char *path, char *mode );
int stdiox_pclose( obj f );
int stdiox_fwrite_str( obj f, obj str );
obj stdiox_fgetc( obj f );
obj stdiox_fgets( obj f );
obj stdiox_fgetln( obj f );
int stdiox_fcanget( obj f );
obj stdiox_fpeekc( obj f );

#define stdiox_stdin() FILE_AS_FX(stdin)
#define stdiox_stdout() FILE_AS_FX(stdout)
#define stdiox_stderr() FILE_AS_FX(stderr)

#define stdiox_fclose(x) fclose(FX_AS_FILE(x))
#define stdiox_fflush(x) fflush(FX_AS_FILE(x))
#define stdiox_ftell(x) ftell(FX_AS_FILE(x))
#define stdiox_fseek(x,y,z) fseek(FX_AS_FILE(x),y,z)

#define stdiox_fread_fill(f,bv,off,ln) fread( (char*)PTR_TO_DATAPTR(bv)+(off),\
					      1, (ln), \
					      FX_AS_FILE(f) )

#define stdiox_fputc(x,ch) fputc( GET_IMMEDIATE_VALUE(ch), FX_AS_FILE(x) )
#define stdiox_ferror(x) ferror( FX_AS_FILE(x) )
#define stdiox_clearerr(x) clearerr( FX_AS_FILE(x) )
#define stdiox_feof(x) (feof(FX_AS_FILE(x)) ? YES : NO)

#endif /* _H_STDIOX */
