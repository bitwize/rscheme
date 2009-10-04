/*-----------------------------------------------------------------*-C-*---
 * File:	    packages/lss/rscheme/pkgs/lss/lss.h
 *
 *          Copyright (C)1998 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest info.
 *
 * File version:     1.16
 * File mod date:    2005-09-16 11:05:33
 * System build:     v0.7.3.4-b7u, 2007-05-30
 * Owned by module:  lss
 *
 * Purpose:          Public interface to LSS functionality
 *------------------------------------------------------------------------*/

#ifndef _H_LSS
#define _H_LSS

#include <stdarg.h>
#include <stdlib.h>

typedef unsigned long long UINT_64;

#ifdef LSS_STANDALONE

typedef unsigned long UINT_32;
typedef unsigned short UINT_16;
typedef unsigned char UINT_8;

#else /* LSS_STANDALONE */

#include <rscheme/obj.h>

#endif
#include <rscheme/pkgs/lss/lsszips.h>

typedef struct LSS LSS;

/*  an (mostly opaque) handle to a readable record  */

typedef struct LSSAccess LSSAccess;

/*  the non-opacity is that all implementations store the
 *  uncompressed size at the beginning, as a `size_t'
 */

size_t lss_access_bytes( LSSAccess *a );

#define LSS_OPEN   (1)     /*                             */
#define LSS_CREATE (2)     /* (int)filemode               */
#define LSS_EXTEND (4)     /* (int)filemode, (char *)from */
#define LSS_RDWR   (16)    /*                             */
#define LSS_RDONLY (0)     /*                             */
#define LSS_BACKLEVEL (32) /* (int)gen                    */

UINT_32 lss_commit( LSS *lss, int flag );

LSS *lss_vopen( const char *file, int opts, va_list va );
LSS *lss_open( const char *file, int opts, ... );

void lss_close( LSS *l );

/* returns NULL if the given record does not exist */

LSSAccess *lss_read_access( LSS *lss, UINT_32 recnum );

void lss_readv( LSS *lss, zipbuf *vec, LSSAccess *a );
void lss_read_release( LSS *lss, LSSAccess *a );
void lss_read_recnum( LSS *lss, void *buf, size_t len, UINT_32 recnum );

void lss_write( LSS *lss, UINT_32 recnum, void *buf, size_t len, 
		zip_algorithm *use ); /* NULL => null algorithm */

void lss_writev( LSS *lss, UINT_32 recnum, zipbuf *vec, 
		 zip_algorithm *use ); /* NULL => null algorithm */

struct LSSRecordInfo {
  UINT_32   record_num;
  int       volume;
  UINT_32   offset;
};

size_t lss_copy_record( LSS *dst, LSS *src, UINT_32 recnum );

UINT_32 *lss_get_record_index( LSS *lss, UINT_32 *cnt );
void lss_get_record_info( LSS *lss, 
			  UINT_32 record_num, 
			  struct LSSRecordInfo *info );

/*  return the filename corresponding to the given
 *  volume. 
 *  Returns NULL if there is no such volume.
 *  negative vol_num's count from the most recent,
 *  so -1 means the latest volume, -2 the next-to-last, etc.
 */

const char *lss_filename( LSS *lss, int vol_num );

/*
 *  Find a record that is in one of the specified volumes.
 *  Only try so hard according to `pass'.  Return ~0 if not found.
 */

UINT_32 lss_find_record_on_vol( LSS *lss, unsigned mask, int pass );

/*
 *  flags:
 *        #o777   for underlying file mode
 *        #x1000  LSS_CREATE
 *        #x2000  LSS_RDWR
 */

#define LSS_MAX_VOLUMES     (10)            /* lssv3 can actually handle 16 */

LSS *lss_openx( const char **filev, unsigned gen, int flags );

int lss_set_tip( LSS *lss, int vol );
int lss_get_tip( LSS *lss );

int lss_attach_vol( LSS *lss, int vol, const char *file ); 
int lss_detach_vol( LSS *lss, int vol ); 
size_t lss_move_record( LSS *lss, int destvol, UINT_32 recnum );
size_t lss_get_vol_size( LSS *lss, int vol );

unsigned lss_current_generation( LSS *lss );
int lss_set_generation( LSS *lss, unsigned gen );

int lss_record_query( LSS *lss, UINT_32 from, UINT_32 to, UINT_32 **result );

int lss_alloc_recs( LSS *lss, UINT_32 minrec, UINT_32 count, UINT_32 *result );
int lss_delete_recs( LSS *lss, UINT_32 first, UINT_32 count );
int lss_delete( LSS *lss, UINT_32 rec );
int lss_tune( LSS *generic, const char *key, const char *value );

#endif /* _H_LSS */
