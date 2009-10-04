/*-----------------------------------------------------------------*-C-*---
 * File:    handc/platform/next/dynlink.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.7
 * File mod date:    2003-08-20 13:33:26
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          NeXTSTEP (Mach) dynamic linking interface
 *------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef __cplusplus
extern "C" {
#endif
#include <streams/streams.h>
#include <mach-o/rld.h>
#ifdef __cplusplus
}
#endif

struct resolved {
    struct resolved *next;
    const char *sym;
};

struct dlfile {
    struct resolved *lookups;
    const char *path;
};

static const char *strdup( const char *str )
{
char *p = (char *)malloc( strlen(str)+1 );

    strcpy( p, str );
    return p;
}

static NXStream *errors = NULL;

void *resolve_link_symbol( void *info, const char *sym )
{
unsigned long val, rc;
struct resolved *i;
struct dlfile *f = (struct dlfile *)info;

    if (info)
    {
	i = (struct resolved *)malloc( sizeof( struct resolved ) );
	i->sym = strdup( sym );
	i->next = f->lookups;
	f->lookups = i;
    }

    if (!errors)
	errors = NXOpenFile( 2, NX_WRITEONLY );

    rc = rld_lookup( errors, sym, &val );
    NXFlush(errors);
    return (rc == 1) ? (void *)val : NULL;
}

void *dynamic_link_file( const char *path )
{
const char *(files[2]) = { path, NULL };
unsigned long rc;

    if (!errors)
	errors = NXOpenFile( 2, NX_WRITEONLY );

    rc = rld_load( errors, NULL, files, NULL );
    NXFlush(errors);
    if (rc == 1)
    {
    struct dlfile *f = (struct dlfile *)malloc( sizeof( struct dlfile ) );
    
	f->lookups = NULL;
        f->path = strdup( path );
	return f;
    }
    return NULL;
}

void done_resolving( void *info )
{
struct dlfile *f = (struct dlfile *)info;
struct resolved *i, *j;

    NXPrintf( errors, "Done with: %s\n", f->path );
    for (i=f->lookups; i; i=j)
    {
	NXPrintf( errors, "    forgetting %s\n", i->sym );
	rld_forget_symbol( errors, i->sym );
	j = i->next;
	free( (void *)i->sym );
	free( (void *)i );
    }
    rld_unload_all( errors, 0 );
    NXFlush( errors );
    free( (void *)f->path );
    free( (void *)f );
}

void init_dynamic_link( const char *argv0 )
{
}

const char *dynamic_link_errors( void )
{
  return NULL;
}
