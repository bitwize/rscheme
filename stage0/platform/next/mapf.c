/*-----------------------------------------------------------------*-C-*---
 * File:    handc/platform/next/mapf.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.5
 * File mod date:    1997-11-29 23:10:47
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          NeXT (Mach) mmap file access for imageio
 *------------------------------------------------------------------------*/

#include <stdio.h>
#include "mapf.h"
#ifdef __cplusplus
extern "C" {
#endif
#include <mach/mach.h>
/* #include <bsd/libc.h> */
#include <sys/file.h>

int open( const char *path, int flags, int mode );
int close( int fd );
long lseek( int, long, int );

char *getsectdata( const char *segname, const char *sectname, int *size );
extern kern_return_t map_fd(int fd, vm_offset_t offset, 
	vm_offset_t *addr, boolean_t find_space, vm_size_t numbytes);

#ifdef __cplusplus
}
#endif
#include <string.h>

static vm_address_t image_mapping_addr;
static vm_size_t image_mapping_size;
static void *image_mapping_ptr;
static int image_file;

rs_bool mapf_open( const char *path )
{
kern_return_t rc;

    if (strncmp(path,"sect:",5) == 0)
    {
    char *s, name[1000];
    
	strcpy( name, path+5 );
	s = strchr( name, ':' );
	*s++ = 0;
	
	image_file = -1;
	image_mapping_addr = 
	    (vm_address_t)getsectdata( name, s, (int *)&image_mapping_size );
	if (!image_mapping_addr)
	{
	    fprintf( stderr, "could not map segment %s, section %s\n",
	    			name, s );
	    return NO;
	}
    }
    else
    {
	image_file = open( path, O_RDONLY, 0 );
	if (image_file < 0)
	{
	    perror( path );
	    return NO;
	}
	image_mapping_size = lseek( image_file, 0L, SEEK_END );
	
	rc = map_fd( image_file, 
		    (vm_offset_t)0, 
		    &image_mapping_addr, 
		    /* find_space */ TRUE,
		    image_mapping_size );
	if (rc != KERN_SUCCESS)
	{
	    fprintf( stderr, "could not map file: %s\n", path );
	    close( image_file );
	    return NO;
	}
    }
    image_mapping_ptr = (void *)image_mapping_addr;
    return YES;
}

void mapf_seek( UINT_32 offset )
{
    image_mapping_ptr = (void *)(image_mapping_addr + offset);
}

void *mapf_read( UINT_32 bytes )
{
void *p = image_mapping_ptr;

    image_mapping_ptr += bytes;
    return p;
}

void mapf_close( void )
{
    if (image_file >= 0)
    {
	vm_deallocate( task_self(), image_mapping_addr, image_mapping_size );
	close( image_file );
    }
}

