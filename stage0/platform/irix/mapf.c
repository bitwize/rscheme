/*-----------------------------------------------------------------*-C-*---
 * File:    handc/platform/irix/mapf.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.3
 * File mod date:    1997-11-29 23:10:47
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          AIX mmap implementation of file access for imageio
 *------------------------------------------------------------------------*/

#include "mapf.h"
#ifdef __cplusplus
extern "C" {
#endif

#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>
#ifdef __cplusplus
}
#endif

#include <unistd.h>
#include <stdio.h>

static caddr_t image_mapping_addr;
static size_t image_mapping_size;
static void *image_mapping_ptr;
static int image_file;

rs_bool mapf_open( const char *path )
{
  image_file = open( path, O_RDONLY, 0 );
  if (image_file < 0)
    {
      perror( path );
      return NO;
    }
  image_mapping_size = lseek( image_file, 0L, SEEK_END );
  
  image_mapping_addr = mmap( (caddr_t)0, image_mapping_size, 
			     PROT_READ, MAP_SHARED, 
			     image_file, (off_t)0 );
  if (!image_mapping_addr)
    {
      fprintf( stderr, "could not map file: %s\n", path );
      close( image_file );
      return NO;
    }

  image_mapping_ptr = (void *)image_mapping_addr;
  return YES;
}

void mapf_seek( UINT_32 offset )
{
    image_mapping_ptr = (void *)((char *)image_mapping_addr + offset);
}

void *mapf_read( UINT_32 bytes )
{
void *p = image_mapping_ptr;

    image_mapping_ptr = (void *)((char *)image_mapping_ptr + bytes);
    return p;
}

void mapf_close( void )
{
    if (image_file >= 0)
    {
        munmap( image_mapping_addr, image_mapping_size );
	close( image_file );
    }
}

