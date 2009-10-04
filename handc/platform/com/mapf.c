/*-----------------------------------------------------------------*-C-*---
 * File:    handc/platform/com/mapf.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.7
 * File mod date:    1998-01-04 14:22:15
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          generic (stdio) file access for imageio
 *------------------------------------------------------------------------*/

#include "mapf.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <rscheme/smemory.h>
#include <rscheme/osglue.h>
#include <string.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif

struct image_buffer {
    void *block;
    struct image_buffer *next;
};

static struct image_buffer *image_buffers;
static FILE *image_file;
static char *image_base;
static char *image_ptr;

rs_bool mapf_open( const char *path )
{
  if (strncmp(path,"memory:",7) == 0)
    {
      image_base = (char *)atol( path+7 );
      image_ptr = image_base;
      image_file = NULL;
    }
  else
    {
      image_file = os_fopen( path, "rb" );
      if (!image_file)
	{
	  perror( path );
	  return NO;
	}
      image_base = NULL;
    }
  image_buffers = NULL;
  return YES;
}

void mapf_seek( UINT_32 offset )
{
  if (image_file)
    {
      int rc;
  
      rc = fseek( image_file, offset, SEEK_SET );
      assert( rc == 0 );
    }
  else
    {
      image_ptr = image_base + offset;
    }
}

void *mapf_read( UINT_32 bytes )
{
  if (image_file)
    {
      struct image_buffer *b;
      int n;

      b = (struct image_buffer *)malloc( sizeof( struct image_buffer ) );

      b->next = image_buffers;
      image_buffers = b;
      b->block = malloc_aligned_32( bytes );
      assert( b->block );
      n = fread( b->block, 1, bytes, image_file );
      assert( n == bytes );
      return b->block;
    }
  else
    {
      void *p = image_ptr;
      image_ptr += bytes;
      return p;
    }
}

void mapf_close( void )
{
  struct image_buffer *b, *n;
  
  for (b=image_buffers; b; b=n)
    {
      n = b->next;
      free_aligned_32( b->block );
      free( b );
    }
  if (image_file)
    fclose(image_file);
  
  image_file = NULL;
  image_base = NULL;
}

