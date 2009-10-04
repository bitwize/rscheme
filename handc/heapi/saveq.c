/*-----------------------------------------------------------------*-C-*---
 * File:    handc/heapi/saveq.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.6
 * File mod date:    1997-11-29 23:10:44
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 *------------------------------------------------------------------------*/

#include <stdlib.h>
#include <rscheme/smemory.h>
#include "saveq.h"

void hi_init_queue_n( SaveQueue *q, UINT_32 init_cap )
{
  q->count = 0;
  q->cap = init_cap;
  q->contents = (struct queue_entry *)
    malloc( init_cap * sizeof(struct queue_entry) );
  q->ptr = 0;
}

void hi_init_queue( SaveQueue *q )
{
  hi_init_queue_n( q, 32 );
}

void hi_enqueue_item2( SaveQueue *q, obj item, obj orig_class )
{
  if (q->count >= q->cap)
    {
      q->cap *= 2;
      q->contents = (struct queue_entry *)
	realloc( q->contents, q->cap * sizeof(struct queue_entry) );
    }
  q->contents[q->count].thing = item;
  q->contents[q->count].orig_class = orig_class;
  q->count++;
}

void hi_enqueue_item( SaveQueue *q, obj item )
{
  hi_enqueue_item2( q, item, CLASSOF_PTR(item) );
}

struct queue_entry *hi_dequeue_item( SaveQueue *q )
{
  if (q->ptr < q->count)
    return &q->contents[q->ptr++];
  else
    return NULL;
}


void hi_free_queue( SaveQueue *q )
{
  free( q->contents );
}
