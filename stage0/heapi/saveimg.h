/*-----------------------------------------------------------------*-C-*---
 * File:    handc/heapi/saveimg.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.6
 * File mod date:    2003-10-13 13:02:28
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          Definitions for heap image saving (traversal/output interface)
 *------------------------------------------------------------------------*/

#ifndef _H_HEAPI_SAVEIMG
#define _H_HEAPI_SAVEIMG

#include <stdio.h>
#include "saveq.h"
#include <rscheme/imagfile.h>

#define TEMPLATE_CODE_PTR	SLOT(0)
#define TEMPLATE_LINKAGE	SLOT(1)

#define SYMBOL_STR		SLOT(0)

#define PART_DESCR_PART_TAG	SLOT(1)
#define PART_DESCR_MODULE_NAME	SLOT(0)

#define NUM_CLASS_MODES         (10)

struct writer_info {
  void            (*queue_writer)( SaveQueue *q );
  enum load2_mode mode2;
};

extern struct writer_info hi_writers[NUM_CLASS_MODES];

void hi_init_output( FILE *f, obj refs_vec, obj ref_names, obj root,
		     SaveQueue *used_refs, UINT_32 n_objects );
void hi_output_refs( SaveQueue *q );
void hi_done_output( void );
void hi_output_mode2( enum load2_mode mode2,
		      SaveQueue **queues, unsigned num_queues );

/*    Find out if the given PTR is to be replaced by some other PTR
 */

static _rs_inline obj replace_ptr( obj thing )
{
  obj c;
  assert( OBJ_ISA_PTR(thing) );

  c = PTR_TO_HDRPTR(thing)->pob_class;

  if (OBJ_ISA_IMMOB(c))
    {
      return OBJ(VAL(c) - IMMOB_TAG + POINTER_TAG);
    }
  return thing;
}

#endif /* _H_HEAPI_SAVEIMG */
