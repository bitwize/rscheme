/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

/******************************** Preamble ********************************/

#define _MODULE_CORELIB
#define _SCM_ALLOC
#define _C_ALLOC
#include "corelib_p.h"
#include <rscheme/vinsns.h>
extern struct module_descr module_corelib;
extern struct part_descr corelib_part_alloc;
static char sccsid[] = "@(#)corelib modules/corelib/alloc.scm [14998529] (RS v0.7.3.4-b7u, 2007-05-30)";

/************************** Function Definitions **************************/


/*************** Raw glue `make-transient-allocation-area' ***************/

static char rsfn_make_transient_allocation_area_name[] = "make-transient-allocation-area";
#define FUNCTION rsfn_make_transient_allocation_area_name

PROLOGUE(make_transient_allocation_area)

BEGIN_FWD(make_transient_allocation_area)
  FWD_MONOTONE(make_transient_allocation_area_0)
END_FWD(make_transient_allocation_area)

#define FPLACE_CODE (1000+0)
MONOTONE(make_transient_allocation_area_0)
{
{
  AllocArea *aa;

  REG0 = bvec_alloc( sizeof( AllocArea ), TLREF(0) );

  aa = (AllocArea *)PTR_TO_DATAPTR(REG0);
  aa->entry = FALSE_OBJ;
  aa->reserved = FALSE_OBJ;
  aa->allocfn = default_alloc_obj;
  aa->info = NULL;  /* transients are guaranteed to have NULL info */

  RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(make_transient_allocation_area)

BEGIN_BACK(make_transient_allocation_area)
  BACK_MONOTONE(make_transient_allocation_area_0)
END_BACK(make_transient_allocation_area)

static struct function_descr make_transient_allocation_area_descr = {
	&corelib_part_alloc,
	JUMP_TABLE( make_transient_allocation_area ),
	rsfn_make_transient_allocation_area_name };
#undef FUNCTION


/********************* Raw glue `allocation-area-op' *********************/
#define raw_area REG0
#define raw_opcode REG1
#define data REG2

static char rsfn_allocation_area_op_name[] = "allocation-area-op";
#define FUNCTION rsfn_allocation_area_op_name

PROLOGUE(allocation_area_op)

BEGIN_FWD(allocation_area_op)
  FWD_MONOTONE(allocation_area_op_0)
END_FWD(allocation_area_op)

#define FPLACE_CODE (1000+0)
MONOTONE(allocation_area_op_0)
{  AllocArea *area;
  int opcode;
  COUNT_ARGS(3);
  if (!OBJ_ISA_PTR_OF_CLASS(raw_area,TLREFB(0)))
    {
      obj c;
      c = make5( TLREFB(1),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_area, NIL_OBJ ),
                 lookup_symbol( "area" ),
                 TLREFB(0) );
      raise_error( c );
    }
  area = (AllocArea *)PTR_TO_DATAPTR(raw_area);

  opcode = basic_raw_int(raw_opcode);


{
  obj old = FALSE_OBJ;

  switch (opcode)
    {
    case 0: /* get-entry */
      old = area->entry;
      break;
    case 1: /* set-entry */
      old = area->entry;
      gvec_set( raw_area, SLOT(0), data );
      break;
    case 2: /* get-reserved */
      old = area->reserved;
      break;
    case 3: /* set-reserved */
      old = area->reserved;
      gvec_set( raw_area, SLOT(1), data );
      break;
    }
  REG0 = old;
  RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(allocation_area_op)

BEGIN_BACK(allocation_area_op)
  BACK_MONOTONE(allocation_area_op_0)
END_BACK(allocation_area_op)

static struct function_descr allocation_area_op_descr = {
	&corelib_part_alloc,
	JUMP_TABLE( allocation_area_op ),
	rsfn_allocation_area_op_name };
#undef FUNCTION

#undef raw_area
#undef raw_opcode
#undef data

/************************** Raw glue `make-gvec' **************************/
#define raw_the_class REG0

static char rsfn_make_gvec_name[] = "make-gvec";
#define FUNCTION rsfn_make_gvec_name

PROLOGUE(make_gvec)

BEGIN_FWD(make_gvec)
  FWD_MONOTONE(make_gvec_0)
END_FWD(make_gvec)

#define FPLACE_CODE (1000+0)
MONOTONE(make_gvec_0)
{  obj the_class;
  COUNT_ARGS_AT_LEAST(1);
  if (!CLASS_P(raw_the_class))
    {
      obj c;
      c = make5( TLREFB(0),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_the_class, NIL_OBJ ),
                 lookup_symbol( "the_class" ),
                 TLREFB(1) );
      raise_error( c );
    }
  the_class = raw_the_class;


{
unsigned i;

    REG0 = alloc( SLOT(arg_count_reg-1), the_class );

    for (i=1; i<arg_count_reg; i++)
	gvec_write_init( REG0, SLOT(i-1), reg_ref(i) );
    
    RETURN(1);
}}
#undef FPLACE_CODE

EPILOGUE(make_gvec)

BEGIN_BACK(make_gvec)
  BACK_MONOTONE(make_gvec_0)
END_BACK(make_gvec)

static struct function_descr make_gvec_descr = {
	&corelib_part_alloc,
	JUMP_TABLE( make_gvec ),
	rsfn_make_gvec_name };
#undef FUNCTION

#undef raw_the_class

/*********************** Raw glue `area-make-gvec' ***********************/
#define raw_area REG0
#define raw_the_class REG1

static char rsfn_area_make_gvec_name[] = "area-make-gvec";
#define FUNCTION rsfn_area_make_gvec_name

PROLOGUE(area_make_gvec)

BEGIN_FWD(area_make_gvec)
  FWD_MONOTONE(area_make_gvec_0)
END_FWD(area_make_gvec)

#define FPLACE_CODE (1000+0)
MONOTONE(area_make_gvec_0)
{  AllocArea *area;
  obj the_class;
  COUNT_ARGS_AT_LEAST(2);
  if (!OBJ_ISA_PTR_OF_CLASS(raw_area,TLREFB(0)))
    {
      obj c;
      c = make5( TLREFB(1),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_area, NIL_OBJ ),
                 lookup_symbol( "area" ),
                 TLREFB(0) );
      raise_error( c );
    }
  area = (AllocArea *)PTR_TO_DATAPTR(raw_area);

  if (!CLASS_P(raw_the_class))
    {
      obj c;
      c = make5( TLREFB(1),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_the_class, NIL_OBJ ),
                 lookup_symbol( "the_class" ),
                 TLREFB(2) );
      raise_error( c );
    }
  the_class = raw_the_class;


{
unsigned i;

    REG0 = alloc_in_area( area, the_class, SLOT(arg_count_reg-2) );

    for (i=2; i<arg_count_reg; i++)
      {
	gvec_write_non_ptr( REG0, SLOT(i-2), ZERO );
	/* note we can't use gvec_write_init here, because that's
	   only valid for the default alloc area */
	gvec_write( REG0, SLOT(i-2), reg_ref(i) );
      }
    
    RETURN(1);
}}
#undef FPLACE_CODE

EPILOGUE(area_make_gvec)

BEGIN_BACK(area_make_gvec)
  BACK_MONOTONE(area_make_gvec_0)
END_BACK(area_make_gvec)

static struct function_descr area_make_gvec_descr = {
	&corelib_part_alloc,
	JUMP_TABLE( area_make_gvec ),
	rsfn_area_make_gvec_name };
#undef FUNCTION

#undef raw_area
#undef raw_the_class

/************************* Raw glue `make-gvec*' *************************/
#define raw_the_class REG0
#define maybe_last REG1

static char rsfn_make_gvec1_name[] = "make-gvec*";
#define FUNCTION rsfn_make_gvec1_name

PROLOGUE(make_gvec1)

BEGIN_FWD(make_gvec1)
  FWD_MONOTONE(make_gvec1_0)
END_FWD(make_gvec1)

#define FPLACE_CODE (1000+0)
MONOTONE(make_gvec1_0)
{  obj the_class;
  COUNT_ARGS_AT_LEAST(2);
  if (!CLASS_P(raw_the_class))
    {
      obj c;
      c = make5( TLREFB(0),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_the_class, NIL_OBJ ),
                 lookup_symbol( "the_class" ),
                 TLREFB(1) );
      raise_error( c );
    }
  the_class = raw_the_class;


{
unsigned i, n;

    n = expand_last();
    REG0 = alloc( SLOT(n-1), the_class );
    
    for (i=1; i<n; i++)
	gvec_write_init( REG0, SLOT(i-1), reg_ref(i) );
    
    RETURN(1);
}}
#undef FPLACE_CODE

EPILOGUE(make_gvec1)

BEGIN_BACK(make_gvec1)
  BACK_MONOTONE(make_gvec1_0)
END_BACK(make_gvec1)

static struct function_descr make_gvec1_descr = {
	&corelib_part_alloc,
	JUMP_TABLE( make_gvec1 ),
	rsfn_make_gvec1_name };
#undef FUNCTION

#undef raw_the_class
#undef maybe_last

/*********************** Raw glue `area-make-gvec*' ***********************/
#define raw_area REG0
#define raw_the_class REG1
#define maybe_last REG2

static char rsfn_area_make_gvec1_name[] = "area-make-gvec*";
#define FUNCTION rsfn_area_make_gvec1_name

PROLOGUE(area_make_gvec1)

BEGIN_FWD(area_make_gvec1)
  FWD_MONOTONE(area_make_gvec1_0)
END_FWD(area_make_gvec1)

#define FPLACE_CODE (1000+0)
MONOTONE(area_make_gvec1_0)
{  AllocArea *area;
  obj the_class;
  COUNT_ARGS_AT_LEAST(3);
  if (!OBJ_ISA_PTR_OF_CLASS(raw_area,TLREFB(0)))
    {
      obj c;
      c = make5( TLREFB(1),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_area, NIL_OBJ ),
                 lookup_symbol( "area" ),
                 TLREFB(0) );
      raise_error( c );
    }
  area = (AllocArea *)PTR_TO_DATAPTR(raw_area);

  if (!CLASS_P(raw_the_class))
    {
      obj c;
      c = make5( TLREFB(1),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_the_class, NIL_OBJ ),
                 lookup_symbol( "the_class" ),
                 TLREFB(2) );
      raise_error( c );
    }
  the_class = raw_the_class;


{
unsigned i, n;

    n = expand_last();
    REG0 = alloc( SLOT(n-2), the_class );
    
    for (i=2; i<n; i++)
      {
	gvec_write_non_ptr( REG0, SLOT(i-2), FALSE_OBJ );
	gvec_write( REG0, SLOT(i-2), reg_ref(i) );
      }
    
    RETURN(1);
}}
#undef FPLACE_CODE

EPILOGUE(area_make_gvec1)

BEGIN_BACK(area_make_gvec1)
  BACK_MONOTONE(area_make_gvec1_0)
END_BACK(area_make_gvec1)

static struct function_descr area_make_gvec1_descr = {
	&corelib_part_alloc,
	JUMP_TABLE( area_make_gvec1 ),
	rsfn_area_make_gvec1_name };
#undef FUNCTION

#undef raw_area
#undef raw_the_class
#undef maybe_last

/************************* Raw glue `area-clone' *************************/
#define raw_area REG0
#define raw_new_class REG1
#define source REG2

static char rsfn_area_clone_name[] = "area-clone";
#define FUNCTION rsfn_area_clone_name

PROLOGUE(area_clone)

BEGIN_FWD(area_clone)
  FWD_MONOTONE(area_clone_0)
END_FWD(area_clone)

#define FPLACE_CODE (1000+0)
MONOTONE(area_clone_0)
{  AllocArea *area;
  obj new_class;
  COUNT_ARGS(3);
  if (!OBJ_ISA_PTR_OF_CLASS(raw_area,TLREFB(2)))
    {
      obj c;
      c = make5( TLREFB(3),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_area, NIL_OBJ ),
                 lookup_symbol( "area" ),
                 TLREFB(2) );
      raise_error( c );
    }
  area = (AllocArea *)PTR_TO_DATAPTR(raw_area);

  if (!CLASS_P(raw_new_class))
    {
      obj c;
      c = make5( TLREFB(3),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_new_class, NIL_OBJ ),
                 lookup_symbol( "new_class" ),
                 TLREFB(4) );
      raise_error( c );
    }
  new_class = raw_new_class;


{
  obj newobj = FALSE_OBJ;

  if (!OBJ_ISA_PTR(source))
    {
      if (EQ(new_class,object_class(source)))
	newobj = source;
      else
	scheme_error( string_text(LITERAL(0)), 2, source, new_class );
    }
  else
    {
      int ht1 = fx2int( gvec_ref( CLASSOF_PTR(source), SLOT(1) ) );
      int ht2 = fx2int( gvec_ref( new_class, SLOT(1) ) );
      UINT_32 i, len = SIZEOF_PTR(source);

      if (ht1 != ht2)
	scheme_error( string_text(LITERAL(0)), 2, source, new_class );
	
      newobj = alloc_in_area( area, new_class, len );
      
      switch (ht1)
	{
	case 0:
	  /* gvec */
	  for (i=0; i<len; i+=SLOT(1))
	    {
	      gvec_write_non_ptr( newobj, i, FALSE_OBJ );
	      gvec_set( newobj, i, gvec_ref( source, i ) );
	    }
	  break;

	case 1:
	  /* bvec */
	  /* copy the bytes, including the whole last word */

	  memcpy( PTR_TO_DATAPTR(newobj), 
		  PTR_TO_DATAPTR(source), 
		  ((len - 1) | (sizeof(UINT_32)-1)) + 1 );
	  break;
	
	default:
	  scheme_error( string_text( LITERAL(1) ), 1, source );
	}
    }
  REG0 = newobj;
  RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(area_clone)

BEGIN_BACK(area_clone)
  BACK_MONOTONE(area_clone_0)
END_BACK(area_clone)

static struct function_descr area_clone_descr = {
	&corelib_part_alloc,
	JUMP_TABLE( area_clone ),
	rsfn_area_clone_name };
#undef FUNCTION

#undef raw_area
#undef raw_new_class
#undef source

/****************** Raw glue `register-for-finalization' ******************/
#define thing REG0

static char rsfn_register_for_finalization_name[] = "register-for-finalization";
#define FUNCTION rsfn_register_for_finalization_name

PROLOGUE(register_for_finalization)

BEGIN_FWD(register_for_finalization)
  FWD_MONOTONE(register_for_finalization_0)
END_FWD(register_for_finalization)

#define FPLACE_CODE (1000+0)
MONOTONE(register_for_finalization_0)
{  COUNT_ARGS(1);

{
   mark_as_finalizable(thing);
   RETURN0();
}}
#undef FPLACE_CODE

EPILOGUE(register_for_finalization)

BEGIN_BACK(register_for_finalization)
  BACK_MONOTONE(register_for_finalization_0)
END_BACK(register_for_finalization)

static struct function_descr register_for_finalization_descr = {
	&corelib_part_alloc,
	JUMP_TABLE( register_for_finalization ),
	rsfn_register_for_finalization_name };
#undef FUNCTION

#undef thing

/************************* Raw glue `gc-cycle-id' *************************/

static char rsfn_gc_cycle_id_name[] = "gc-cycle-id";
#define FUNCTION rsfn_gc_cycle_id_name

PROLOGUE(gc_cycle_id)

BEGIN_FWD(gc_cycle_id)
  FWD_MONOTONE(gc_cycle_id_0)
END_FWD(gc_cycle_id)

#define FPLACE_CODE (1000+0)
MONOTONE(gc_cycle_id_0)
{
{
   REG0 = get_gc_cycle_id();
   RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(gc_cycle_id)

BEGIN_BACK(gc_cycle_id)
  BACK_MONOTONE(gc_cycle_id_0)
END_BACK(gc_cycle_id)

static struct function_descr gc_cycle_id_descr = {
	&corelib_part_alloc,
	JUMP_TABLE( gc_cycle_id ),
	rsfn_gc_cycle_id_name };
#undef FUNCTION


/*************************** Raw glue `gc-work' ***************************/
#define raw_amt REG0

static char rsfn_gc_work_name[] = "gc-work";
#define FUNCTION rsfn_gc_work_name

PROLOGUE(gc_work)

BEGIN_FWD(gc_work)
  FWD_MONOTONE(gc_work_0)
END_FWD(gc_work)

#define FPLACE_CODE (1000+0)
MONOTONE(gc_work_0)
{  int amt;
  COUNT_ARGS(1);
  amt = basic_raw_int(raw_amt);


{
  gc_work( (amt < 0) ? 0 : amt );
  RETURN0();
}}
#undef FPLACE_CODE

EPILOGUE(gc_work)

BEGIN_BACK(gc_work)
  BACK_MONOTONE(gc_work_0)
END_BACK(gc_work)

static struct function_descr gc_work_descr = {
	&corelib_part_alloc,
	JUMP_TABLE( gc_work ),
	rsfn_gc_work_name };
#undef FUNCTION

#undef raw_amt

/*************************** Raw glue `gc-now' ***************************/

static char rsfn_gc_now_name[] = "gc-now";
#define FUNCTION rsfn_gc_now_name

PROLOGUE(gc_now)

BEGIN_FWD(gc_now)
  FWD_MONOTONE(gc_now_0)
END_FWD(gc_now)

#define FPLACE_CODE (1000+0)
MONOTONE(gc_now_0)
{
{
   gc_now();
   RETURN0();
}}
#undef FPLACE_CODE

EPILOGUE(gc_now)

BEGIN_BACK(gc_now)
  BACK_MONOTONE(gc_now_0)
END_BACK(gc_now)

static struct function_descr gc_now_descr = {
	&corelib_part_alloc,
	JUMP_TABLE( gc_now ),
	rsfn_gc_now_name };
#undef FUNCTION


/********************** Raw glue `live-object-stats' **********************/

static char rsfn_live_object_stats_name[] = "live-object-stats";
#define FUNCTION rsfn_live_object_stats_name

PROLOGUE(live_object_stats)

BEGIN_FWD(live_object_stats)
  FWD_MONOTONE(live_object_stats_0)
END_FWD(live_object_stats)

#define FPLACE_CODE (1000+0)
MONOTONE(live_object_stats_0)
{
{
   scheme_error( "live-stats: not implemented", 0 );
   RETURN0();
}}
#undef FPLACE_CODE

EPILOGUE(live_object_stats)

BEGIN_BACK(live_object_stats)
  BACK_MONOTONE(live_object_stats_0)
END_BACK(live_object_stats)

static struct function_descr live_object_stats_descr = {
	&corelib_part_alloc,
	JUMP_TABLE( live_object_stats ),
	rsfn_live_object_stats_name };
#undef FUNCTION


/************************ Raw glue `all-instances' ************************/
#define of_class REG0

static char rsfn_all_instances_name[] = "all-instances";
#define FUNCTION rsfn_all_instances_name

PROLOGUE(all_instances)

BEGIN_FWD(all_instances)
  FWD_MONOTONE(all_instances_0)
END_FWD(all_instances)

#define FPLACE_CODE (1000+0)
MONOTONE(all_instances_0)
{
{
  flush_stack_cache();
  if (arg_count_reg < 2)
     gc_now();
  REG0 = all_instances(of_class);
  RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(all_instances)

BEGIN_BACK(all_instances)
  BACK_MONOTONE(all_instances_0)
END_BACK(all_instances)

static struct function_descr all_instances_descr = {
	&corelib_part_alloc,
	JUMP_TABLE( all_instances ),
	rsfn_all_instances_name };
#undef FUNCTION

#undef of_class

/*********************** Raw glue `all-pointers-to' ***********************/
#define item REG0

static char rsfn_all_pointers_to_name[] = "all-pointers-to";
#define FUNCTION rsfn_all_pointers_to_name

PROLOGUE(all_pointers_to)

BEGIN_FWD(all_pointers_to)
  FWD_MONOTONE(all_pointers_to_0)
END_FWD(all_pointers_to)

#define FPLACE_CODE (1000+0)
MONOTONE(all_pointers_to_0)
{
{
  flush_stack_cache();
  if (arg_count_reg < 2)
     gc_now();
  REG0 = all_pointers_to(item);
  RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(all_pointers_to)

BEGIN_BACK(all_pointers_to)
  BACK_MONOTONE(all_pointers_to_0)
END_BACK(all_pointers_to)

static struct function_descr all_pointers_to_descr = {
	&corelib_part_alloc,
	JUMP_TABLE( all_pointers_to ),
	rsfn_all_pointers_to_name };
#undef FUNCTION

#undef item

/********************** Raw glue `relocate-objects' **********************/
#define tbl REG0

static char rsfn_relocate_objects_name[] = "relocate-objects";
#define FUNCTION rsfn_relocate_objects_name

PROLOGUE(relocate_objects)

BEGIN_FWD(relocate_objects)
  FWD_MONOTONE(relocate_objects_0)
END_FWD(relocate_objects)

#define FPLACE_CODE (1000+0)
MONOTONE(relocate_objects_0)
{
{
  REG0 = int2fx( rs_relocate_objects(tbl) );
  RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(relocate_objects)

BEGIN_BACK(relocate_objects)
  BACK_MONOTONE(relocate_objects_0)
END_BACK(relocate_objects)

static struct function_descr relocate_objects_descr = {
	&corelib_part_alloc,
	JUMP_TABLE( relocate_objects ),
	rsfn_relocate_objects_name };
#undef FUNCTION

#undef tbl
/******************************* Postamble *******************************/
/**************************** Part Link Table ****************************/


static struct function_descr *(part_alloc_tab[]) = {
    &make_transient_allocation_area_descr,
    &allocation_area_op_descr,
    &make_gvec_descr,
    &area_make_gvec_descr,
    &make_gvec1_descr,
    &area_make_gvec1_descr,
    &area_clone_descr,
    &register_for_finalization_descr,
    &gc_cycle_id_descr,
    &gc_work_descr,
    &gc_now_descr,
    &live_object_stats_descr,
    &all_instances_descr,
    &all_pointers_to_descr,
    &relocate_objects_descr,
    NULL };
struct part_descr corelib_part_alloc = {
    14998529,
    &module_corelib,
    part_alloc_tab,
    "alloc",
    0, sccsid };
#undef _MODULE_CORELIB
#undef _SCM_ALLOC
#undef _C_ALLOC
