/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

/******************************** Preamble ********************************/

#define _MODULE_CORELIB
#define _SCM_LINKINFO
#define _C_LINKINFO
#include "corelib_p.h"
#include <rscheme/vinsns.h>
extern struct module_descr module_corelib;
extern struct part_descr corelib_part_linkinfo;
static char sccsid[] = "@(#)corelib modules/corelib/linkinfo.scm [32815113] (RS v0.7.3.4-b7u, 2007-05-30)";

/************************** Function Definitions **************************/


/************************* Raw glue `get-c-units' *************************/

static char rsfn_get_c_units_name[] = "get-c-units";
#define FUNCTION rsfn_get_c_units_name

PROLOGUE(get_c_units)

BEGIN_FWD(get_c_units)
  FWD_MONOTONE(get_c_units_0)
END_FWD(get_c_units)

#define FPLACE_CODE (1000+0)
MONOTONE(get_c_units_0)
{
{
  REG0 = get_c_units();
  RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(get_c_units)

BEGIN_BACK(get_c_units)
  BACK_MONOTONE(get_c_units_0)
END_BACK(get_c_units)

static struct function_descr get_c_units_descr = {
	&corelib_part_linkinfo,
	JUMP_TABLE( get_c_units ),
	rsfn_get_c_units_name };
#undef FUNCTION


/*********************** Raw glue `flush-link-data' ***********************/

static char rsfn_flush_link_data_name[] = "flush-link-data";
#define FUNCTION rsfn_flush_link_data_name

PROLOGUE(flush_link_data)

BEGIN_FWD(flush_link_data)
  FWD_MONOTONE(flush_link_data_0)
END_FWD(flush_link_data)

#define FPLACE_CODE (1000+0)
MONOTONE(flush_link_data_0)
{
{
  REG0 = flush_link_data();
  RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(flush_link_data)

BEGIN_BACK(flush_link_data)
  BACK_MONOTONE(flush_link_data_0)
END_BACK(flush_link_data)

static struct function_descr flush_link_data_descr = {
	&corelib_part_linkinfo,
	JUMP_TABLE( flush_link_data ),
	rsfn_flush_link_data_name };
#undef FUNCTION


/*************** Raw glue `set-c-module-descr-loaded-from!' ***************/
#define c_module REG0
#define lf REG1

static char rsfn_set_c_module_descr_loaded_from_name[] = "set-c-module-descr-loaded-from!";
#define FUNCTION rsfn_set_c_module_descr_loaded_from_name

PROLOGUE(set_c_module_descr_loaded_from)

BEGIN_FWD(set_c_module_descr_loaded_from)
  FWD_MONOTONE(set_c_module_descr_loaded_from_0)
END_FWD(set_c_module_descr_loaded_from)

#define FPLACE_CODE (1000+0)
MONOTONE(set_c_module_descr_loaded_from_0)
{
{
  struct module_descr *m = (struct module_descr *)OBJ_TO_RAW_PTR(c_module);
  m->loaded_from = strdup( string_text( lf ) );
  RETURN0();
}}
#undef FPLACE_CODE

EPILOGUE(set_c_module_descr_loaded_from)

BEGIN_BACK(set_c_module_descr_loaded_from)
  BACK_MONOTONE(set_c_module_descr_loaded_from_0)
END_BACK(set_c_module_descr_loaded_from)

static struct function_descr set_c_module_descr_loaded_from_descr = {
	&corelib_part_linkinfo,
	JUMP_TABLE( set_c_module_descr_loaded_from ),
	rsfn_set_c_module_descr_loaded_from_name };
#undef FUNCTION

#undef c_module
#undef lf

/********************* Raw glue `get-c-module-descr' *********************/
#define c_module REG0

static char rsfn_get_c_module_descr_name[] = "get-c-module-descr";
#define FUNCTION rsfn_get_c_module_descr_name

PROLOGUE(get_c_module_descr)

BEGIN_FWD(get_c_module_descr)
  FWD_MONOTONE(get_c_module_descr_0)
END_FWD(get_c_module_descr)

#define FPLACE_CODE (1000+0)
MONOTONE(get_c_module_descr_0)
{
{
  struct module_descr *m = (struct module_descr *)OBJ_TO_RAW_PTR(c_module);
  struct part_descr **p;
  obj lst = NIL_OBJ;

  for (p=m->parts; *p; p++)
    {
      lst = cons( RAW_PTR_TO_OBJ(*p), lst );
    }
  REG0 = make_string( m->name );
  REG1 = lst;
  REG2 = int2fx( m->num_roots );
  REG3 = m->loaded_from ? make_string( m->loaded_from ) : FALSE_OBJ;
  RETURN(4);
}}
#undef FPLACE_CODE

EPILOGUE(get_c_module_descr)

BEGIN_BACK(get_c_module_descr)
  BACK_MONOTONE(get_c_module_descr_0)
END_BACK(get_c_module_descr)

static struct function_descr get_c_module_descr_descr = {
	&corelib_part_linkinfo,
	JUMP_TABLE( get_c_module_descr ),
	rsfn_get_c_module_descr_name };
#undef FUNCTION

#undef c_module

/********************** Raw glue `c-module-root-ref' **********************/
#define c_module REG0
#define ko REG1

static char rsfn_c_module_root_ref_name[] = "c-module-root-ref";
#define FUNCTION rsfn_c_module_root_ref_name

PROLOGUE(c_module_root_ref)

BEGIN_FWD(c_module_root_ref)
  FWD_MONOTONE(c_module_root_ref_0)
END_FWD(c_module_root_ref)

#define FPLACE_CODE (1000+0)
MONOTONE(c_module_root_ref_0)
{
{
  struct module_descr *m = (struct module_descr *)OBJ_TO_RAW_PTR(c_module);
  int k = fx2int(ko);

  if ((k < 0) || (k >= m->num_roots)) {
    scheme_error( "bad root index ~s", 1, ko );
  }
  REG0 = m->root_vector[ k ];
  RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(c_module_root_ref)

BEGIN_BACK(c_module_root_ref)
  BACK_MONOTONE(c_module_root_ref_0)
END_BACK(c_module_root_ref)

static struct function_descr c_module_root_ref_descr = {
	&corelib_part_linkinfo,
	JUMP_TABLE( c_module_root_ref ),
	rsfn_c_module_root_ref_name };
#undef FUNCTION

#undef c_module
#undef ko

/********************* Raw glue `c-module-root-set!' *********************/
#define c_module REG0
#define ko REG1
#define item REG2

static char rsfn_c_module_root_set_name[] = "c-module-root-set!";
#define FUNCTION rsfn_c_module_root_set_name

PROLOGUE(c_module_root_set)

BEGIN_FWD(c_module_root_set)
  FWD_MONOTONE(c_module_root_set_0)
END_FWD(c_module_root_set)

#define FPLACE_CODE (1000+0)
MONOTONE(c_module_root_set_0)
{
{
  struct module_descr *m = (struct module_descr *)OBJ_TO_RAW_PTR(c_module);
  int k = fx2int(ko);
  obj old;

  if ((k < 0) || (k >= m->num_roots)) {
    scheme_error( "bad root index ~s", 1, ko );
  }
  old = m->root_vector[k];   /* return the old value */
  m->root_vector[k] = item;
  REG0 = old;
  RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(c_module_root_set)

BEGIN_BACK(c_module_root_set)
  BACK_MONOTONE(c_module_root_set_0)
END_BACK(c_module_root_set)

static struct function_descr c_module_root_set_descr = {
	&corelib_part_linkinfo,
	JUMP_TABLE( c_module_root_set ),
	rsfn_c_module_root_set_name };
#undef FUNCTION

#undef c_module
#undef ko
#undef item

/********************** Raw glue `get-c-part-descr' **********************/
#define part_ptr REG0

static char rsfn_get_c_part_descr_name[] = "get-c-part-descr";
#define FUNCTION rsfn_get_c_part_descr_name

PROLOGUE(get_c_part_descr)

BEGIN_FWD(get_c_part_descr)
  FWD_MONOTONE(get_c_part_descr_0)
END_FWD(get_c_part_descr)

#define FPLACE_CODE (1000+0)
MONOTONE(get_c_part_descr_0)
{
{
  struct part_descr *p = (struct part_descr *)OBJ_TO_RAW_PTR(part_ptr);
  struct function_descr **f;
  obj lst = NIL_OBJ;

  for (f=p->functions; *f; f++)
    {
      lst = cons( RAW_PTR_TO_OBJ(*f), lst );
    }
  REG0 = make_string( p->name );
  REG1 = int2fx( p->tag );
  REG2 = lst;
  REG3 = RAW_PTR_TO_OBJ( p->in_module );
  REG4 = p->sccsid ? make_string( p->sccsid ) : FALSE_OBJ;
  RETURN(5);
}}
#undef FPLACE_CODE

EPILOGUE(get_c_part_descr)

BEGIN_BACK(get_c_part_descr)
  BACK_MONOTONE(get_c_part_descr_0)
END_BACK(get_c_part_descr)

static struct function_descr get_c_part_descr_descr = {
	&corelib_part_linkinfo,
	JUMP_TABLE( get_c_part_descr ),
	rsfn_get_c_part_descr_name };
#undef FUNCTION

#undef part_ptr

/******************** Raw glue `get-c-function-descr' ********************/
#define fn_d REG0

static char rsfn_get_c_function_descr_name[] = "get-c-function-descr";
#define FUNCTION rsfn_get_c_function_descr_name

PROLOGUE(get_c_function_descr)

BEGIN_FWD(get_c_function_descr)
  FWD_MONOTONE(get_c_function_descr_0)
END_FWD(get_c_function_descr)

#define FPLACE_CODE (1000+0)
MONOTONE(get_c_function_descr_0)
{
{
  struct function_descr *f = (struct function_descr *)OBJ_TO_RAW_PTR(fn_d);
  jump_addr *m;
  obj prev, lst;

  if (f->in_part->tag == STUB_PART_TAG) {
    f = resolve_function_descr( f );
  }

  lst = cons( JUMP_ADDR_TO_OBJ(f->monotones[0]), NIL_OBJ );

  prev = lst;
  for (m=f->monotones+1; *m; m++)
    {
      obj cell = cons( JUMP_ADDR_TO_OBJ(*m), NIL_OBJ );
      gvec_write_fresh_ptr( prev, SLOT(1), cell );
      prev = cell;
    }
  REG0 = make_string( f->name );
  REG1 = lst;
  REG2 = RAW_PTR_TO_OBJ( f->in_part );
  RETURN(3);
}}
#undef FPLACE_CODE

EPILOGUE(get_c_function_descr)

BEGIN_BACK(get_c_function_descr)
  BACK_MONOTONE(get_c_function_descr_0)
END_BACK(get_c_function_descr)

static struct function_descr get_c_function_descr_descr = {
	&corelib_part_linkinfo,
	JUMP_TABLE( get_c_function_descr ),
	rsfn_get_c_function_descr_name };
#undef FUNCTION

#undef fn_d

/********************* Raw glue `find-linked-module' *********************/
#define name REG0

static char rsfn_find_linked_module_name[] = "find-linked-module";
#define FUNCTION rsfn_find_linked_module_name

PROLOGUE(find_linked_module)

BEGIN_FWD(find_linked_module)
  FWD_MONOTONE(find_linked_module_0)
END_FWD(find_linked_module)

#define FPLACE_CODE (1000+0)
MONOTONE(find_linked_module_0)
{
{
  struct module_descr *m;

  m = find_module( string_text(name) );
  REG0 = m ? RAW_PTR_TO_OBJ(m) : FALSE_OBJ;
  RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(find_linked_module)

BEGIN_BACK(find_linked_module)
  BACK_MONOTONE(find_linked_module_0)
END_BACK(find_linked_module)

static struct function_descr find_linked_module_descr = {
	&corelib_part_linkinfo,
	JUMP_TABLE( find_linked_module ),
	rsfn_find_linked_module_name };
#undef FUNCTION

#undef name

/***************** Raw glue `find-part-in-linked-module' *****************/
#define module REG0
#define part REG1

static char rsfn_find_part_in_linked_module_name[] = "find-part-in-linked-module";
#define FUNCTION rsfn_find_part_in_linked_module_name

PROLOGUE(find_part_in_linked_module)

BEGIN_FWD(find_part_in_linked_module)
  FWD_MONOTONE(find_part_in_linked_module_0)
END_FWD(find_part_in_linked_module)

#define FPLACE_CODE (1000+0)
MONOTONE(find_part_in_linked_module_0)
{
{
  struct part_descr *p;

  p = find_part( (struct module_descr *)OBJ_TO_RAW_PTR(module), fx2int(part) );
  REG0 = p ? RAW_PTR_TO_OBJ(p) : FALSE_OBJ;
  RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(find_part_in_linked_module)

BEGIN_BACK(find_part_in_linked_module)
  BACK_MONOTONE(find_part_in_linked_module_0)
END_BACK(find_part_in_linked_module)

static struct function_descr find_part_in_linked_module_descr = {
	&corelib_part_linkinfo,
	JUMP_TABLE( find_part_in_linked_module ),
	rsfn_find_part_in_linked_module_name };
#undef FUNCTION

#undef module
#undef part

/******************** Raw glue `find-code-ptr-in-part' ********************/
#define part REG0
#define fn_num REG1

static char rsfn_find_code_ptr_in_part_name[] = "find-code-ptr-in-part";
#define FUNCTION rsfn_find_code_ptr_in_part_name

PROLOGUE(find_code_ptr_in_part)

BEGIN_FWD(find_code_ptr_in_part)
  FWD_MONOTONE(find_code_ptr_in_part_0)
END_FWD(find_code_ptr_in_part)

#define FPLACE_CODE (1000+0)
MONOTONE(find_code_ptr_in_part_0)
{
{
  struct function_descr *f;
  struct part_descr *p = (struct part_descr *)OBJ_TO_RAW_PTR(part);

  f = p->functions[ fx2int(fn_num) ];
  REG0 = RAW_PTR_TO_OBJ( f->monotones[0] );
  REG1 = RAW_PTR_TO_OBJ( f );
  RETURN(2);
}}
#undef FPLACE_CODE

EPILOGUE(find_code_ptr_in_part)

BEGIN_BACK(find_code_ptr_in_part)
  BACK_MONOTONE(find_code_ptr_in_part_0)
END_BACK(find_code_ptr_in_part)

static struct function_descr find_code_ptr_in_part_descr = {
	&corelib_part_linkinfo,
	JUMP_TABLE( find_code_ptr_in_part ),
	rsfn_find_code_ptr_in_part_name };
#undef FUNCTION

#undef part
#undef fn_num

/********************** Raw glue `dl-error-message' **********************/

static char rsfn_dl_error_message_name[] = "dl-error-message";
#define FUNCTION rsfn_dl_error_message_name

PROLOGUE(dl_error_message)

BEGIN_FWD(dl_error_message)
  FWD_MONOTONE(dl_error_message_0)
END_FWD(dl_error_message)

#define FPLACE_CODE (1000+0)
MONOTONE(dl_error_message_0)
{
{
  const char *txt = dynamic_link_errors();
  if (txt)
    REG0 = make_string( txt );
  else
    REG0 = LITERAL(0);
  RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(dl_error_message)

BEGIN_BACK(dl_error_message)
  BACK_MONOTONE(dl_error_message_0)
END_BACK(dl_error_message)

static struct function_descr dl_error_message_descr = {
	&corelib_part_linkinfo,
	JUMP_TABLE( dl_error_message ),
	rsfn_dl_error_message_name };
#undef FUNCTION


/************************* Raw glue `dl-resolve' *************************/
#define ent REG0
#define raw_sym REG1

static char rsfn_dl_resolve_name[] = "dl-resolve";
#define FUNCTION rsfn_dl_resolve_name

PROLOGUE(dl_resolve)

BEGIN_FWD(dl_resolve)
  FWD_MONOTONE(dl_resolve_0)
END_FWD(dl_resolve)

#define FPLACE_CODE (1000+0)
MONOTONE(dl_resolve_0)
{  char *sym;
  COUNT_ARGS(2);
  if (!STRING_P(raw_sym))
    {
      obj c;
      c = make5( TLREFB(0),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_sym, NIL_OBJ ),
                 lookup_symbol( "sym" ),
                 TLREFB(1) );
      raise_error( c );
    }
  sym = (char *)string_text(raw_sym);


{
  void *ptr;
  ptr = resolve_link_symbol(OBJ_TO_RAW_PTR(ent),sym);
  REG0 = ptr ? (RAW_PTR_TO_OBJ(ptr)) : FALSE_OBJ;
  RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(dl_resolve)

BEGIN_BACK(dl_resolve)
  BACK_MONOTONE(dl_resolve_0)
END_BACK(dl_resolve)

static struct function_descr dl_resolve_descr = {
	&corelib_part_linkinfo,
	JUMP_TABLE( dl_resolve ),
	rsfn_dl_resolve_name };
#undef FUNCTION

#undef ent
#undef raw_sym

/*************************** Raw glue `dl-done' ***************************/
#define ent REG0

static char rsfn_dl_done_name[] = "dl-done";
#define FUNCTION rsfn_dl_done_name

PROLOGUE(dl_done)

BEGIN_FWD(dl_done)
  FWD_MONOTONE(dl_done_0)
END_FWD(dl_done)

#define FPLACE_CODE (1000+0)
MONOTONE(dl_done_0)
{  COUNT_ARGS(1);

{
  done_resolving(OBJ_TO_RAW_PTR(ent));
  RETURN0();
}}
#undef FPLACE_CODE

EPILOGUE(dl_done)

BEGIN_BACK(dl_done)
  BACK_MONOTONE(dl_done_0)
END_BACK(dl_done)

static struct function_descr dl_done_descr = {
	&corelib_part_linkinfo,
	JUMP_TABLE( dl_done ),
	rsfn_dl_done_name };
#undef FUNCTION

#undef ent

/*************************** Raw glue `dl-call' ***************************/
#define cfn REG0

static char rsfn_dl_call_name[] = "dl-call";
#define FUNCTION rsfn_dl_call_name

PROLOGUE(dl_call)

BEGIN_FWD(dl_call)
  FWD_MONOTONE(dl_call_0)
END_FWD(dl_call)

#define FPLACE_CODE (1000+0)
MONOTONE(dl_call_0)
{  COUNT_ARGS(1);

{
 void *r, *(*fn)( void );

 fn = ((void *(*)(void))OBJ_TO_RAW_PTR(cfn));
 r = fn();
 REG0 = r ? (RAW_PTR_TO_OBJ(r)) : FALSE_OBJ;
 RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(dl_call)

BEGIN_BACK(dl_call)
  BACK_MONOTONE(dl_call_0)
END_BACK(dl_call)

static struct function_descr dl_call_descr = {
	&corelib_part_linkinfo,
	JUMP_TABLE( dl_call ),
	rsfn_dl_call_name };
#undef FUNCTION

#undef cfn

/************************* Raw glue `dl-install' *************************/
#define m REG0

static char rsfn_dl_install_name[] = "dl-install";
#define FUNCTION rsfn_dl_install_name

PROLOGUE(dl_install)

BEGIN_FWD(dl_install)
  FWD_MONOTONE(dl_install_0)
END_FWD(dl_install)

#define FPLACE_CODE (1000+0)
MONOTONE(dl_install_0)
{  COUNT_ARGS(1);

{
  install_module( OBJ_TO_RAW_PTR(m) );
  RETURN0();
}}
#undef FPLACE_CODE

EPILOGUE(dl_install)

BEGIN_BACK(dl_install)
  BACK_MONOTONE(dl_install_0)
END_BACK(dl_install)

static struct function_descr dl_install_descr = {
	&corelib_part_linkinfo,
	JUMP_TABLE( dl_install ),
	rsfn_dl_install_name };
#undef FUNCTION

#undef m

/*************************** Raw glue `dl-open' ***************************/
#define raw_path REG0

static char rsfn_dl_open_name[] = "dl-open";
#define FUNCTION rsfn_dl_open_name

PROLOGUE(dl_open)

BEGIN_FWD(dl_open)
  FWD_MONOTONE(dl_open_0)
END_FWD(dl_open)

#define FPLACE_CODE (1000+0)
MONOTONE(dl_open_0)
{  char *path;
  COUNT_ARGS(1);
  if (!STRING_P(raw_path))
    {
      obj c;
      c = make5( TLREFB(1),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_path, NIL_OBJ ),
                 lookup_symbol( "path" ),
                 TLREFB(2) );
      raise_error( c );
    }
  path = (char *)string_text(raw_path);


{
  void *ent;
  ent = dynamic_link_file(path);
  if (!ent)
   {
     REG0 = raw_path;
     APPLY(1,TLREF(0));
   }
  else
   {
     REG0 = RAW_PTR_TO_OBJ(ent);
     RETURN1();
   }
}}
#undef FPLACE_CODE

EPILOGUE(dl_open)

BEGIN_BACK(dl_open)
  BACK_MONOTONE(dl_open_0)
END_BACK(dl_open)

static struct function_descr dl_open_descr = {
	&corelib_part_linkinfo,
	JUMP_TABLE( dl_open ),
	rsfn_dl_open_name };
#undef FUNCTION

#undef raw_path

/************************** Raw glue `dl-c-unit' **************************/
#define raw_path REG0
#define raw_unit_name REG1

static char rsfn_dl_c_unit_name[] = "dl-c-unit";
#define FUNCTION rsfn_dl_c_unit_name

PROLOGUE(dl_c_unit)

BEGIN_FWD(dl_c_unit)
  FWD_MONOTONE(dl_c_unit_0)
END_FWD(dl_c_unit)

#define FPLACE_CODE (1000+0)
MONOTONE(dl_c_unit_0)
{  char *path;
  char *unit_name;
  COUNT_ARGS(2);
  if (!STRING_P(raw_path))
    {
      obj c;
      c = make5( TLREFB(1),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_path, NIL_OBJ ),
                 lookup_symbol( "path" ),
                 TLREFB(2) );
      raise_error( c );
    }
  path = (char *)string_text(raw_path);

  if (!STRING_P(raw_unit_name))
    {
      obj c;
      c = make5( TLREFB(1),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_unit_name, NIL_OBJ ),
                 lookup_symbol( "unit_name" ),
                 TLREFB(2) );
      raise_error( c );
    }
  unit_name = (char *)string_text(raw_unit_name);


{
  if (!dynamic_link_c_unit( path, unit_name ))
    {
      REG0 = raw_path;
      APPLY(1,TLREF(0));
    }
  else
    {
      RETURN0();
    }
}}
#undef FPLACE_CODE

EPILOGUE(dl_c_unit)

BEGIN_BACK(dl_c_unit)
  BACK_MONOTONE(dl_c_unit_0)
END_BACK(dl_c_unit)

static struct function_descr dl_c_unit_descr = {
	&corelib_part_linkinfo,
	JUMP_TABLE( dl_c_unit ),
	rsfn_dl_c_unit_name };
#undef FUNCTION

#undef raw_path
#undef raw_unit_name
/******************************* Postamble *******************************/
/**************************** Part Link Table ****************************/


static struct function_descr *(part_linkinfo_tab[]) = {
    &get_c_units_descr,
    &flush_link_data_descr,
    &set_c_module_descr_loaded_from_descr,
    &get_c_module_descr_descr,
    &c_module_root_ref_descr,
    &c_module_root_set_descr,
    &get_c_part_descr_descr,
    &get_c_function_descr_descr,
    &find_linked_module_descr,
    &find_part_in_linked_module_descr,
    &find_code_ptr_in_part_descr,
    &dl_error_message_descr,
    &dl_resolve_descr,
    &dl_done_descr,
    &dl_call_descr,
    &dl_install_descr,
    &dl_open_descr,
    &dl_c_unit_descr,
    NULL };
struct part_descr corelib_part_linkinfo = {
    32815113,
    &module_corelib,
    part_linkinfo_tab,
    "linkinfo",
    0, sccsid };
#undef _MODULE_CORELIB
#undef _SCM_LINKINFO
#undef _C_LINKINFO
