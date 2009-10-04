/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/linktype.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.14
 * File mod date:    2003-08-20 13:38:30
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          C-unit linkage
 *------------------------------------------------------------------------*
 * Notes:
 *      Data structures used to pickle and unpickle code pointers,
 *      and to store C-level information about code modules
 *------------------------------------------------------------------------*/

#ifndef _H_RSCHEME_LINKTYPE
#define _H_RSCHEME_LINKTYPE

#include <rscheme/obj.h>
#include <rscheme/bcextend.h>

struct module_descr;
struct part_descr;
struct function_descr;

/* 
 *  note: the FASL recognizes stubbed out procedures
 *  by having a function_descr which points to a
 *  part_descr with tag >= STUB_PART_TAG (== 2^29)
 */

#define STUB_PART_TAG  (1<<29)

struct part_descr {
    UINT_32		  tag;             /* must be 1st, to work w/FASL */
    struct module_descr   *in_module;
    struct function_descr **functions;
    const char 		  *name;
    UINT_32		  unswizzled_as;	/* for use during saving */
    const char            *sccsid;
};

struct function_descr {
    struct part_descr   *in_part;    /* must be first, to work with FASL */
    jump_addr 		*monotones;
    const char 		*name;
};

struct root_info {
    int			slot;
    const char		*name;
};

struct bcx_descr {
  UINT_8               extn_code;
  RS_bc_extension_fn  *handler;
  char                *name;
  struct module_descr *owner;
};

struct module_descr {
  const char            *name;
  struct part_descr    **parts;
  unsigned	         num_roots;
  obj		        *root_vector;
  struct root_info      *roots;
  struct bcx_descr      *bc_extensions;
  unsigned               num_bc_extensions;
  char                  *loaded_from;      /* NULL for statically loaded */

  /* filled in by install_module() */
  obj                    name_hash;
  struct module_descr   *next;
};

void init_linkage( struct module_descr **initial_table );

void install_module( struct module_descr *m );

struct unit_linkage_data {
  char    *name;
  UINT_8  *link_meta_data;
  UINT_32  link_meta_data_cnt;
  UINT_8  *link_unit_data;
  UINT_32  link_unit_data_cnt;
  struct unit_linkage_data *next;
};

void register_link_data( struct unit_linkage_data *lnk );
obj flush_link_data( void ); /* returns a <vector> of link data info */

obj get_c_units( void );  /* returns a <vector> of raw module_descr ptrs */

struct unit_root_iterator {
  struct module_descr  *mod;
  int	                root_num;
  int	                dir_index;
};

void init_unit_root_iterator( struct unit_root_iterator *iter );
obj *unit_root_iterator_next( struct unit_root_iterator *iter );

struct module_descr *find_module( const char *module );
struct part_descr *find_part( struct module_descr *m, unsigned tag );

#ifdef PLATFORM_AIX
struct export_table {
    char *name;
    void *value;
};
#endif

/* install_bc_extension() is implemented in bci/bcinterp.c, 
   but is private (only called from install_module & init_linkage)
*/

void install_bc_extension( struct bcx_descr *extn );


/* used to un-stub-out a template, which is recognized
 * by being in a part with tag >= STUB_PART_TAG
 *
 * library provides a default, error-signalling version which
 * is useful for the base system which never encounters such
 * a part tag
 */

jump_addr template_unstub( obj the_template );

/*
 *  internal low-level interface for dynamic linking of code
 */

void *resolve_link_symbol( void *info, const char *sym );
void *dynamic_link_file( const char *path );
void done_resolving( void *info );
void init_dynamic_link( const char *argv0 );

struct module_descr *dynamic_link_c_unit( const char *path, 
					  const char *unit_name );

typedef struct function_descr *function_descr_resolver( struct function_descr *f );

extern function_descr_resolver *resolve_function_descr_fn;
struct function_descr *resolve_function_descr( struct function_descr *f );

#endif /* _H_RSCHEME_LINKTYPE */
