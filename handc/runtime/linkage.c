/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/linkage.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.14
 * File mod date:    2003-10-13 13:02:02
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          `C' linkage unit utility functions
 *------------------------------------------------------------------------*/

#include <string.h>
#include <stdlib.h>
#include <rscheme/runtime.h>
#include <rscheme/smemory.h>
#include <rscheme/scheme.h>
#include <rscheme/allocns.h>

#if 0
#ifdef INLINES
#include "gcclient1.ci"
#include "gcclient2.ci"
#else
#include <rscheme/gcclient.h>
#endif
#endif

/* a hand-coded hash table of C units */

#define UNIT_HASHDIR_SIZE  (29)

static struct module_descr *(unit_hashdir[UNIT_HASHDIR_SIZE]) = {
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };
static unsigned num_units = 0;

static rs_bool original_modules = YES;
    
void install_module( struct module_descr *new_module )
{
  obj h = raw_bytes_hash( new_module->name, strlen(new_module->name) );
  unsigned i, n, x;

  new_module->name_hash = h;
  x = ((UINT_32)fx2int(h)) % UNIT_HASHDIR_SIZE;

  new_module->next = unit_hashdir[x];
  unit_hashdir[x] = new_module;
  num_units++;

  n = new_module->num_bc_extensions;
  for (i=0; i<n; i++)
    install_bc_extension( &new_module->bc_extensions[i] );
  
  /* clear its roots */
  for (i=0; i<new_module->num_roots; i++)
    new_module->root_vector[i] = FALSE_OBJ;
}

static struct module_descr *find_module1( const char *module )
{
  struct module_descr *m;
  unsigned x;
  obj h = raw_bytes_hash( module, strlen(module) );

  x = ((UINT_32)fx2int(h)) % UNIT_HASHDIR_SIZE;

  for (m=unit_hashdir[x]; m; m=m->next)
    {
      if (EQ(m->name_hash,h) && (strcmp(m->name,module) == 0))
	{
	  /* we've found the module */
	  return m;
	}
    }
  return NULL;
}

obj get_c_units( void )
{
  obj v;
  unsigned i, j;
  struct module_descr *m;

  v = alloc( SLOT(num_units), vector_class );

  j = 0;
  for (i=0; i<UNIT_HASHDIR_SIZE; i++)
    {
      for (m=unit_hashdir[i]; m; m=m->next)
	{
	  gvec_write_init_non_ptr( v, SLOT(j), RAW_PTR_TO_OBJ(m) );
	  j++;
	}
    }
  return v;
}

void init_unit_root_iterator( struct unit_root_iterator *iter )
{
  iter->mod = NULL;
  iter->root_num = 0;
  iter->dir_index = -1;
}

obj *unit_root_iterator_next( struct unit_root_iterator *iter )
{
top:
  if (iter->mod)
    {
      if (iter->root_num < iter->mod->num_roots)
	{
	  return &iter->mod->root_vector[(iter->root_num)++];
	}
      iter->mod = iter->mod->next;
      iter->root_num = 0;
      goto top;
    }
  if (iter->dir_index < (UNIT_HASHDIR_SIZE-1))
    {
      iter->dir_index++;
      iter->mod = unit_hashdir[iter->dir_index];
      iter->root_num = 0;
      goto top;
    }
  return NULL;
}


/**********************************************************************/

static unsigned num_link_data_entries = 0;
static struct unit_linkage_data *link_top = NULL;

void register_link_data( struct unit_linkage_data *lnk )
{
  lnk->next = link_top;
  link_top = lnk;
  num_link_data_entries++;
}

obj flush_link_data( void )
{
  obj e, v = alloc( SLOT(num_link_data_entries), vector_class );
  struct unit_linkage_data *p;
  unsigned i;

  for (i=num_link_data_entries, p=link_top; p; p=p->next)
    {
      char t1[30], t2[30];

      i--;
      sprintf( t1, "memory:%lu", (unsigned long)p->link_meta_data );
      sprintf( t2, "memory:%lu", (unsigned long)p->link_unit_data );

      e = make3( vector_class,
		 make_string( p->name ),
		 make_string( t1 ),
		 make_string( t2 ) );
      gvec_write_init_ptr( v, SLOT(i), e );
    }

  link_top = NULL;
  num_link_data_entries = 0;

  return v;
}

/**********************************************************************/

void init_linkage( struct module_descr **initial_table )
{
  struct module_descr **m;

  for (m=initial_table; *m; m++)
    install_module( *m );
}

struct module_descr *dynamic_link_c_unit( const char *path,
					  const char *unit_name )
{
  void *handle;
  struct module_descr *ent, *(*entfn)( void );
  char ent_name[1024];

  handle = dynamic_link_file(path);
  if (!handle)
    {
      return NULL;
    }

  sprintf( ent_name, "RS_fm_%s", unit_name );
  entfn = resolve_link_symbol( handle, ent_name );
  done_resolving( handle );

  if (!entfn)
    {
      return NULL;
    }
  ent = entfn();

  ent->loaded_from = malloc( strlen(path)+1 );
  strcpy( ent->loaded_from, path );

  install_module( ent );
  return ent;
}

struct part_descr *find_part( struct module_descr *m, unsigned tag )
{
struct part_descr **p;

    for (p=m->parts; *p; p++)
    {
	if ((*p)->tag == tag)
	{
	    /* we've found the part */
	    return *p;
	}
    }
    return NULL;
}

function_descr_resolver *resolve_function_descr_fn = NULL;

struct function_descr *resolve_function_descr( struct function_descr *f )
{
  if (resolve_function_descr_fn) {
    return resolve_function_descr_fn( f );
  } else {
    scheme_error( "no resolve_function_descr_fn", 0 );
    return NULL;
  }
}

/*
 *  interpret a C unit name like "foo|/bar/baz/libfoo.so"
 *  as meaning the C unit "foo" as loaded from /bar/baz/libfoo.so
 *  (in case it's not loaded already, we can link it in)
 */

struct module_descr *find_module( const char *m_name )
{
  const char *b = strchr( m_name, '|' );
  if (b)
    {
      char *temp = (char *)malloc( b - m_name + 1 );
      struct module_descr *m;

      memcpy( temp, m_name, b-m_name );
      temp[b-m_name] = 0;

      m = find_module1( temp );
      if (!m)
	{
	  m = dynamic_link_c_unit( b+1, temp );
	}
      return m;
    }
  else
    {
      return find_module1( m_name );
    }
}

int process_module_roots( process_root_fn *fn, void *info )
{
  struct module_descr *p;
  unsigned i, n;
  obj *r;
  
  for (i=0; i<UNIT_HASHDIR_SIZE; i++)
    for (p=unit_hashdir[i]; p; p=p->next)
      {
	r = p->root_vector;
	n = p->num_roots;
	while (n > 0)
	  {
	    int rc = fn( r++, info );
	    if (rc)
	      return rc;
	    n--;
	  }
      }
  return 0;
}
