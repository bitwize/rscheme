#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include "fasldef.h"
#include <rscheme/smemory.h>
#include <rscheme/scheme.h>
#include <rscheme/linktype.h>

struct fnd_record {
  struct fnd_record *next;
  struct function_descr *fn;
  unsigned monotone_num;
  struct FASL_UnswizzledFnDescr *stored_fnd;
};

struct part_accum {
  struct part_accum *next;
  struct part_descr *descr;
  struct fnd_record *fnd_first;
  struct FASL_PartHdr *stored_part;
};

struct module_accum {
  struct module_accum *next;
  struct module_descr *descr;
  struct part_accum *first_part;
  struct FASL_ModuleHdr *stored_module;
};

struct module_accum *first_module = NULL;
void write_part_info( struct part_accum *pa, struct FASL_ModuleHdr *mh );

typedef struct _linked_list {
   struct _linked_list *next;
} linked_list_t;

static unsigned ll_count( linked_list_t *h )
{
  unsigned i;

  for (i=0; h; h=h->next)
    i++;
  return i;
}

static void write_out_code_index( struct FASL_Header *hdr )
{
  unsigned i, j,  n;
  struct FASL_ModuleHdr *mh;
  struct module_accum *ma;

  n = ll_count( (linked_list_t *)first_module );

  if (fasl_verbose >= 0)
  {
    struct module_accum *p;

    printf( "%u C modules:\n  ", n );

    for (p=first_module; p; p=p->next)
    {
      printf( " %s", p->descr->name );
    }
    printf( "\n" );
  }

  mh = FASL_ALLOCN( struct FASL_ModuleHdr, n );
  hdr->module_header_table = mh;
  hdr->num_modules = n;

  for (i=0, ma=first_module; i<n; i++, ma=ma->next)
    {
      struct module_descr *m = ma->descr;
      struct FASL_PartHdr **part_ix;
      struct part_accum *pa;
      unsigned num_parts;
      char *m_name;

      ma->stored_module = &mh[i];

      if (m->loaded_from)
	{
	  /* if this module was dynamically loaded, write out the
	   * name in encoded format, including the load path
	   */
	  int n = 1
	    + strlen(m->name)
	    + strlen(m->loaded_from);
	  m_name = FASL_ALLOCN( char, n+1 );
	  sprintf( m_name, "%s|%s", 
		   m->name,
		   m->loaded_from );
	}
      else
	{
	  m_name = FASL_ALLOCN( char, strlen(m->name)+1 );
	  strcpy( m_name, m->name );
	}
      mh[i].name = m_name;

      for (pa=ma->first_part; pa; pa=pa->next)
	{
	  write_part_info( pa, &mh[i] );
	}
    }
}

void write_part_info( struct part_accum *pa,
		      struct FASL_ModuleHdr *mh )
{
  struct FASL_PartHdr *p;
  struct part_descr *local_part = pa->descr;
  unsigned i, n;
  struct fnd_record *f;

  p = FASL_ALLOC(struct FASL_PartHdr);
  pa->stored_part = p;
  p->stub_part_flag = STUB_PART_TAG; /* how to recognize a stubbed fn */
  p->part_tag = local_part->tag;
  p->container = mh;

  for (n=0; local_part->functions[n]; n++);
  p->num_fns = n;
  p->fnds = FASL_ALLOCN(struct FASL_UnswizzledFnDescr, n);

  for (i=0; i<n; i++)
    {
      p->fnds[i].real_code_ptr = NULL;
      p->fnds[i].container = NULL; /* filled in when encountered */
    }
  for (f=pa->fnd_first; f; f=f->next)
    {
      /* find out which function# this is */
      for (i=0; local_part->functions[i]; i++)
	{
	  if (local_part->functions[i] == f->fn)
	    {
	      assert( !p->fnds[i].container );
	      /* install the up-pointer */
	      p->fnds[i].container = p;
	      /* and the unswizzling pointer */
	      f->stored_fnd = &p->fnds[i];
	      break;
	    }
	}
    }
}

/*
 *  generate code pointer index
 */

static struct module_accum *get_module_accum( struct module_descr *module )
{
  struct module_accum *m;

  for (m=first_module; m; m=m->next)
    {
      if (m->descr == module)
	{
	  return m;
	}
    }
  m = MALLOC( struct module_accum );
  m->next = first_module;
  m->descr = module;
  m->first_part = NULL;
  first_module = m;
  return m;
}

static struct part_accum *get_part_accum( struct module_accum *ma, 
					  struct part_descr *part )
{
  struct part_accum *p;

  for (p=ma->first_part; p; p=p->next)
    {
      if (p->descr == part)
	return p;
    }
  p = MALLOC( struct part_accum );
  p->next = ma->first_part;
  p->descr = part;
  p->fnd_first = NULL;
  ma->first_part = p;
  return p;
}

static struct fnd_record *get_fnd_record( struct function_descr *fn,
					  unsigned monotone,
					  int update_q )
{
  struct part_descr *part;
  struct module_accum *ma;
  struct part_accum *pa;
  struct fnd_record *f;

  part = fn->in_part;
  ma = get_module_accum( part->in_module );
  pa = get_part_accum( ma, part );

  /*printf( " %s.%d (%s) ", ma->descr->name, pa->descr->tag, fn->name );*/
  for (f=pa->fnd_first; f; f=f->next)
    {
      if (f->fn == fn && f->monotone_num == monotone)
	{
	  /*printf( "ok %p\n", f );*/
	  return f;
	}
    }
  if (update_q)
    {
      f = MALLOC( struct fnd_record );
      /*printf( "new %p\n", f );*/
      f->next = pa->fnd_first;
      f->fn = fn;
      f->monotone_num = monotone;
      pa->fnd_first = f;
    }
  else
    {
      /*printf( "missed!\n" );*/
      fprintf( stderr, "internal error; fn missed in first pass!\n" );
      abort();
    }
  return f;
}

static void update_code_ptr( struct FASL_Header *hdr,
			     struct function_descr *fn,
			     unsigned monotone,
			     obj *where )
{
  struct fnd_record *fr;

  fr = get_fnd_record( fn, monotone, 0 );
  where[0] = JUMP_ADDR_TO_OBJ((jump_addr)hdr->stub_proc);
  where[1] = RAW_PTR_TO_OBJ(fr->stored_fnd);
}

static void record_code_ptr( struct function_descr *fn,
			     unsigned monotone )
{
  get_fnd_record( fn, monotone, 1 );
}

void gen_code_ptrs( struct FASL_Header *hdr, obj templates_queue )
{
  struct IRC_PtrBucket *b;
  struct IRC_Header **p, **lim;
  obj fnd;
  unsigned i, tcount;

  tcount = fx2int( dequeue_count( templates_queue ) );
  printf( "%u templates\n", tcount );

  /* Pass I.  Index all of the code pointers */

  for (i=0; i<tcount; i++) {
    obj thing = dequeue_ref( templates_queue, int2fx( i ) );
	  
    if (fasl_verbose >= 3) {
      printf( "recording code pointer in: %p\n", *p );
    }

    fnd = gvec_read( thing, SLOT(1) );
    record_code_ptr( (struct function_descr *)OBJ_TO_RAW_PTR(fnd),
                     0 );
  }

  /* Pass II.  Write out the information */
  
  write_out_code_index(hdr);

  /* Pass III. Fix up the saved code pointers */

  for (i=0; i<tcount; i++) {
    obj thing = dequeue_ref( templates_queue, int2fx( i ) );

    fnd = gvec_read( thing, SLOT(1) );
    update_code_ptr( hdr,
                     (struct function_descr *)OBJ_TO_RAW_PTR(fnd),
                     0,
                     (obj *)PTR_TO_DATAPTR(thing) );
  }
}

