#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/time.h>

#include <fcntl.h>
#include "fasldef.h"
#include <rscheme/smemory.h>
#include <rscheme/scheme.h>
#include <rscheme/api.h>
#include <rscheme/bcextend.h>
#include <rscheme/rlseconf.h>
#include <rscheme/regs.h>
#include <rscheme/heapi.h>
#include <rscheme/timeprof.h>

#if defined(PLATFORM_RHAPSODY) || defined(PLATFORM_NEXT)
#include <mach/mach.h>
#include <mach/mach_error.h>
#endif


UINT_8 *bc_stdio_extension( UINT_8 *pc, RS_bc_datum **args );
static jump_addr the_stub_entry( void );

void patch_code( struct FASL_Header *h );

static void fixup_fnd( struct FASL_UnswizzledFnDescr *xd )
{
  struct FASL_PartHdr *xp = xd->container;
  struct FASL_ModuleHdr *xm;
  struct part_descr *p;
  unsigned i;

  if (xd->real_fn_d) {
    return;
  }

  xm = xp->container;

  if (!xm->module)
    {
      struct module_descr *m;

      /* printf( "** mapping in %s module\n", xm->name );*/
      m = find_module( xm->name );
      if (!m) {
        char *p = strchr( xm->name, '|' );
        if (p) {
          scheme_error( "dynamic module ~s not loaded: ~a", 2, 
                        make_string( p+1 ),
                        make_string( dynamic_link_errors() ) );
        } else {
          scheme_error( "static module ~s not loaded", 1, 
                        make_string( xm->name ) );
        }
      }
      xm->module = m;
    }
  /* printf( "** mapping in %s[%d]\n", xm->name, xp->part_tag );*/
  p = find_part( xm->module, xp->part_tag );
  if (!p)
    {
      scheme_error( "module ~s is missing part ~d",
                    2,
                    make_string( xm->name ),
                    int2fx( xp->part_tag ) );
    }
  for (i=0; p->functions[i]; i++)
    {
      xp->fnds[i].real_fn_d = p->functions[i];
      xp->fnds[i].real_code_ptr = p->functions[i]->monotones[0];
    }
}


struct function_descr *fasl_function_descr_resolver( struct function_descr *f )
{
  if (f->in_part->tag == STUB_PART_TAG) {
    struct FASL_UnswizzledFnDescr *xd;
    xd = (struct FASL_UnswizzledFnDescr *)f;
    fixup_fnd( xd );
    return xd->real_fn_d;
  } else {
    return f;
  }
}

jump_addr template_unstub( obj the_template )
{
  struct FASL_UnswizzledFnDescr *xd;

  timepoint(1100);
  xd = OBJ_TO_RAW_PTR( gvec_ref( the_template, SLOT(1) ) );

  if (xd->container->stub_part_flag != STUB_PART_TAG)
    {
      /* already unstubbed */
      return OBJ_TO_JUMP_ADDR( gvec_ref( the_template, SLOT(0) ) );
    }

  if (!xd->real_code_ptr) {
    fixup_fnd( xd );
  }
  gvec_write_non_ptr( the_template, 
		    SLOT(0),
		    JUMP_ADDR_TO_OBJ( xd->real_code_ptr ) );
  gvec_write_non_ptr( the_template, 
		    SLOT(1),
		    RAW_PTR_TO_OBJ( xd->real_fn_d ) );
  timepoint(1101);

  return xd->real_code_ptr;
}

/* this function is called when a template is called
 * that hasn't been fixed up yet.  All we do it fix it
 * up and go
 */

static jump_addr the_stub_entry( void )
{
  return template_unstub( literals_reg )();
}

void patch_code( struct FASL_Header *h )
{
  timepoint(1000);
  init_stub_procs( h, (jump_addr)the_stub_entry );
  timepoint(1001);
}

#if defined(PLATFORM_RHAPSODY) || defined(PLATFORM_NEXT)

#if defined(PLATFORM_RHAPSODY)
#define task_self()  mach_task_self()
#endif

static void *map_it( const char *path, int fd, void *map_at, size_t len )
{
  kern_return_t rc;
  vm_offset_t addr;

  addr = (vm_offset_t)map_at;

  rc = vm_allocate( task_self(), &addr, len, /* anywhere */ FALSE );
  if (rc != KERN_SUCCESS)
    {
      mach_error( "vm_allocate", rc );
      fprintf( stderr, "%s: could not map at %08lx\n", 
	       path, (unsigned long)map_at );
      return NULL;
    }

  rc = map_fd( fd, 0, &addr, /*find_space*/ FALSE, len );
  if (rc != KERN_SUCCESS)
    {
      mach_error( "map_fd", rc );
      fprintf( stderr, "%s: could not map at %08lx\n", 
	       path, (unsigned long)map_at );
      return NULL;
    }
  return (void *)addr;
}

#else
static void *map_it( const char *path, int fd, void *map_at, size_t len )
{
  caddr_t rgn;
  rgn = mmap( (caddr_t)map_at,
	      len,
	      PROT_READ|PROT_WRITE|PROT_EXEC,
	      MAP_PRIVATE|MAP_FIXED,
	      fd,
	      0);
  if (rgn == (caddr_t)~0UL)
    {
      fprintf( stderr, "%s: could not map at %08lx\n", 
	       path, (unsigned long)map_at );
      return NULL;
    }
  return (void *)rgn;
}
#endif

struct FASL_Header *fasl_load( const char *path, rs_bool verboseq )
{
  struct FASL_Header temph, *h;
  struct stat s;
  int fd;
  void *rgn;

  timepoint( 100 );

  fd = open( path, O_RDONLY );
  if (fd < 0)
    {
      perror( path );
      return NULL;
    }

  fstat( fd, &s );

  if ((read( fd, &temph, sizeof( struct FASL_Header ) )
      != sizeof( struct FASL_Header ))
      || (temph.image_magic != FASL_MAGIC))
    {
      fprintf( stderr, "%s: not a FASL file\n", path );
      return NULL;
    }

  timepoint( 101 );

  rgn = map_it( path, fd, temph.pre_loaded_at, s.st_size );
  if (!rgn)
    return NULL;

  fasl_loaded_at = rgn;
  h = (struct FASL_Header *)rgn;
  timepoint( 102 );

  if (h->image_magic != FASL_MAGIC)
    {
      fprintf( stderr, "%s: not a FASL file\n", path );
      return NULL;
    }

  timepoint( 103 );

  if (verboseq && h->num_roots >= 2)
    {
      if (h->root_list[2])
	printf( "%s\n", (char *)h->root_list[2] );
    }

  /* patch up code pointers */

  timepoint( 105 );
  patch_code(h);
  timepoint( 106 );

  /* patch up <allocation-area>'s (which have code ptrs) */
  {
    AllocArea *nxt, *aa = h->first_alloc_area;
    while (aa)
      {
	nxt = (AllocArea *)aa->allocfn;
	aa->allocfn = default_alloc_obj;
	aa = nxt;
      }
  }
  timepoint( 107 );

  return h;
}


struct FASL_Header *load_fasl_heap( const char *path, rs_bool verboseq )
{
  struct FASL_Header *h;
  IRC_Heap *heap;

  timepoint( 200 );
  h = fasl_load( path, verboseq );
  if (!h)
    return NULL;
  timepoint( 201 );
  
  /* patch up other stuff */
  timepoint(202);

  heap = h->heap;
  heap->clientInfo = NULL;
  heap->moreSpacePtr = NULL;
  heap->spaceLeft = 0;
  
  heap->alloc_chunk_meth = irc_std_alloc_chunk;
  heap->alloc_big_meth = irc_std_alloc_big;

  gc_arena = h->heap;
  return h;
}

/* this function was copied from heapi/loadboot.c, where
 * it was the `load_initial_heap' that we're replacing
 */

obj plain_load_boot( const char *path, rs_bool verboseq )
{
char *gc_argv[3];
int vers;
obj r;

  gc_argv[0] = "rs";
  gc_argv[1] = verboseq ? NULL : "-q";
  gc_argv[2] = NULL;
  init_gc( verboseq ? 1 : 2, (const char **)gc_argv );
  
  /* make room for it... */
  
  gc_safe_point( 1024*1024 );
  r = load_image_file( path, FALSE_OBJ, FALSE_OBJ, &vers );
  if (truish(r))
    {
      switch (vers)
	{
	case FMTV_RSCHEME_0_5:  /* assume it's bootable */
	case FMTV_RSCHEME_0_6_BOOT:
	  break;
	default:
	  fprintf( stderr, "%s: image version %d -- not bootable\n",
		   path, vers );
	  return FALSE_OBJ;
	}
    }
  return r;
}

obj fasl_load_boot( const char *path, rs_bool verboseq )
{
  struct FASL_Header *h;

  resolve_function_descr_fn = fasl_function_descr_resolver;

  h = load_fasl_heap( path, verboseq );

  if (!h)
    return FALSE_OBJ;

  return GCPTR_TO_PTR( h->root_list[0] );
}

static UINT_32 check_magic( const char *path )
{
  struct FASL_Header h;
  int fd;

  fd = open( path, O_RDONLY );
  if (fd < 0)
    return 0;

  if (read( fd, &h, sizeof(h) ) != sizeof(h))
    {
      close(fd);
      return 0;
    }
  close(fd);
  return h.image_magic;
}

obj load_initial_heap( const char *path, rs_bool verboseq )
{
  char temp[1024];
  size_t len;

  len = strlen(path);

  if (len > 4 && (strcmp( path + len - 4, ".fas" ) == 0))
    {
      /* it's directly a fas file */

      return fasl_load_boot( path, verboseq );
    }
    else if (len > 4 && (strcmp( path + len - 4, ".img") == 0))
    {
      /* see if there is a .fas file nearby */
      struct stat s_img, s_fas;
      
      strcpy( temp, path );
      strcpy( temp + len - 3, "fas" );
      if ((stat( temp, &s_fas ) == 0)
	  && !((stat( path, &s_img ) == 0) 
	       && (s_img.st_mtime < s_fas.st_mtime)))
	{
	  return fasl_load_boot( temp, verboseq );
	}
    }
  /* check for one of our magic numbers to see if it might
   * REALLY be a fasl file
   */
  if (check_magic(path) == FASL_MAGIC)
    {
      return fasl_load_boot( path, verboseq );
    }

  /* it's a plain ol' image file */
  return plain_load_boot( path, verboseq );
}
