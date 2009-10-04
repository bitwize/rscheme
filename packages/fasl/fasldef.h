#include <rscheme/irc.h>
#include <rscheme/smemory.h>

struct FASL_Header {
  UINT_8      skip[128];
  XUINT_32    image_magic;       /* 'RSfz' */
  XUINT_32    fasl_version;
  UINT_16     for_arch;
  UINT_16     fasl_magic_2;
  UINT_16     spare;

  UINT_32     build_date;
  UINT_32     total_size;
  void       *pre_loaded_at;
  
  void      **root_list;
  UINT_32     num_roots;
  
  /* alloc areas are chained together through their ALLOCFN ptr */
  
  AllocArea  *first_alloc_area;
  
  struct FASL_ModuleHdr *module_header_table;
  UINT_32     num_modules;
  
  IRC_Heap   *heap;  /* stored heap */

  /* needs 32-bit insns even on Alpha */
  XUINT_32     stub_proc[32];
};

/*
 *  some or all code pointers in the stored image refer
 *  to a single stub function (stub_proc), which accesses this record
 *  to fix up the code pointer and read function descriptor
 */

struct FASL_UnswizzledFnDescr {
  /* 
   * cached part lookup -- NULL in stored image
   */

  struct FASL_PartHdr *container;
  jump_addr real_code_ptr;
  struct function_descr *real_fn_d;
};

struct FASL_ModuleHdr {
    struct module_descr *module;
    char      *name;
};

struct FASL_PartHdr {
    UINT_32  stub_part_flag;   /* == STUB_PART_TAG */
    UINT_32  part_tag;
    unsigned  num_fns;
    struct FASL_UnswizzledFnDescr *fnds;
    struct FASL_ModuleHdr *container;
};


#define FASL_VERSION  (1)
#define FASL_MAGIC    (0x5253667a)  /* 'RSfz' */

#ifndef FASL_ARCH
#define FASL_ARCH   (0)
#endif

void *fasl_alloc( size_t len );
void gen_code_ptrs( struct FASL_Header *hdr, obj templates_queue );
void init_stub_procs( struct FASL_Header *hdr, jump_addr stub );

#define FASL_ALLOC(t) ((t *)fasl_alloc( sizeof(t) ))
#define FASL_ALLOCN(t,n) ((t *)fasl_alloc( (n) * sizeof(t) ))

extern void *fasl_loaded_at;

#define fasl_verbose  (0)
