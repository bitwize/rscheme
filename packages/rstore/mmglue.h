#ifndef _H_MMGLUE
#define _H_MMGLUE

#include <sys/types.h>

#define MM_PAGE_BITS (13)
#define MM_PAGE_SIZE (1<<(MM_PAGE_BITS))
#define MM_PAGE_MASK (MM_PAGE_SIZE-1)

#define MM_PAGE_BASE_ADDR(p) ((void *)(((size_t)p) & ~MM_PAGE_MASK))

enum mm_mode {
    MM_MODE_NO_ACCESS,
    MM_MODE_READ_ONLY,
    MM_MODE_READ_WRITE
};

void *mm_alloc( size_t bytes, enum mm_mode mode );
void mm_free( void *base, size_t bytes );

void mm_set_prot( void *base, size_t bytes, enum mm_mode new_mode );

/*
 *  mm_unload() is like setting the protection to MM_MODE_NO_ACCESS,
 *  but also tells the OS to discard any real memory associated
 *  with the page
 */
void mm_unload( void *base, size_t bytes );

void init_mm( void );

/* provided by MM client */

void mmc_access_failed( void *addr );

#endif /* _H_MMGLUE */
