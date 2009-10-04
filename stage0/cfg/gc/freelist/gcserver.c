/*-----------------------------------------------------------------*-C-*---
 * File:    handc/cfg/gc/freelist/gcserver.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.5
 * File mod date:    1997-11-29 23:10:47
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          freelist garbage collector
 *------------------------------------------------------------------------*/

#include <rscheme/smemory.h>
#ifdef INLINES
#include <rscheme/gcxverse.ci>
#endif
#include <rscheme/gcserver.h>
#include <rscheme/gcclient.h>
#include <rscheme/regs.h>

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* #define DEBUG_0 */
/* #define MEM_TRACE */

#define WEAK_POINTERS

#ifdef WEAK_POINTERS
#define MAX_FINALIZING  (1000)
#define MAX_WEAK_PTRS (10000)

static lang_weak_ptr_addr weak_pointer_set[MAX_WEAK_PTRS];
static unsigned num_weak_pointers;

void gc_weak_pointer( lang_weak_ptr_addr wpa )
{
    printf( "[%u] weak pointer at: %#x\n", num_weak_pointers, (unsigned)wpa );
    weak_pointer_set[ num_weak_pointers++ ] = wpa;
}
#endif /* WEAK_POINTERS */

#ifndef INLINES
#include "gcserver1.ci"
#include "gcserver2.ci"
#include "gcxverse.ci"
#endif

#ifndef DEBUG_BACKTRACK
#undef MACH_BACKTRACK_SERVER
#endif

#ifdef MACH_BACKTRACK_SERVER
void init_backtracking_service( void );
#endif /* MACH_BACKTRACK_SERVER */

#define TRUE 1
#define FALSE 0

#define DEFAULT_GC_CYCLE_BYTES (512*1024)

MEMSizeClass size_classes[NUM_SIZE_CLASSES];
MEMSizeClass other_size_class;
static void freeup( MEMSizeClass *c );
static void gc_traverse_all( void );

int gc_cycle_bytes;
int gc_space_left;
static const char *gc_message;

extern char *getenv( const char * );

void MEMInit( void )
{
unsigned i;
MEMSizeClass *sz;
char *s;

    gc_message = getenv( "ON_GC" );
    s = getenv("GC_CYCLE_K");
    if (s)
    {
	if (s[0] == 'b')
	{
	    gc_cycle_bytes = atoi( s+1 );
	    printf( "GC_CYCLE_K=%d bytes\n", gc_cycle_bytes );
	}
	else
	{
	    gc_cycle_bytes = atoi( s );
	    if (gc_cycle_bytes < 1)
	    {
		fprintf( stderr, "GC_CYCLE_K=%s: cycle less than 1K\n", s );
		gc_cycle_bytes = 1;
	    }
	    gc_cycle_bytes *= 1024;
	}
    }
    else
	gc_cycle_bytes = DEFAULT_GC_CYCLE_BYTES;

    for (i=0; i<NUM_SIZE_CLASSES; i++)
    {
        sz = &size_classes[i];
        sz->free = NULL;
        sz->alloced = NULL;
	sz->item_size = (i + 1) * BYTES_PER_SIZE_CLASS;
	sz->alloc_num = 8192 / (sizeof(MEMHeader) + sz->item_size);
    }
    gc_space_left = gc_cycle_bytes;

#ifdef MACH_BACKTRACK_SERVER
    init_backtracking_service();
#endif /* MACH_BACKTRACK_SERVER */
}

MEMHeader *gc_alloc_other( size_t bytes )
{
MEMHeader *p;

    p = (MEMHeader *)malloc_aligned_32( sizeof(MEMHeader) + bytes );
    p->next = NULL;
    return p;
}

MEMHeader *MEMGrabMore( size_t bytes, MEMSizeClass *sz )
{
unsigned i, n, chunk, per_item;
void *mem; 
MEMHeader *next, *p;

    if (sz == &other_size_class)
	return gc_alloc_other( bytes );

    n = sz->alloc_num;
    per_item = sz->item_size + sizeof(MEMHeader);
    chunk = n * per_item;
    mem = malloc_aligned_32( chunk );
    assert( mem );
    p = (MEMHeader *)mem;
    next = sz->free;
    for (i=0; i<n; i++)
    {
        p->next = next;
	next = p;
	p = (MEMHeader *)(per_item + (char *)p);
    }
    return next;	/* our caller will adjust sz->free */
}



/*
   This is the most insanely brain-dead garbage
   collector you can imagine.  It's not even
   incremental!!
*/

/*
   had to turn this into a explicit-state management
   (as opposed to just re-entering) because of MONDO
   stack overflows (SEGV w/2Mb stack!)
*/

#define GC_BUCKET_SIZE (400)

struct GCBucket {
    struct GCBucket *next;
    void	*(contents[GC_BUCKET_SIZE+1]);
};

static struct GCBucket *free_bucket_list = NULL;

static struct GCBucket *alloc_bucket( void )
{
struct GCBucket *b;

    if (free_bucket_list)
    {
	b = free_bucket_list;
	free_bucket_list = b->next;
    }
    else
	b = (struct GCBucket *)malloc_aligned_32( sizeof( struct GCBucket ) );
    return b;
}

static void free_bucket( struct GCBucket *b )
{
    b->next = free_bucket_list;
    free_bucket_list = b;
}

#ifdef WEAK_POINTERS
static gc_obj_addr finalizable_set[MAX_FINALIZING];
static unsigned num_finalizable = 0;

static gc_obj_addr finalizing_set[MAX_FINALIZING];
static unsigned num_finalizing = 0;

gc_obj_addr gc_next_finalizing_object( void )
{
    if (num_finalizing)
      return finalizing_set[--num_finalizing];
    else
      return NULL;
}

void gc_register_finalizable_object( gc_obj_addr thing )
{
    finalizable_set[ num_finalizable++ ] = thing;
}
#endif /* WEAK_POINTERS */

static struct GCBucket *next_bucket;	/* next bucket to empty */

static struct GCBucket *current_bucket;	/* bucket being filled up */
static void **current_bucket_slot, **current_bucket_limit;

static void flush_bucket( void )
{
    *current_bucket_slot++ = NULL;
    current_bucket->next = next_bucket;
    next_bucket = current_bucket;
    current_bucket = NULL;
}

void MEMGCNextObject( gc_obj_addr item )
{
MEMHeader *h;

    h = &((MEMHeader *)item)[-1];
    h->black_flag = TRUE;
    /* add this object to the queue */
    if (!current_bucket)
    {
	current_bucket = alloc_bucket();
	current_bucket_slot = current_bucket->contents;
	current_bucket_limit = current_bucket_slot + GC_BUCKET_SIZE;
    }
    *current_bucket_slot++ = item;
    if (current_bucket_slot >= current_bucket_limit)
	flush_bucket();
}
  
rs_bool report_gc = NO;


void MEMCollectGarbage( size_t ensure_space )
{
gc_obj_addr p;
unsigned i;

    if ((gc_space_left < 0) && (gc_cycle_bytes > 5000))
    {
	OUT_CALL(
	    fprintf( stderr, "WARNING: Headroom exhausted by %d\n", 
			     -gc_space_left );
	);
    }
    if (gc_message)
    {
	printf( gc_message );
	fflush( stdout );
    }
    if (report_gc)
    {
	OUT_CALL(
	    printf( "Garbage collecting..." );
	    fflush( stdout );
	);
    }

    /* Garbage detection */
    
    next_bucket = NULL;
    current_bucket = NULL;
#ifdef WEAK_POINTERS
    num_weak_pointers = 0;
#endif
    stable_root_reset();
    for (p=stable_root_next(); p; p=stable_root_next())
    {
	gc_next_object( NULL, p );
    }

    quasistable_root_reset();
    for (p=quasistable_root_next(); p; p=quasistable_root_next())
    {
	gc_next_object( NULL, p );
    }

    unstable_root_reset();
    for (p=unstable_root_next(); p; p=unstable_root_next())
    {
	gc_next_object( NULL, p );
    }

#ifdef WEAK_POINTERS
    for (i=0; i<num_finalizing; i++)
        gc_next_object( NULL, finalizing_set[i] );
#endif

    gc_traverse_all();

#ifdef WEAK_POINTERS
    /* check for new objects to be finalized */

    for (i=0; i<num_finalizable; i++)
    {
    gc_obj_addr p = finalizable_set[i];
    MEMHeader *h = ((MEMHeader *)p)-1;
    
        if (!h->black_flag)
	  {
	    finalizable_set[i] = finalizable_set[--num_finalizable];
	    i--;
	    finalizing_set[num_finalizing++] = p;
	    gc_next_object(NULL,p);
	    printf( "Finalizable object died: %#x\n", (unsigned)p );
	  }
    }

    gc_traverse_all();

    /* Weak Pointer Clearing */

    for (i=0; i<num_weak_pointers; i++)
      {
	lang_weak_ptr_addr wp = weak_pointer_set[i];
	gc_obj_addr p;
	MEMHeader *h;

	p = (gc_obj_addr)lang_deref_weak_pointer_addr( wp );
	if (p)
	  {
	    h = ((MEMHeader *)p)-1;
	    if (!h->black_flag)
	      {
		printf( "weak pointer at %#x: died\n", (unsigned)wp );
		lang_clear_weak_pointer( wp );
	      }
	    else
		printf( "weak pointer at %#x: still alive\n", (unsigned)wp );
	  }
      }
#endif
    /* Garbage collection */

    {
    unsigned i;
    
	for (i=0; i<NUM_SIZE_CLASSES; i++)
	    freeup( &size_classes[i] );
    }
    
    freeup( &other_size_class );
    {
    MEMHeader *b, *next;
    
	OUT_CALL(
	    b = other_size_class.free;
	    while (b)
	    {
		next = b->next;
		free_aligned_32(b);
		b = next;
	    }
	    other_size_class.free = NULL;
	);
    }
    
    if (report_gc)
    {
	OUT_CALL( 
	    printf( "collected some nodes\n" );
	);
    }
    gc_space_left = (ensure_space < gc_cycle_bytes) 
                     ? gc_cycle_bytes : ensure_space;
}

static void freeup( MEMSizeClass *c )
{
MEMHeader *prev, *next, *block;

    block = c->alloced;
    prev = NULL;
    while (block)
    {
	next = block->next;
	if (!block->black_flag)
	{
#ifdef ATRACE
	    atrace_did_free( block+1 );
#endif
#ifdef VALIDATE_BLOCKS
	    block->black_flag = 0xDEADC0DE;
#endif
#ifdef MEM_DEBUG
	    { UINT_32 *x = (UINT_32 *)(block+1);
	      UINT_32 i;
	      
		for (i=0; i<c->item_size; i+=4)
		    *x++ = 0xDEADD00D;
	    }  
#endif	    
	    block->next = c->free;
	    c->free = block;
	    if (prev)
		prev->next = next;
	    else
		c->alloced = next;
	}
	else
	{
	    block->black_flag = FALSE;
	    prev = block;
	}
	block = next;
    }
}

#ifdef DEBUG_BACKTRACK

unsigned find_live_word_in_sc( MEMSizeClass *c, void *word )
{
MEMHeader *block;
void **p;
unsigned i, n, N=0;

    block = c->alloced;
    while (block)
      {
	p = (void **)(block+1);
	n = block->actual_size;
	for (i=0; i<n; i+=sizeof(void*))
	  {
	    if (*p++ == word)
	      {
		printf( "    %#x found in object at %#x (offset %u)\n", 
		       (unsigned)word,
		       (unsigned)(block+1), 
		       i );
		N++;
		break;
	      }
	  }
	block = block->next;
      }
    return N;
}

void find_live_word( void *word )
{
unsigned i, N = 0;

    for (i=0; i<NUM_SIZE_CLASSES; i++)
        N += find_live_word_in_sc( &size_classes[i], word );
    N += find_live_word_in_sc( &other_size_class, word );
}

#ifdef MACH_BACKTRACK_SERVER
#include "bktrksvr.c"
#include "bktrksvc.c"
#endif

#endif /* DEBUG_BACKTRACK */

/* no write barrier:  this is stop-and-collect */

static void gc_traverse_all( void )
{
gc_obj_addr p;

    /* traversal:  loop until no more buckets left */
    
    while (next_bucket || current_bucket)
    {
    struct GCBucket *bucket;
    void **qp;
    rs_bool did_flush;
    
	if (!next_bucket)
	{
	void **base = current_bucket->contents;
	
	    if (current_bucket_slot < (base + GC_BUCKET_SIZE/4))
	    {
	    int n = 0;
	    
		/* a different strategy... use the bucket as a stack */
	    
		while (!next_bucket && current_bucket_slot > base)
		{
		    p = *--current_bucket_slot;
		    find_pointers( p );
		    n++;
		}
		if (report_gc)
		{
		    OUT_CALL(
			printf( "1*%d", n );
			fflush( stdout );
		    );
		}
		if (!next_bucket)
		    break;
		did_flush = NO;
	    }
	    else
	    {
		did_flush = YES;
		flush_bucket(); /* steal the partially full bucket */
	    }
	}
	else
	{
	    did_flush = NO;
	}

	bucket = next_bucket;
	next_bucket = bucket->next;

	if (report_gc)
	{
	int n = 0;
	
	    qp = bucket->contents;
	    while (*qp)
	    {
		n++;
		qp++;
	    }
	    OUT_CALL(
		printf( "%s(%d) ", did_flush ? "F" : "", n );
		fflush( stdout );
	    );
	}
	
	qp = bucket->contents;
	while (*qp)
	    find_pointers( *qp++ );
	free_bucket( bucket );
    }
}

void gc_full_collect( void )
{
    MEMCollectGarbage(0);
}
