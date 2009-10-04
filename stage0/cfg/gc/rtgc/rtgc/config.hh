#ifndef CONFIG_HH
#define CONFIG_HH


#ifndef SCHEME_GC
#define SCHEME_GC
#endif


//#define C_PLUS_PLUS_GC 

/* use a generational collector.  Currently, this is a noop, but should */
/* still be defined.                                                    */

#define GENERATIONAL
//#define HARD_REAL_TIME
//#define SNAPSHOT_WRITE_BARRIER
#define INCREMENTAL_UPDATE_WRITE_BARRIER

/* Tell the collector what color to allocate new objects. */

#define ALLOCATE_BLACK
//#define ALLOCATE_WHITE



/* This file contains compile time parameters for the garbage collector. */
/* These can be set to change the performance characteristics of the     */
/* collector.                                                            */

#ifdef GENERATIONAL
#define VERSION_STRING "Generational Real-Time garbage collector V0.4 (19 April 1996)"
#else
#define VERSION_STRING "Real-Time garbage collector V0.4 (19 April 1996)"
#endif

/* The default is for one generation and one step. */
/* Since the generational code isn't yet implemented, these macros should */
/* be used as defined below without change.                               */

#ifdef GENERATIONAL
#ifndef NUMBER_OF_GENERATIONS
#define NUMBER_OF_GENERATIONS 2
#endif
#else
#ifndef NUMBER_OF_GENERATIONS
#define NUMBER_OF_GENERATIONS 1
#endif
#endif

#ifndef NUMBER_OF_STEPS
#define NUMBER_OF_STEPS 2
#endif

#ifndef SBRK_POINTER_LIST_SIZE
#define SBRK_POINTER_LIST_SIZE 256
#endif

/*
// WORD_OF_ONE defines one machine word where all of the bits are set.
// it is used to mask of bits from pointer fields.  This needs to be
// changed if the system is to compile and run on a non 32-bit machine.
*/

#ifndef WORD_OF_ONE
#define WORD_OF_ONE 0xffffffff
#endif

/* NUM_IGP is the number of intergenerational pointers allowed at any */
/* given time.                                                        */

#ifndef NUM_IGP
#define NUM_IGP 100000
#endif

/*
// NUM_SIZE_CLASSES determines how many different size classes there will
// be in each generation.
*/

#ifndef NUM_SIZE_CLASSES
#define NUM_SIZE_CLASSES 32
#endif

/* The next constant is the initial amount of memory per size class     */
/* currently this parameter is unused, but it may be used in the future */

#ifndef NUM_PAGES_PER_SIZE_CLASS
#define NUM_PAGES_PER_SIZE_CLASS 32
#endif

/* The maximum amount of data that will be live at a time.  One version of */
/* the real-time collector uses this fact to set the collection rate.  The */
/* other (which we currently implement) sets the collection rate to be     */
/* one times the allocation rate (the same as the allocation rate), and    */
/* this parameter is not used.                                             */

#ifndef MAX_LIVE
#define MAX_LIVE 299008
#endif

/* STORED_INTO_LIST_SIZE sets the maximum size of the stored into list */
/* which is used by the write barrier.  50000 should be large enough   */
/* for any application, but if the system does a lot of mutations,     */
/* and the parameter AMOUNT_TO_ALLOCATE_PER_INCREMENT is set very     */
/* high, then this value may need to be increased.                     */
/* STORED_INTO_LIST_SIZE must be a power of 2.                         */

#ifndef STORED_INTO_LIST_SIZE
#define STORED_INTO_LIST_BITS 18
#define STORED_INTO_LIST_SIZE (1 << STORED_INTO_LIST_BITS)
#define STORED_INTO_LIST_MASK (STORED_INTO_LIST_SIZE - 1)
#endif


/* the constant NUM_HEAP_PAGES should be set to the total number of pages */
/* to be allocated in the heap.                                           */

#ifndef NUM_HEAP_PAGES
#define NUM_HEAP_PAGES 32768
#endif


/* PAGE_SIZE_BITS is a measure of the logical page size for garbage        */
/* collected pages.  A page size is 2^PAGE_SIZE_BITS.  A page is carved    */
/* into objects of idintical size.  The larger the page size, the more     */
/* identical sized objects are created, which might increase fragmentation */
/* There is an object manager for every page of memory, so the page size   */
/* probably should not be too small (There is also overhead for any        */
/* objects over one page size large).                                      */

#ifndef PAGE_SIZE_BITS
#define PAGE_SIZE_BITS 12
#endif

/* defines the size of a heap page in bytes. */

#ifndef GC_PAGE_SIZE
#define GC_PAGE_SIZE (1 << PAGE_SIZE_BITS)
#endif

/*
// PAGE_MASK is a mask used to find the excess bits on a page.  It should have
// all ones in the bit fields representing one page size (PAGE_SIZE) and 
// zeros everywhere else.
*/

#ifndef PAGE_MASK
#define PAGE_MASK ((1<<PAGE_SIZE_BITS)-1)
#endif


/* AMOUNT_TO_ALLOCATE_PER_INCREMENT is the number of bytes to allocate */
/* between each increment of collection work.                           */

#ifndef AMOUNT_TO_ALLOCATE_PER_INCREMENT
#define AMOUNT_TO_ALLOCATE_PER_INCREMENT 8192
#endif

/* When a size class runs out of memory, it must allocate more memory  */
/* from the heap.  NUM_PAGES_TO_SNARF_AT_A_TIME is the number of heap  */
/* pages to allocate each time more memory is needed for a size class. */

#ifndef NUM_PAGES_TO_SNARF_AT_A_TIME
#define NUM_PAGES_TO_SNARF_AT_A_TIME 15
#endif

/*
// The next define if defined, states that there will never be any
// derived pointers in the appliction program.
*/

#ifndef NO_DERIVED_POINTERS
#ifdef SCHEME_GC
#define NO_DERIVED_POINTERS
#endif
#endif


/* Tell the collector to do a depth or breadth first traversal of the */
/* program pointer graph.                                             */

#define DEPTH_FIRST
/*#define BREADTH_FIRST*/

/* Tells the collector that no write barrier will be called for assignments */
/* into root variables.  This allows the write barrier to be optimized      */
/* a little.                                                                */

#ifndef NO_ROOT_WRITE_BARRIER
#ifdef SCHEME_GC
#define NO_ROOT_WRITE_BARRIER
#endif
#endif

/* If NO_ROOT_POINTERS is defined, you are guarenteeing to the collector that */
/* no pointer passed to gc_next_object points to a root object.  This is      */
/* usefull for languages like scheme, but should not be defined for C++ as    */
/* pointers into the root set are quite common for C++.                       */

#ifdef SCHEME_GC
#define NO_ROOT_POINTERS
#endif

/* #define HEAP_ALLOCATED_GC */

#ifndef SMALL_GC_HEADERS
#define SMALL_GC_HEADERS
#endif

/* NOTE!!! if the size of the garbage collection headers is changed, */
/* then this variable must be changed.  Its correct value is checked */
/* with an assertion check in the init_gc routine                    */

#ifdef SMALL_GC_HEADERS
#define SIZE_OF_GC_OBJECT_BASE 8
#else
#define SIZE_OF_GC_OBJECT_BASE 16
#endif


#ifndef GC_TURN_OFF_ABLE
#define GC_TURN_OFF_ABLE
#endif


/* GC_TURN_OFF_ABLE can only be used when allocating black */

#if defined(ALLOCATE_WHITE) || defined(GENERATIONAL)
#ifdef GC_TURN_OFF_ABLE
#undef GC_TURN_OFF_ABLE
#endif
#endif

#ifdef GENERATIONAL
#define DEFAULT_THROTTLE_SETTING 1.5
#else
#define DEFAULT_THROTTLE_SETTING 0.5
#endif

#ifndef PROFILE_PAUSES
/* #define PROFILE_PAUSES */
#endif

#ifndef PROFILE_WRITE_BARRIER
/* #define PROFILE_WRITE_BARRIER */
#endif

#ifndef SPTR_CONTAINS_OBJ_HDR
/* #define SPTR_CONTAINS_OBJ_HDR */
#endif

#ifndef TEST_LIFETIME_DIST
/* #define TEST_LIFETIME_DIST */
#endif

#ifndef _H_RSCHEME_PLATFORM
typedef long int INT_32;
typedef unsigned long int UINT_32;
#endif

typedef char BYTE;


/*
#define DEBUG_LEVEL1
#define DEBUG_LEVEL2
*/


#endif

