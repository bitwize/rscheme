/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/atrace.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.4
 * File mod date:    1997-11-29 23:10:48
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          Allocation trace generator
 *------------------------------------------------------------------------*/

#include <rscheme/smemory.h>
#include <string.h>
#include <stdio.h>
#include <signal.h>
#include <rscheme/runtime.h>
#include <rscheme/gcserver.h>
#include <rscheme/atrace.h>

/* nb we can easily go through 500Mb of memory */

#define ATRACE_BUFFER 1000000
struct atrace_alloc *atrace_allocs, *atrace_base_allocs;
struct atrace_free *atrace_frees, *atrace_base_frees;
struct atrace_realt *atrace_realts, *atrace_base_realts;
struct atrace_class *atrace_classes, *atrace_base_classes;
struct atrace_exec *atrace_execs, *atrace_base_execs;

static UINT_32 atrace_class_cache[16][2] = {{1,1},{1,1},{1,1},{1,1},
    					 {1,1},{1,1},{1,1},{1,1},
    					 {1,1},{1,1},{1,1},{1,1},
    					 {1,1},{1,1},{1,1},{1,1}};

unsigned atrace_num_alloc;
unsigned atrace_buf_left;
unsigned atrace_time_sync;

#define DEFAULT_TIME_SYNC  (128)

FILE *atrace_allocs_file;
FILE *atrace_frees_file;
FILE *atrace_realts_file;
FILE *atrace_classes_file;
FILE *atrace_execs_file;

void atrace_flush( int close )
{
int i;

    fwrite( atrace_base_allocs, sizeof( struct atrace_alloc ),
    	    atrace_allocs - atrace_base_allocs,
	    atrace_allocs_file );
    fwrite( atrace_base_frees, sizeof( struct atrace_free ),
    	    atrace_frees - atrace_base_frees,
	    atrace_frees_file );
    fwrite( atrace_base_realts, sizeof( struct atrace_realt ),
    	    atrace_realts - atrace_base_realts,
	    atrace_realts_file );
    fwrite( atrace_base_classes, sizeof( struct atrace_class ),
    	    atrace_classes - atrace_base_classes,
	    atrace_classes_file );
    fwrite( atrace_base_execs, sizeof( struct atrace_exec ),
    	    atrace_execs - atrace_base_execs,
	    atrace_execs_file );

    atrace_allocs = atrace_base_allocs;
    atrace_frees = atrace_base_frees;
    atrace_realts = atrace_base_realts;
    atrace_classes = atrace_base_classes;
    atrace_execs = atrace_base_execs;
    atrace_buf_left = ATRACE_BUFFER;
    atrace_time_sync = 0;

    /* confuse the functionality of "flush" by
       also flushing the class cache */
    for (i=0; i<16; i++)
    {
	atrace_class_cache[i][0] = 1;
	atrace_class_cache[i][1] = 1;
    }
    if (close)
    {
    FILE *trf;
    struct module_descr **m;
    struct part_descr **p;
    struct function_descr **f;
    struct atrace_linktab x;
    jump_addr *a;
    
	fclose( atrace_allocs_file );
	fclose( atrace_frees_file );
	fclose( atrace_realts_file );
	fclose( atrace_classes_file );
	fclose( atrace_execs_file );
	
	printf( "Dumping link table...\n" );
	trf = os_fopen( "/tmp/atrace.linktab", "wb" );
	for (m=master_table; *m; m++)
	{
	    for (p=(*m)->parts; *p; p++)
	    {
		memset( x.part, 0, PART_NAME_LIMIT );
		if ((*p)->name)
		    strncpy( x.part, (*p)->name, PART_NAME_LIMIT );
		
		for (f=(*p)->functions; *f; f++)
		{
		    memset( x.name, 0, FN_NAME_LIMIT );
		    strncpy( x.name, (*f)->name, FN_NAME_LIMIT );
		    
		    x.num_monotones = 0;
		    for (a=(*f)->monotones; *a; a++)
			x.num_monotones++;
		    fwrite( &x, sizeof x, 1, trf );
		    fwrite( (*f)->monotones, 
			    sizeof(jump_addr), x.num_monotones, trf );
		}
	    }
	}
	fclose( trf );
    }
}

static void handle_alarm( int x )
{
    atrace_time_sync = 0;
}

void atrace_init( void )
{
struct itimerval it;

    atrace_base_allocs = malloc( sizeof( struct atrace_alloc ) 
    				* ATRACE_BUFFER );
    atrace_base_frees = malloc( sizeof( struct atrace_free ) 
    				* ATRACE_BUFFER );
    atrace_base_realts = malloc( sizeof( struct atrace_realt ) 
    				* ATRACE_BUFFER );
    atrace_base_classes = malloc( sizeof( struct atrace_class ) 
    				* ATRACE_BUFFER );
    atrace_base_execs = malloc( sizeof( struct atrace_exec ) 
    				* ATRACE_BUFFER );
    assert( atrace_base_allocs );
    assert( atrace_base_frees );
    assert( atrace_base_realts );
    assert( atrace_base_classes );
    assert( atrace_base_execs );
    atrace_allocs = atrace_base_allocs;
    atrace_frees = atrace_base_frees;
    atrace_realts = atrace_base_realts;
    atrace_classes = atrace_base_classes;
    atrace_execs = atrace_base_execs;
    atrace_buf_left = ATRACE_BUFFER;
    atrace_time_sync = 0;
    
    atrace_allocs_file = os_fopen( "/tmp/atrace.allocs", "wb" );
    atrace_frees_file = os_fopen( "/tmp/atrace.frees", "wb" );
    atrace_realts_file = os_fopen( "/tmp/atrace.realt", "wb" );
    atrace_classes_file =  os_fopen( "/tmp/atrace.classes", "wb" );
    atrace_execs_file =  os_fopen( "/tmp/atrace.execs", "wb" );

    signal( SIGALRM, handle_alarm );
    it.it_interval.tv_usec = 10000;	 /* force a sync at least
					    every 10ms, as long as we
					    are doing any allocation
					    at all */
    it.it_interval.tv_sec = 0;
    it.it_value = it.it_interval;
    setitimer( ITIMER_REAL, &it, NULL );
}

void atrace_did_alloc( POBHeader *p )
{
struct atrace_alloc *x = atrace_allocs++;
UINT_32 tc = VAL(p->pob_class), *cacheline;

    if (atrace_time_sync)
	atrace_time_sync--;
    else
    {
    struct atrace_realt *t = atrace_realts++;
    
	gettimeofday( &t->realt, NULL );
	t->alloct = atrace_num_alloc;
	atrace_time_sync = DEFAULT_TIME_SYNC;
    }

    x->size = p->pob_size;
    x->class = tc;

    cacheline = atrace_class_cache[(tc >> 2) & 15];
    if (!(cacheline[0] == tc || cacheline[1] == tc))
    {
    struct atrace_class *c = atrace_classes++;
    
	cacheline[1] = cacheline[0];
	cacheline[0] = tc;
	c->class = tc;
	memset( c->name, 0, CLASS_NAME_LIMIT );
	if (CLASS_P(p->pob_class)
	    && SYMBOL_P(class_name(p->pob_class)))
	    strncpy( c->name, 
		     symbol_text( class_name(p->pob_class) ),
		     CLASS_NAME_LIMIT );
    }
    p->name = atrace_num_alloc++;

    if (!(--atrace_buf_left))
	atrace_flush(0);
}

void atrace_did_free( gc_obj_addr p )
{
struct atrace_free *f = atrace_frees++;

    f->name = ((POBHeader *)p)->name;
    f->time = atrace_num_alloc;
    if (!(--atrace_buf_left))
	atrace_flush(0);
}

void atrace_will_execute( jump_addr addr )
{
struct atrace_exec *x = atrace_execs++;

    x->alloct = atrace_num_alloc;
    x->addr = (unsigned)addr;
    if (!(--atrace_buf_left))
	atrace_flush(0);
}
