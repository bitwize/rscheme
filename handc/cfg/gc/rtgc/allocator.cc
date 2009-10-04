#include <rtgc/gc.hh>
#include <rtgc/allocator.hh>
#include <rtgc/sizclass.h>
#ifdef INLINES
#include <rtgc/inlines.hh>
#endif

colors allocation_color;
color_set *allocation_sets[NUM_SIZE_CLASSES];
color_set *allocation_set_fast_lookup[FAST_LOOKUP_TABLE_SIZE];

void gc_allocation_table_initialize(void){
    for(int i = 0; i < NUM_SIZE_CLASSES; i++){
	allocation_sets[i] = gc.get_gen(0)->get_color_set(i);
    }
    for(int size = 0 ; size < FAST_LOOKUP_TABLE_SIZE; size++){
	allocation_set_fast_lookup[size]
	    = gc.get_gen(0)
		->get_color_set(get_size_class(size+SIZE_OF_GC_OBJECT_BASE));
    }
}

#ifndef INLINES
#include <rtgc/allocator.ci>
#endif
