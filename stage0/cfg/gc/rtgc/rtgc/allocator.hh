#ifndef ALLOCATOR_HH
#define ALLOCATOR_HH
#include <rtgc/config.hh>
#include <rtgc/sizclass.h>
#include <rtgc/colorset.hh>

/*This function sets up the allocation tables. Called by the contructor of
gc.*/
extern void gc_allocation_table_initialize(void);

/* This variable holds the allocation color. */
extern colors allocation_color;
/* This array contains the pointers to the colorsets used for the allocation.*/
/* They are usually set to gc.generation[0].color_set.*/
/* That is, allocation_sets[i] == gc.generation(0)->color_set(i) */
/* In order to get the allocation set for the object with size n, */
/* you should use the colorset:  */
/* allocation_sets[get_size_class(n+sizeof(gc_obj_base))] */
extern color_set *allocation_sets[NUM_SIZE_CLASSES];
/* For smaller objects, we don't spend extra time to convert the object size */
/* to the size class and then get the allocation set. To avoid this, we */
/* prepare the allocation set pointer table for small objects. */
/* If the object size n is smaller than FAST_LOOKUP_TABLE_SIZE, */
/* we can use allocation_set_fast_lookup[n] for the allocation set. */
#define FAST_LOOKUP_TABLE_SIZE 256
extern color_set *allocation_set_fast_lookup[FAST_LOOKUP_TABLE_SIZE];
#endif ALLOCATOR_HH
