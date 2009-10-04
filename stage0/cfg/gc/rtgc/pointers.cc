#include <rtgc/pointers.hh>

#ifdef INLINES
#include <rtgc/inlines.hh>
#endif

gc_object_base *pointing_object;

#ifdef DEBUG_LEVEL2
void follow_pointers_check_from_black(gc_object_base *object){
        // Save the argument to a global variable and call find_pointers.
    // The saved argument is used by the gc_next_object.
    pointing_object = object;
    find_pointers_from_black((gc_obj_addr)(((char *) object)
					   + sizeof(gc_object_base)));
}

void follow_pointers_check_from_non_black(gc_object_base *object){
    // Save the argument to a global variable and call find_pointers.
    // The saved argument is used by the gc_next_object.
    pointing_object = object;
    find_pointers_from_non_black((gc_obj_addr)(((char *) object)
					       + sizeof(gc_object_base)));
}
#endif

#ifndef INLINES
#include <rtgc/pointers.ci>
#endif
