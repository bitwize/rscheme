#ifndef OBJMGR_HH
#define OBJMGR_HH

#include <rtgc/config.hh>
#include <rtgc/gc.hh>

class gc_object_base;  // forward declaration

// The class object_manager provides default information on how to shade and
// move around objects  within a particular page of the heap.  If different
// functionality is desired, simply inheriate from this class, and make the
// corresponding function virtual.
//
// Note:  The object manager can not have a pointer to the color set for the
//        objects that it manages.  It must use information local to the
//        objects themselves.  This is because any given page may contain
//        objects in different generations, and hence in different color sets.

class object_manager {
// ******* Variables
  protected:
    int size_class;
    unsigned int object_start_mask; // a mask to find the start of the object
                                    // within the starting page of the object.
    unsigned int page_offset;       // number of bytes back (whole pages) to 
                                    // the start of the object.

#ifdef TEST_FRAGMENTATION
    int num_objects_in_use;
    int num_objects_on_page;
#endif

// ******** Member functions
  public:
    object_manager(INT_32 page_number, int size_class);
    inline int get_size_class(void) {return(size_class);}
    gc_object_base *find_start_of_object(const void *);
    gc_object_base *derive_start_of_object(const void *);

};

#endif
