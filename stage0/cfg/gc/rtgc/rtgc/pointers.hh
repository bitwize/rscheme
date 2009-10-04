#ifndef POINTERS_HH
#define POINTERS_HH
#include <rtgc/config.hh>
#include <rtgc/colorset.hh>
#include <rtgc/gcclient.h>

extern gc_object_base *pointing_object;

LINK_TYPE UINT_32 follow_pointers(UINT_32 max_bytes_to_scan);
#ifdef DEBUG_LEVEL2
void follow_pointers_check_from_black(gc_object_base *ptr);
void follow_pointers_check_from_non_black(gc_object_base *ptr);
#endif
#endif // POINTERS_HH
