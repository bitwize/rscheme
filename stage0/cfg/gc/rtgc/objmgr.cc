#include <rtgc/objmgr.hh>

#ifdef INLINES
#include <rtgc/inlines.hh>
#endif
//*****************************************************************
//* object_manager::object_manager
//*
//* This is the constructor for object managers.  It computes the
//* mask needed to derive the start of an object that it manages.

object_manager::object_manager(INT_32 page_number, int size_class_)
{
    if (size_class_ > PAGE_SIZE_BITS)
    {
	object_start_mask = WORD_OF_ONE << PAGE_SIZE_BITS;
    } else {
	object_start_mask = WORD_OF_ONE << size_class_;
    }
    size_class = size_class_;
    page_offset = page_number * GC_PAGE_SIZE;
#ifdef TEST_FRAGMENTATION
    num_objects_in_use = 0;
    num_objects_on_page = GC_PAGE_SIZE >> size_class_;
#endif
}

#ifndef INLINES
#include <rtgc/objmgr.ci>
#endif
