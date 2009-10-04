#ifndef SIZCLASS_H
#define SIZCLASS_H

#include <stdlib.h>
#include <rtgc/config.hh>

/*
   Constant definitions
*/

#define BASE 16
#define BASE_BITS 4

int get_size_class (size_t n);

int get_size_class_non_base_case (size_t n);

#endif




