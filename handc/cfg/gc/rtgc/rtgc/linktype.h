#ifndef LINK_TYPE_H
#define LINK_TYPE_H

/* The macro LINK_TYPE is used to tell the compiler whether to compile
   with inlines, without inlines, or for non-c++ linkage.  See the Makefile
   more information about these options.
*/

#if !(__GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 7))
        typedef int bool;
        enum { false = 0, true = 1 };
#endif

#undef LINK_TYPE
#undef EXTERNAL_LINK_TYPE

#ifdef __cplusplus
   #ifdef FOREIGN
      #define LINK_TYPE extern "C"
      #define EXTERNAL_LINK_TYPE extern "C"
   #else
      #ifdef INLINES
         #define LINK_TYPE inline
         #define EXTERNAL_LINK_TYPE
      #else
         #define LINK_TYPE
         #define EXTERNAL_LINK_TYPE
      #endif
   #endif
#else
   #define LINK_TYPE
   #define EXTERNAL_LINK_TYPE
#endif

#endif
