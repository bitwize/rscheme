#ifndef _H_UBFSYMTAB
#define _H_UBFSYMTAB

#include "ubf.h"

extern unsigned symtab_count;
extern unsigned symtab_max;
extern UBFObject symtab[];

UBFObject *intern_const( const unsigned char *str, unsigned len );

void symtab_dump( void );

#endif /* _H_UBFSYMTAB */
