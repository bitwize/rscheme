#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ubfsymtab.h"
#include "hash.h"


UBFObject *intern_const( const unsigned char *str, unsigned len )
{
  unsigned h = crc_hash( str, len );
  unsigned i = h % symtab_max;
  int looped = 0;

  while (1) {
    if (symtab[i].type != UBF_CONSTANT) {
      /* not found, and also a free slot */
      symtab[i].type = UBF_CONSTANT;
      symtab[i].tag = NULL;
      symtab[i].data.ubf_constant.text = malloc( len+1 );
      memcpy( symtab[i].data.ubf_constant.text, str, len );
      symtab[i].data.ubf_constant.text[ len ] = '\0';
      symtab[i].data.ubf_constant.hash = h;
      symtab_count++;
      return &symtab[i];
    }
    if ((symtab[i].data.ubf_constant.hash == h)
        && (memcmp( symtab[i].data.ubf_constant.text, str, len ) == 0)) {
      return &symtab[i];
    }

    i++;
    if (i == symtab_max) {
      i = 0;
      if (looped) {
        abort();
      }
      looped = 1;
    }
  }
}

void symtab_dump( void )
{
  unsigned i;

  printf( "symbol table: (size %u)\n", symtab_count );
  for (i=0; i<symtab_max; i++) {
    if (symtab[i].type == UBF_CONSTANT) {
      printf( "  <%08x>  \"%s\"\n",
              symtab[i].data.ubf_constant.hash,
              symtab[i].data.ubf_constant.text );
    }
  }
}


