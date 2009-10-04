#include <rscheme.h>

typedef struct _LZWStream LZWStream;

LZWStream *lzw_create( obj port );
void lzw_run( LZWStream *s, unsigned char *ptr, unsigned len );

