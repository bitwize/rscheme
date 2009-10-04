#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "ubf.h"
#include "ubfsymtab.h"

#define STACK_MAX       (200)
#define VECMEM_MAX      (3000)
#define STRMEM_MAX      (33000)
#define MEM_MAX         (1000)

#define uassert(expr)   do { if (!(expr)) { ubf->badstuff( __LINE__, # expr ); } } while(0)

struct _UBFReader {
  UBFObject *(binreg[256]);
  UBFObject *memptr;
  UBFObject **sp;
  unsigned structsp;
  unsigned char *ptr;
  void *state;
  void (*callback)( UBFObject *datum, void *info );
  void *callback_info;
  void (*badstuff)( int line, const char *text );

  unsigned char *str_heap;
  
  long long accum;
  int is_neg;
  char strterm;
  unsigned char *str_start;
  unsigned char *str_accum;
  UBFObject **vecmem_ptr;
  UBFObject *(stack[STACK_MAX]);
  unsigned char str_mem[STRMEM_MAX];
  UBFObject **(structs[STACK_MAX]);
  UBFObject *(vec_mem[VECMEM_MAX]);
  UBFObject mem[MEM_MAX];
};

void ubf_set_error_handler( UBFReader *ubf, 
                            void (*abortfn)( int line, const char *msg ) )
{
  ubf->badstuff = abortfn;
}

UBFObject **ubf_allocvec( UBFReader *ubf, unsigned n )
{
  UBFObject **p = ubf->vecmem_ptr;
  ubf->vecmem_ptr += n;
  uassert( ubf->vecmem_ptr <= &ubf->vec_mem[VECMEM_MAX] );
  return p;
}

UBFObject *ubf_alloc( UBFReader *ubf )
{
  UBFObject *p = ubf->memptr++;
  /*
  printf( "memptr = %p mem = %p max = %p (n=%d)\n",
          ubf->memptr,
          &ubf->mem[0],
          &ubf->mem[MEM_MAX],
          ubf->memptr - &ubf->mem[0]);
  */
  uassert( ubf->memptr <= &ubf->mem[MEM_MAX] );
  p->tag = NULL;
  return p;
}

UBFObject *ubf_mkbin( UBFReader *buf, const void *data, unsigned len )
{
  UBFObject *u = ubf_alloc( buf );
  u->type = UBF_BINARY;
  u->data.ubf_binary.ptr = (void *)data;
  u->data.ubf_binary.len = len;
  return u;
}

UBFObject *ubf_mkstringl( UBFReader *buf, const char *text, unsigned len )
{
  UBFObject *u = ubf_alloc( buf );
  u->type = UBF_STRING;
  u->data.ubf_string.ptr = (void *)text;
  u->data.ubf_string.len = len;
  return u;
}

UBFObject *ubf_mkstring( UBFReader *buf, const char *text )
{
  return ubf_mkstringl( buf, text, strlen(text) );
}

UBFObject *ubf_mkint( UBFReader *buf, long long k )
{
  UBFObject *u = ubf_alloc( buf );
  u->type = UBF_INTEGER;
  u->data.ubf_integer = k;
  return u;
}

UBFObject *ubf_mkstruct( UBFReader *buf, UBFObject **data, unsigned len )
{
  unsigned i;
  UBFObject *u = ubf_alloc( buf );

  u->type = UBF_STRUCT;
  u->data.ubf_struct.vec = ubf_allocvec( buf, len );
  for (i=0; i<len; i++) {
    u->data.ubf_struct.vec[i] = data[i];
  }
  u->data.ubf_struct.len = len;
  return u;
}

UBFObject *ubf_cons( UBFReader *buf, UBFObject *first, UBFObject *rest )
{
  UBFObject *u = ubf_alloc( buf );
  u->type = UBF_PAIR;
  u->data.ubf_pair.item = first;
  u->data.ubf_pair.rest = rest;
  return u;
}


#define PAUSE(label)  do { ubf->state = &&label; return; } while (0)
#define PUSH(item) do { uassert( ubf->sp < &ubf->stack[STACK_MAX] ); *(ubf->sp)++ = (item); } while (0)

static UBFObject null = { UBF_NULL };
UBFObject *ubf_null = &null;

static UBFObject ubf_digit[11] = {
  { UBF_INTEGER, 0, data: { ubf_integer: -1 } },
  { UBF_INTEGER, 0, data: { ubf_integer: 0 } },
  { UBF_INTEGER, 0, data: { ubf_integer: 1 } },
  { UBF_INTEGER, 0, data: { ubf_integer: 2 } },
  { UBF_INTEGER, 0, data: { ubf_integer: 3 } },
  { UBF_INTEGER, 0, data: { ubf_integer: 4 } },
  { UBF_INTEGER, 0, data: { ubf_integer: 5 } },
  { UBF_INTEGER, 0, data: { ubf_integer: 6 } },
  { UBF_INTEGER, 0, data: { ubf_integer: 7 } },
  { UBF_INTEGER, 0, data: { ubf_integer: 8 } },
  { UBF_INTEGER, 0, data: { ubf_integer: 9 } } };

static void ubf_reset( UBFReader *ubf )
{
  memset( &ubf->binreg[0], 0, sizeof( ubf->binreg ) );
  ubf->structsp = 0;
  ubf->sp = &ubf->stack[0];
  ubf->memptr = &ubf->mem[0];
  ubf->str_heap = &ubf->str_mem[0];
  ubf->vecmem_ptr = &ubf->vec_mem[0];
}

unsigned char *usys_p;
unsigned char *usys_limit;

void ubf_run( UBFReader *ubf, unsigned char *p, unsigned len )
{
  unsigned char *limit = p + len;
  UBFObject *x;

  usys_p = p;
  usys_limit = limit;

  if (ubf->state) {
    goto *(ubf->state);
  }

 start:
  ubf_reset( ubf );
 next:
  if (p == limit) {
    PAUSE( next );
  }
  
  switch (*p) {
    
  case '-':
    p++;
    ubf->accum = 0;
    ubf->is_neg = 1;
    goto reading_int;

  case '0': p++; ubf->accum = 0; ubf->is_neg = 0; goto reading_int;
  case '1': p++; ubf->accum = 1; ubf->is_neg = 0; goto reading_int;
  case '2': p++; ubf->accum = 2; ubf->is_neg = 0; goto reading_int;
  case '3': p++; ubf->accum = 3; ubf->is_neg = 0; goto reading_int;
  case '4': p++; ubf->accum = 4; ubf->is_neg = 0; goto reading_int;
  case '5': p++; ubf->accum = 5; ubf->is_neg = 0; goto reading_int;
  case '6': p++; ubf->accum = 6; ubf->is_neg = 0; goto reading_int;
  case '7': p++; ubf->accum = 7; ubf->is_neg = 0; goto reading_int;
  case '8': p++; ubf->accum = 8; ubf->is_neg = 0; goto reading_int;
  case '9': p++; ubf->accum = 9; ubf->is_neg = 0; goto reading_int;

  case '$':
    p++;
    uassert( ubf->sp == &ubf->stack[1] );
    ubf->callback( ubf->stack[0], ubf->callback_info );
    goto start;

  case '>':
    p++;
    goto reading_def;

  case '\'':
  case '`':
  case '"':
    ubf->strterm = *p++;
    ubf->str_start = ubf->str_accum = ubf->str_heap;
    goto reading_str;

  case '&':
    p++;
    x = ubf_alloc( ubf );
    uassert( ubf->sp >= &ubf->stack[2] );
    x->type = UBF_PAIR;
    x->data.ubf_pair.item = *--ubf->sp;
    x->data.ubf_pair.rest = *--ubf->sp;
    PUSH(x);
    goto next;

  case ' ':
  case '\t':
  case '\r':
  case '\n':
    p++;
    goto next;

  case '~':
    uassert( ubf->sp > &ubf->stack[0] );
    uassert( ubf->sp[-1]->type == UBF_INTEGER );
    p++;
    ubf->accum = (*--ubf->sp)->data.ubf_integer;
    ubf->str_start = ubf->str_accum = ubf->str_heap;
    uassert( (ubf->str_start + ubf->accum) <= &ubf->str_mem[STRMEM_MAX] );
    goto reading_binary;

  case '{':
    p++;
    uassert( ubf->structsp < STACK_MAX );
    ubf->structs[ ubf->structsp++ ] = ubf->sp;
    goto next;

  case '}':
    uassert( ubf->structsp > 0 );
    {
      UBFObject **base = ubf->structs[ --ubf->structsp ];
      unsigned i, n = ubf->sp - base;
      p++;
      
      /*printf( "%u items in struct\n", n );*/
      x = ubf_alloc( ubf );
      x->type = UBF_STRUCT;
      x->data.ubf_struct.len = n;
      x->data.ubf_struct.vec = ubf_allocvec( ubf, n );
      for (i=0; i<n; i++) {
        x->data.ubf_struct.vec[i] = base[i];
      }
      ubf->sp = base;
      PUSH( x );
      goto next;
    }
      
  case '#':
    p++;
    PUSH( ubf_null );
    goto next;
    
  default:
    uassert( ubf->binreg[ *p ] );
    PUSH( ubf->binreg[ *p++ ] );
    goto next;
  }
  
 reading_binary_terminator:
  if (p == limit) {
    PAUSE( reading_binary_terminator );
  }
  uassert( *p == '~' );
  p++;

  x = ubf_alloc( ubf );
  x->type = UBF_BINARY;
  x->data.ubf_string.ptr = ubf->str_start;
  x->data.ubf_string.len = ubf->str_accum - ubf->str_start;
  *(ubf->str_accum)++ = '\0';
  ubf->str_heap = ubf->str_accum;
  PUSH( x );
  goto next;
  
 reading_binary:

  if (p == limit) {
    PAUSE( reading_binary );
  }
  /* if the whole binary is in our input, slurp it all at once */
  /* (note that this handles the case of ubf->accum == 0 as well) */
  if (p + ubf->accum <= limit) {
    memcpy( ubf->str_accum, p, ubf->accum );
    ubf->str_accum += ubf->accum;
    p += ubf->accum;
    goto reading_binary_terminator;
  } else {
    unsigned n = limit - p;
    /* it's only partially available.  slurp all we can, and then pause */
    memcpy( ubf->str_accum, p, n );
    ubf->str_accum += n;
    ubf->accum -= n;
    p += n;
    PAUSE( reading_binary );
  }
  
 saw_backslash:
  if (p == limit) {
    PAUSE( saw_backslash );
  }
  if (*p == '\\') {
    p++;
    *(ubf->str_accum)++ = '\\';
    goto reading_str;
  }
  if (*p == ubf->strterm) {
    p++;
    *(ubf->str_accum)++ = ubf->strterm;
    goto reading_str;
  }
  *(ubf->str_accum)++ = '\\';
  *(ubf->str_accum)++ = *p++;
  goto reading_str;
  
 reading_str:
  if (p == limit) {
    PAUSE( reading_str );
  }
  if (*p == '\\') {
    p++;
    goto saw_backslash;
  }
  if (*p == ubf->strterm) {
    p++;

    /* check limits after the fact, but hopefully we won't crash before this... */
    uassert( ubf->str_accum < &ubf->str_mem[STRMEM_MAX] );

    if (ubf->strterm == '\"') {
      x = ubf_alloc( ubf );
      x->type = UBF_STRING;
      x->data.ubf_string.ptr = ubf->str_start;
      x->data.ubf_string.len = ubf->str_accum - ubf->str_start;
      *(ubf->str_accum)++ = '\0';
      ubf->str_heap = ubf->str_accum;
      PUSH( x );
    } else if (ubf->strterm == '\'') {
      *(ubf->str_accum) = '\0';
      x = intern_const( ubf->str_start, ubf->str_accum - ubf->str_start );
      PUSH( x );
    } else if (ubf->strterm == '`') {
      *(ubf->str_accum) = '\0';
      x = intern_const( ubf->str_start, ubf->str_accum - ubf->str_start );
      uassert( ubf->sp > &ubf->stack[0] );

      /* not allowed to tag constants... */
      uassert( ubf->sp[-1]->type != UBF_CONSTANT );
      ubf->sp[-1]->tag = x;
    }
    goto next;
  }
  *(ubf->str_accum)++ = *p++;
  goto reading_str;

 reading_def:
  if (p == limit) {
    PAUSE( reading_def );
  }

  ubf->binreg[ *p++ ] = *--ubf->sp;
  goto next;

 reading_int:
  
  if (p == limit) {
    PAUSE( reading_int );
  }
  
  if (isdigit(*p)) {
    ubf->accum = ubf->accum * 10 + (*p++) - '0';
    goto reading_int;
  }

  if (ubf->is_neg) {
    ubf->accum = -ubf->accum;
  }

  if ((ubf->accum >= -1) && (ubf->accum < 10)) {
    x = &ubf_digit[ubf->accum+1];
  } else {
    x = ubf_alloc( ubf );
    x->type = UBF_INTEGER;
    x->data.ubf_integer = ubf->accum;
  }
  PUSH( x );
  goto next;
}

  
char *(indent[11]) = { "", "  ", "    ", "      ", "        ",
                       "      ",
                       "       ",
                       "        ",
                       "         ",
                       "          ",
                       "           " };

char escmode[256] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                      0, 0, '\"', 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, '\\', 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };

char *lltostr( long long x )
{
  static char tmp[32], *p;
  int neg;
  unsigned long long u;

  p = &tmp[32];
  *--p = '\0';
  if (x < 0) {
    neg = 1;
    u = -x;
  } else {
    neg = 0;
    u = x;
  }
  do {
    char digit = '0' + (u % 10);
    u = u / 10;
    *--p = digit;
  } while (u);
  if (neg) {
    *--p = '-';
  }
  return p;
}

void ubf_echo1( UBFObject *o, int j )
{
  unsigned i;
  char xtag[60];
  
  if (o->tag) {
    snprintf( xtag, sizeof(xtag),
              " `%.50s`", o->tag->data.ubf_constant.text );
  } else {
    xtag[0] = '\0';
  }
             
  switch (o->type) {
  case UBF_INTEGER:
    printf( "%sINTEGER%s %s\n", indent[j], xtag, lltostr( o->data.ubf_integer ) );
    break;

  case UBF_NULL:
    printf( "%sLIST%s ()\n", indent[j], xtag );
    break;

  case UBF_BINARY:
    printf( "%sBINARY%s [", indent[j], xtag );
    for (i=0; i<o->data.ubf_binary.len; i++) {
      if (i) {
        putchar( ' ' );
      }
      printf( "%02x", o->data.ubf_binary.ptr[i] );
    }
    printf( "]\n" );
    break;

  case UBF_CONSTANT:
    printf( "%sCONSTANT '%s'\n", indent[j], o->data.ubf_constant.text );
    break;

  case UBF_STRING:
    printf( "%sSTRING%s \"", indent[j], xtag );
    for (i=0; i<o->data.ubf_string.len; i++) {
      switch (escmode[ o->data.ubf_string.ptr[i] ]) {
      case 0:
        putchar( o->data.ubf_string.ptr[i] );
        break;
      case 1:
        printf( "\\%03o", o->data.ubf_string.ptr[i] );
        break;
      default:
        putchar( '\\' );
        putchar( escmode[ o->data.ubf_string.ptr[i] ] );
        break;
      }
    }
    printf( "\"\n" );
    break;
    
  case UBF_PAIR:
    printf( "%sLIST%s (\n", indent[j], xtag );
    while (o->type == UBF_PAIR) {
      ubf_echo1( o->data.ubf_pair.item, j+1 );
      o = o->data.ubf_pair.rest;
    }
    printf( "%s     )\n", indent[j] );
    break;
    
  case UBF_STRUCT:
    printf( "%sSTRUCT%s {\n", indent[j], xtag );
    for (i=0; i<o->data.ubf_struct.len; i++) {
      ubf_echo1( o->data.ubf_struct.vec[i], j+1 );
    }
    printf( "%s       }\n", indent[j] );
    break;

  default:
    printf( "%s?? <%d>\n", indent[j], o->type );
    break;
  }
}

void ubf_echo( UBFObject *o )
{
  ubf_echo1( o, 0 );
}

void ubf_print( FILE *dest, UBFObject *u )
{
  UBFObject *t;
  unsigned i;
  
  switch (u->type) {
  case UBF_INTEGER:
    fputs( lltostr( u->data.ubf_integer ), dest );
    break;

  case UBF_NULL:
    fprintf( dest, "()" );
    break;

  case UBF_BINARY:
    fprintf( dest, "%u ~", u->data.ubf_binary.len );
    fwrite( u->data.ubf_binary.ptr, 1, u->data.ubf_binary.len, dest );
    fprintf( dest, "~" );
    break;

  case UBF_CONSTANT:
    fprintf( dest, "'%s'", u->data.ubf_constant.text );
    break;

  case UBF_STRING:
    fputc( '\"', dest );
    for (i=0; i<u->data.ubf_string.len; i++) {
      switch (escmode[ u->data.ubf_string.ptr[i] ]) {
      case 0:
        fputc( u->data.ubf_string.ptr[i], dest );
        break;
      case 1:
        fprintf( dest, "\\%03o", u->data.ubf_string.ptr[i] );
        break;
      default:
        fputc( '\\', dest );
        fputc( escmode[ u->data.ubf_string.ptr[i] ], dest );
        break;
      }
    }
    fputc( '\"', dest );
    break;
    
  case UBF_PAIR:
    fputc( '(', dest );
    for (t=u; t->type == UBF_PAIR; t=UBF_REST(t)) {
      ubf_print( dest, UBF_FIRST(t) );
    }
    fputc( ')', dest );
    break;
    
  case UBF_STRUCT:
    fputc( '{', dest );
    for (i=0; i<u->data.ubf_struct.len; i++) {
      if (i) {
        fputc( ' ', dest );
      }
      ubf_print( dest, u->data.ubf_struct.vec[i] );
    }
    fputc( '}', dest );
    break;

  default:
    fprintf( dest, "#[? %d]", u->type );
    break;
  }
  if (u->tag) {
    fprintf( dest, " `%s`", u->tag->data.ubf_constant.text );
  }
}

static char *prdelim( char *p, const char *data, unsigned len, char delim )
{
  int i;

  *p++ = delim;
  for (i=0; i<len; i++) {
    if ((data[i] == delim) || (data[i] == '\\')) {
      *p++ = '\\';
    }
    *p++ = data[i];
  }
  *p++ = delim;
  return p;
}

static char *prstr( char *p, const char *src )
{
  while (*src) {
    *p++ = *src++;
  }
  return p;
}


int ubf_sprint( char *dest, UBFObject *u )
{
  unsigned i;
  char *p = dest;

  switch (u->type) {
  case UBF_INTEGER:
    p = prstr( p, lltostr( u->data.ubf_integer ) );
    break;

  case UBF_NULL:
    p += sprintf( p, "#" );
    break;

  case UBF_BINARY:
    p += sprintf( p, "%u ~", u->data.ubf_binary.len );
    memcpy( p, u->data.ubf_binary.ptr, u->data.ubf_binary.len );
    p += u->data.ubf_binary.len;
    *p++ = '~';
    break;

  case UBF_CONSTANT:
    p = prdelim( p, u->data.ubf_constant.text, 
                 strlen(u->data.ubf_constant.text),
                 '\'' );
    break;

  case UBF_STRING:
    p = prdelim( p, 
                 (char *)u->data.ubf_string.ptr,
                 u->data.ubf_string.len,
                 '\"' );
    break;
    
  case UBF_PAIR:
    p += ubf_sprint( p, UBF_REST(u) );
    p += ubf_sprint( p, UBF_FIRST(u) );
    *p++ = '&';
    break;

  case UBF_STRUCT:
    *p++ = '{';
    for (i=0; i<u->data.ubf_struct.len; i++) {
      p += ubf_sprint( p, u->data.ubf_struct.vec[i] );
    }
    *p++ = '}';
    break;

  default:
    p += sprintf( p, "?(%d)", u->type );
    break;
  }
  if (u->tag) {
    p += sprintf( p, "`%s`", u->tag->data.ubf_constant.text );
  }
  *p = 0;
  return p - dest;
}

void rebind_ubf_reader( UBFReader *ubf,
                        void (*callback)( UBFObject *datum, void *info ),
                        void *info )
{
  ubf->callback = callback;
  ubf->callback_info = info;
}

void free_ubf_reader( UBFReader *ubf )
{
  free( ubf );
}

static void uabort( int line, const char *text )
{
  fprintf( stderr, "ubf.c:%d: UBF assertion failed: %s\n", line, text );
  abort();
}

UBFReader *new_ubf_reader( void (*cb)( UBFObject *datum, void *info ),
                           void *info )
{
  UBFReader *u = malloc( sizeof( UBFReader ) );
  u->callback = cb;
  u->callback_info = info;
  u->badstuff = uabort;
  u->state = NULL;
  ubf_reset( u );
  return u;
}

unsigned ubf_length( UBFObject *u )
{
  unsigned n = 0;

  while (UBF_PAIR_Q(u)) {
    u = UBF_REST( u );
    n++;
  }
  return n;
}


UBFObject *assq( UBFObject *lst, UBFObject *key )
{
  while (UBF_PAIR_Q( lst )) {
    if (UBF_STRUCT_REF( UBF_FIRST( lst ), 0 ) == key) {
      return UBF_STRUCT_REF( UBF_FIRST( lst ), 1 );
    }
    lst = UBF_REST( lst );
  }
  return NULL;
}


#ifdef UBF_DEVEL

int main( int argc, const char **argv )
{
  UBFReader u;
  /*char *test = "#2& 1~a~&1&#\"bar\\\\\"&\"foo \\\"cat\\\"\"&3~xxx~&$";*/
  char *test = "{3~foo~ >FF `text/plain` {1 2 3} `blech` 'blech' F 123456789123 }$ \"ha\"$";
  
  void cb( UBFObject *d, void *info ) {
    ubf_echo( d );
    char tmp[512];
    ubf_sprint( tmp, d );
    printf( "----\n%s\n---\n", tmp );
  }

  u.callback = cb;
  u.state = NULL;

  printf( "UBFReader: %u bytes\n", sizeof(u) );

  /*ubf_run( &u, "123 $", 5 );*/
  if (argc == 1) {
    ubf_run( &u, test, strlen(test) );
  } else {
    int i;
    
    for (i=1; i<argc; i++) {
      if (argv[i][0] == '.') {
        char tmp[1000];
        FILE *f = fopen( argv[i], "r" );
        int n;
        n = fread( &tmp[0], 1, sizeof(tmp), f );
        ubf_run( &u, &tmp[0], n );
      } else {
        ubf_run( &u, (char *)argv[i], strlen(argv[i]) );
      }
    }
  }
      
  symtab_dump();
  return 0;
}
#endif
