#ifndef _H_UBF
#define _H_UBF

#include <stdio.h>

enum UBFType {
  UBF_NULL,
  UBF_INTEGER,
  UBF_STRING,
  UBF_BINARY,
  UBF_CONSTANT,
  UBF_PAIR,
  UBF_STRUCT
};

typedef struct _UBFObject {
  enum UBFType type;
  struct _UBFObject *tag;
  union {
    long long ubf_integer;
    struct {
      unsigned char *ptr;       /* NUL terminated */
      unsigned       len;       /* but might as well record len anyway */
    } ubf_string;
    struct {
      unsigned char *ptr;
      unsigned       len;
    } ubf_binary;
    struct {
      char *text;
      unsigned hash;
    } ubf_constant;
    struct {
      struct _UBFObject **vec;
      unsigned          len;
    } ubf_struct;
    struct {
      struct _UBFObject *item;
      struct _UBFObject *rest;
    } ubf_pair;
  } data;
} UBFObject;

extern UBFObject *ubf_null;
typedef struct _UBFReader UBFReader;

UBFReader *new_ubf_reader( void (*callback)( UBFObject *datum, void *info ),
                           void *info );
void rebind_ubf_reader( UBFReader *ubf,
                        void (*callback)( UBFObject *datum, void *info ),
                        void *info );
void free_ubf_reader( UBFReader *ubf );
void ubf_set_error_handler( UBFReader *ubf, 
                            void (*abortfn)( int line, const char *msg ) );

void ubf_run( UBFReader *ubf, unsigned char *p, unsigned len );
void ubf_echo( UBFObject *o );  /* debug to stdout */
void ubf_print( FILE *dest, UBFObject *u );
int ubf_sprint( char *dest, UBFObject *u );

/*
 *  Support for building UBF structures programmatically
 */

UBFObject *ubf_mkbin( UBFReader *buf, const void *data, unsigned len );
UBFObject *ubf_mkstring( UBFReader *buf, const char *text );
UBFObject *ubf_mkstringl( UBFReader *buf, const char *text, unsigned len );
UBFObject *ubf_mkint( UBFReader *buf, long long k );
UBFObject *ubf_mkstruct( UBFReader *buf, UBFObject **data, unsigned len );
UBFObject *ubf_cons( UBFReader *buf, UBFObject *first, UBFObject *rest );

static inline UBFObject **ubf_struct_slot( const char *f, int l, 
                                           UBFObject *x, unsigned i )
{
#ifndef UBF_NOCHECK
  if (i >= x->data.ubf_struct.len) {
    fprintf( stderr, "XXX-YYYYF Internal error at %s:%d\n", f, l );
    fprintf( stderr, "XXX-YYYYF Index %u into struct out of range {0,..,%u}\n",
             i,
             x->data.ubf_struct.len );
    return NULL;
  }
#endif /* UBF_NOCHECK */
  return &x->data.ubf_struct.vec[i];
}

static inline unsigned char *ubf_string_slot( const char *f, int l, 
                                              UBFObject *x, unsigned i )
{
#ifndef UBF_NOCHECK
  if (i >= x->data.ubf_string.len) {
    fprintf( stderr, "XXX-YYYYF Internal error at %s:%d\n", f, l );
    fprintf( stderr, "XXX-YYYYF Index %u into string out of range {0,..,%u}\n",
             i,
             x->data.ubf_string.len );
    return NULL;
  }
#endif /* UBF_NOCHECK */
  return &x->data.ubf_string.ptr[i];
}

static inline UBFObject *ubf_check( const char *f, int l, 
                                    UBFObject *x, enum UBFType t )
{
#ifndef UBF_NOCHECK
  if (!x) {
    fprintf( stderr, "XXX-YYYYF internal error at %s:%d\n", f, l );
    fprintf( stderr, "XXX-YYYYF Found NULL object when expecting type=%d", t );
    return NULL;
  } else if (x->type != t) {
    fprintf( stderr, "XXX-YYYYF internal error at %s:%d\n", f, l );
    fprintf( stderr, "XXX-YYYYF Found %d object when expecting type=%d", 
             x->type, t );
    return NULL;
  }
#endif /* UBF_NOCHECK */
  return x;
}

#define UBF_NULL_Q(u)           ((u)->type == UBF_NULL)
#define UBF_STRUCT_Q(u)         ((u)->type == UBF_STRUCT)
#define UBF_INTEGER_Q(u)        ((u)->type == UBF_INTEGER)
#define UBF_BINARY_Q(u)         ((u)->type == UBF_BINARY)
#define UBF_STRING_Q(u)         ((u)->type == UBF_STRING)
#define UBF_PAIR_Q(u)           ((u)->type == UBF_PAIR)
#define UBF_CONSTANT_Q(u)       ((u)->type == UBF_CONSTANT)

#define UBF_STRUCT_LEN(u) (UBF_CHECK( u, UBF_STRUCT )->data.ubf_struct.len)

#define UBF_ZERO_Q(u) (((u)->type == UBF_INTEGER) && ((u)->data.ubf_integer == 0))
#define UBF_CHECK(u,t) ubf_check( __FILE__, __LINE__, u, t )

#define UBF_STRUCT_SLOT(u,x) ubf_struct_slot( __FILE__, __LINE__, UBF_CHECK(u,UBF_STRUCT), x )
#define UBF_STRING_SLOT(u,x) ubf_string_slot( __FILE__, __LINE__, UBF_CHECK(u,UBF_STRING), x )

#define UBF_FIRST(u)    (UBF_CHECK(u,UBF_PAIR)->data.ubf_pair.item)
#define UBF_REST(u)    (UBF_CHECK(u,UBF_PAIR)->data.ubf_pair.rest)

#define UBF_STRUCT_REF(u,i) (*(UBF_STRUCT_SLOT( u, i )))
#define UBF_VECTOR_REF(u,i) (*(UBF_STRUCT_SLOT( u, i )))
#define UBF_STRING_REF(u,i) (*(UBF_STRING_SLOT( u, i )))

#define UBF_STRING_TEXT(u)  ((u)->data.ubf_string.ptr)
#define UBF_CONSTANT_TEXT(u)  (UBF_CHECK(u,UBF_CONSTANT)->data.ubf_constant.text)
#define UBF_INTEGER_VALUE(u) (UBF_CHECK(u,UBF_INTEGER)->data.ubf_integer)

unsigned ubf_length( UBFObject *u );

/* sometimes useful... */
UBFObject *assq( UBFObject *lst, UBFObject *key );


/*
 *  These are actually implemented in globals.c
 */
UBFObject *do_setoption( UBFObject *key, UBFObject *value, int whence );
UBFObject *pgm_invoke( UBFObject *key, UBFObject *invocation );


#endif /* _H_UBF */
