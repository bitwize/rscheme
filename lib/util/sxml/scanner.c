#include <ctype.h>
#include <string.h>
#include <setjmp.h>
#include "scanner.h"

typedef struct _XScan {
  unsigned char *ptr;
  unsigned char *base;
  unsigned char *limit;

  obj            classv;
  obj            buffer;
  int            end_of_entity;
  jmp_buf        esc;
  obj            result;
} XScan;


#define EMPTY_VECTOR(xs)                (gvec_ref( (xs)->classv, SLOT(0) ))
#define ELEMENT_CLASS(xs)               (gvec_ref( (xs)->classv, SLOT(1) ))
#define START_ELEMENT_CLASS(xs)         (gvec_ref( (xs)->classv, SLOT(2) ))
#define END_ELEMENT_CLASS(xs)           (gvec_ref( (xs)->classv, SLOT(3) ))
#define TEXT_CLASS(xs)                  (gvec_ref( (xs)->classv, SLOT(4) ))
#define PI_CLASS(xs)                    (gvec_ref( (xs)->classv, SLOT(5) ))
#define DOCTYPE_DECL_CLASS(xs)          (gvec_ref( (xs)->classv, SLOT(6) ))
#define COMMENT_CLASS(xs)               (gvec_ref( (xs)->classv, SLOT(7) ))
#define ATTR_CLASS(xs)                  (gvec_ref( (xs)->classv, SLOT(8) ))

#define CHAR_IS_LETTER          (0x01)
#define CHAR_IS_NAMECHAR        (0x02)
#define CHAR_IS_SPACE           (0x04)
#define CHAR_IS_PUBIDCHAR       (0x08)

unsigned char xmlCharClass[256] = {
0,0,0,0,0,0,0,0,0,4,12,0,4,12,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,12,8,0,8,8,8,0,8,8,8,8,8,8,10,10,8,10,10,10,10,10,10,10,10,10,10,10,8,0,8,0,8,8,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,0,0,0,0,10,0,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,0,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,0,3,3,3,3,3,3,3,3
};


#define XML_SPACE(ch)                   (((ch) == ' ') || ((ch) == '\n'))
#define XML_LETTER(ch)                  (xmlCharClass[ch] & CHAR_IS_LETTER)
#define XML_NAMECHAR(ch)                (xmlCharClass[ch] & CHAR_IS_NAMECHAR)
#define XML_PUBIDCHAR(ch)               (xmlCharClass[ch] & CHAR_IS_PUBIDCHAR)

typedef struct _Atom {
  unsigned char *start;
  unsigned       length;
  unsigned char *colon;
} Atom;

static volatile void xs_error( XScan *xs, int code, const char *msg )
{
  if (xs->ptr < xs->limit) {
    scheme_error( "XML Error [~d] ~a (at +~d: #\\x~02x ~s)", 
                  5,
                  int2fx( code ), make_string( msg ),
                  int2fx( xs->ptr - xs->base ),
                  int2fx( *xs->ptr ),
                  MAKE_ASCII_CHAR( *xs->ptr ) );
  } else {
    scheme_error( "XML Error [~d] ~a (at EOF)", 
                  2,
                  int2fx( code ), make_string( msg ) );
  }
}


static unsigned char xs_eof( XScan *xs )
{
  if (xs->end_of_entity) {
    xs_error( xs, 0, "unexpected EOF" );
  } else {
    longjmp( xs->esc, 1 );
  }
  return '\0';
}

static inline unsigned char readchar( XScan *xs )
{
  if (xs->ptr >= xs->limit) {
    xs_eof( xs );
  }
  return *(xs->ptr)++;
}

static inline unsigned char peekchar( XScan *xs )
{
  if (xs->ptr >= xs->limit) {
    xs_eof( xs );
  }
  return *(xs->ptr);
}

static int atom_is_xml( Atom *n )
{
  return ((n->length == 3)
          && ((n->start[0] == 'x') || (n->start[0] == 'X'))
          && ((n->start[1] == 'm') || (n->start[1] == 'M'))
          && ((n->start[2] == 'l') || (n->start[2] == 'L')));
}

static int atom_is( Atom *n, const char *str )
{
  return ((n->length == strlen(str))
          && (memcmp( n->start, str, n->length ) == 0));
}

static int crack_atom_ns( Atom *n )
{
  unsigned i;

  n->colon = NULL;

  for (i=0; i<n->length-1; i++) {
    if (n->start[i] == ':') {
      if (i == 0) {
        /* the ':' is the first character.  Not so good. */
        return 0;
      }
      n->colon = &n->start[i];
      return 1;
    }
  }
  return 0;
}

static obj atom_to_namespace( Atom *n )
{
  if (n->colon) {
    unsigned char *tmp;
    unsigned prelen = n->colon - n->start;
    
    tmp = alloca( prelen + 1 );
    memcpy( tmp, n->start, prelen );
    tmp[ prelen ] = 0;
      
    return lookup_symbol( tmp );
  } else {
    return FALSE_OBJ;
  }
}

static obj atom_to_string( Atom *n )
{
  obj t = bvec_alloc( n->length + 1, string_class );
  memcpy( PTR_TO_DATAPTR( t ), n->start, n->length );
  ((char *)PTR_TO_DATAPTR( t ))[n->length] = 0;
  return t;
}

static obj atom_to_symbol( Atom *n )
{
  unsigned char tmp[32];

  if (n->length < 30) {
    memcpy( &tmp[0], n->start, n->length );
    tmp[n->length] = 0;
    return lookup_symbol( tmp );
  } else {
    obj t = bvec_alloc( n->length + 1, string_class );
    memcpy( PTR_TO_DATAPTR( t ), n->start, n->length );
    ((char *)PTR_TO_DATAPTR( t ))[n->length] = 0;
    return intern( t );
  }
}

static obj atom_to_symbol_with_ns( Atom *n )
{
  if (n->colon) {
    unsigned char *tmp;
    unsigned postlen = n->length - (n->colon + 1 - n->start);
    
    tmp = alloca( postlen + 1 );
    memcpy( tmp, n->colon + 1, postlen );
    tmp[ postlen ] = 0;
      
    return lookup_symbol( tmp );
  } else {
    return atom_to_symbol( n );
  }
}

/*
 *  Return 1 if at least one whitespace character was present
 */

static int xs_Space( XScan *xs )
{
  unsigned char ch;

  ch = peekchar( xs );

  if (!XML_SPACE(ch)) {
    return 0;
  }

  xs->ptr++;
  while (1) {
    ch = peekchar( xs );
    if (!XML_SPACE(ch)) {
      break;
    }
    xs->ptr++;
  }
  return 1;
}

static int xs_PubidLiteral( XScan *xs, Atom *n )
{
  unsigned char delim = readchar( xs );
  unsigned char ch;

  if (!((delim == '"') || (delim == '\''))) {
    return 0;
  }
  
  n->start = xs->ptr;
  while ((ch = readchar( xs )) != delim) {
    if (!XML_PUBIDCHAR(ch)) {
      xs_error( xs, 13, "invalid PubidChar" );
    }
  }
  n->length = (xs->ptr - 1) - n->start;
  return 1;
}

static int xs_SystemLiteral( XScan *xs, Atom *n )
{
  unsigned char delim = readchar( xs );

  if (!((delim == '"') || (delim == '\''))) {
    return 0;
  }
  
  n->start = xs->ptr;
  while (readchar( xs ) != delim) {
    /* keep going; nothing special inside SystemLiteral */
  }
  n->length = (xs->ptr - 1) - n->start;
  return 1;
}

static int xs_Name( XScan *xs, Atom *n )
{
  unsigned char ch;

  n->start = xs->ptr;

  ch = peekchar( xs );
  if (!(XML_LETTER(ch) || (ch == '_') || (ch == ':'))) {
    return 0;
  }
  readchar( xs );

  while (1) {
    ch = peekchar( xs );
    if (!(XML_NAMECHAR(ch))) {
      break;
    }
    xs->ptr++;
  }
  n->length = xs->ptr - n->start;
  return 1;
}


/*
 *  Scan the "rest" of a [66] CharRef, which is to say,
 *  we've already read out the '&', now read the rest
 */

static int xs_CharRefRest( XScan *xs, unsigned *codepoint )
{
  unsigned char ch;
  unsigned accum = 0;

  ch = peekchar( xs );

  if (ch != '#') {
    return 0;
  }

  ch = readchar( xs );   /* eat the sharp */

  ch = readchar( xs );   /* see what's next */

  if ((ch == 'x') || (ch == 'X')) {
    /* it's in hex */
    ch = readchar( xs );
    do {
      if (!isxdigit( ch )) {
        xs_error( xs, 66, "expected hex digits after '&#x' in CharRef" );
      }
      if ((ch >= '0') && (ch <= '9')) {
        accum = (accum << 4) + ch - '0';
      } else if ((ch >= 'a') && (ch <= 'f')) {
        accum = (accum << 4) + ch - 'a' + 10;
      } else if ((ch >= 'A') && (ch <= 'F')) {
        accum = (accum << 4) + ch - 'A' + 10;
      }
      ch = readchar( xs );
    } while (ch != ';');

    *codepoint = accum;
    return 1;
  } else {
    /* it's in decimal */
    do {
      if (!isdigit( ch )) {
        xs_error( xs, 66, "expected decimal digits after '&#' in CharRef" );
      }
      accum = (accum * 10) + ch - '0';
      ch = readchar( xs );
    } while (ch != ';');

    *codepoint = accum;
    return 1;
  }

  return 0;     /* XXX stubbed out */
}

/*
 *  The '&' (or perhaps '%' in some contexts) has been seen; what's next?
 */

static int xs_ReferenceRest( XScan *xs, obj *item )
{
  Atom n;
  unsigned codepoint;
  obj p;

  if (xs_Name( xs, &n )) {
    *item = atom_to_symbol( &n );
    if (readchar( xs ) != ';') {
      xs_error( xs, 68, "expected ';' at end of EntityRef" );
    }
    return 1;
  } else if (xs_CharRefRest( xs, &codepoint )) {
    obj p, ch;
    if (codepoint <= 0xFF) {
      *item = MAKE_ASCII_CHAR( codepoint );
    } else {
      *item = MAKE_UNICODE_CHAR( codepoint );
    }
    return 1;
  } else {
    return 0;
  }
}

static int xs_EntityValue( XScan *xs, obj *v )
{
  unsigned char delim = readchar( xs );
  unsigned char ch;
  obj head, tail, p;
  unsigned char *run, *c;

  if (!((delim == '"') || (delim == '\''))) {
    return 0;
  }

  head = cons( FALSE_OBJ, NIL_OBJ );
  tail = head;
  run = xs->ptr;

  while (1) {

    c = xs->ptr;
    ch = readchar( xs );
    if (ch == delim) {
      break;
    }
    if ((ch == '&') || (ch == '%')) {
      obj ref;

      /* Flush the current run */
      if (c > run) {
        p = cons( make3( vector_class, 
                         xs->buffer,
                         int2fx( run - xs->base ),
                         int2fx( c - xs->base ) ),
                  NIL_OBJ );
        pair_set_cdr( tail, p );
        tail = p;
      }

      if (!xs_ReferenceRest( xs, &ref )) {
        if (ch == '&') {
          xs_error( xs, 67, "saw '&' in EntityValue; expected a valid Reference to follow" );
        } else if (ch == '%') {
          xs_error( xs, 69, "saw '%' in EntityValue; expected a valid PEReference to follow" );
        }
      }
      if (ch == '%') {
        ref = cons( lookup_symbol( "%" ), ref );
      }
      p = cons( ref, NIL_OBJ );
      pair_set_cdr( tail, p );
      tail = p;

      /* start a new run */
      run = xs->ptr;
    }
  }
  /* Flush the current run */
  if (c > run) {
    obj p = cons( make3( vector_class, 
                         xs->buffer,
                         int2fx( run - xs->base ),
                         int2fx( c - xs->base ) ),
                  NIL_OBJ );
    pair_set_cdr( tail, p );
    tail = p;
  }
  *v = pair_cdr( head );
  return 1;
}

static int xs_AttValue( XScan *xs, obj *v )
{
  unsigned char delim = readchar( xs );
  unsigned char ch;
  obj head, tail, p;
  unsigned char *run, *c;

  if (!((delim == '"') || (delim == '\''))) {
    return 0;
  }

  head = cons( FALSE_OBJ, NIL_OBJ );
  tail = head;
  run = xs->ptr;

  while (1) {

    c = xs->ptr;
    ch = readchar( xs );
    if (ch == delim) {
      break;
    }
    if (ch == '&') {
      obj ref;

      /* Flush the current run */
      if (c > run) {
        p = cons( make3( vector_class, 
                         xs->buffer,
                         int2fx( run - xs->base ),
                         int2fx( c - xs->base ) ),
                  NIL_OBJ );
        pair_set_cdr( tail, p );
        tail = p;
      }

      if (!xs_ReferenceRest( xs, &ref )) {
        xs_error( xs, 67, "saw '&' in AttValue; expected a valid Reference to follow" );
      }
      p = cons( ref, NIL_OBJ );
      pair_set_cdr( tail, p );
      tail = p;

      /* start a new run */
      run = xs->ptr;
    } else if (ch == '<') {
      xs_error( xs, 10, "Illegal char in AttValue" );
    }
  }
  /* Flush the current run */
  if (c > run) {
    obj p = cons( make3( vector_class, 
                         xs->buffer,
                         int2fx( run - xs->base ),
                         int2fx( c - xs->base ) ),
                  NIL_OBJ );
    pair_set_cdr( tail, p );
    tail = p;
  }
  *v = pair_cdr( head );
  return 1;
}

static int xs_comment( XScan *xs )
{
  unsigned char *start, ch;

  start = xs->ptr;

 body:
  if (readchar( xs ) != '-') {
    goto body;
  }
  
 saw_dash:
  if (readchar( xs ) != '-') {
    goto body;
  }

 saw_dashdash:
  ch = readchar( xs );
  if (ch != '>') {
    if (ch == '-') {
      goto saw_dashdash;
    }
    goto body;
  }
  xs->result = make3( COMMENT_CLASS(xs),
                      FALSE_OBJ,
                      EMPTY_VECTOR(xs),
                      make3( vector_class,
                             xs->buffer,
                             int2fx( start - xs->base ),
                             int2fx( (xs->ptr-3) - xs->base ) ) );
  return TC_COMMENT;
}

static int xs_externalid( XScan *xs, obj *xid_sys, obj *xid_pub )
{
  Atom t;

  if (!xs_Name( xs, &t )) {
    return 0;
  }

  if (atom_is( &t, "SYSTEM" )) {
    /*
     *  Parse a SYSTEM ExternalId
     */
    Atom sysid;

    if (!xs_Space( xs )) {
      xs_error( xs, 75, "Expected S (space) after SYSTEM" );
    }
    if (!xs_SystemLiteral( xs, &sysid )) {
      xs_error( xs, 75, "Expected SystemLiteral after SYSTEM" );
    }
    *xid_sys = atom_to_string( &sysid );
    *xid_pub = FALSE_OBJ;
    return 1;
  } else if (atom_is( &t, "PUBLIC" )) {
    /*
     *  Parse a PUBLIC ExternalId
     */
    Atom sysid, pubid;


    if (!xs_Space( xs )) {
      xs_error( xs, 75, "Expected S (space) after PUBLIC" );
    }
    if (!xs_PubidLiteral( xs, &pubid )) {
      xs_error( xs, 75, "Expected PubidLiteral after PUBLIC" );
    }
    if (!xs_Space( xs )) {
      xs_error( xs, 75, "Expected S (space) after PubidLiteral" );
    }
    if (!xs_SystemLiteral( xs, &sysid )) {
      xs_error( xs, 75, "Expected SystemLiteral after PubidLiteral" );
    }
    *xid_sys = atom_to_string( &sysid );
    *xid_pub = atom_to_string( &pubid );
    return 1;
  } else {
    return 0;
  }
}

static int xs_doctype( XScan *xs )
{
  Atom n;
  obj doctype_pub = FALSE_OBJ;
  obj doctype_sys = FALSE_OBJ;
  obj has_subtype = FALSE_OBJ;
  unsigned char ch;

  if (!xs_Space( xs )) {
    xs_error( xs, 28, "Expected S (space) after DOCTYPE" );
  }

  if (!xs_Name( xs, &n )) {
    xs_error( xs, 28, "Expected Name in DOCTYPE" );
  }

  if (xs_Space( xs )) {
    /* might have an ExternalID, now, which is

       either: SYSTEM S SystemLiteral
           or: PUBLIC S PubidLiteral S SystemLiteral
    */

    if (peekchar( xs ) == '[') {
      readchar( xs );
      has_subtype = TRUE_OBJ;
      goto done;
    }

    if (!xs_externalid( xs, &doctype_sys, &doctype_pub )) {
      xs_error( xs, 75, "Expected SYSTEM or PUBLIC for ExternalID" );
    }

    xs_Space( xs );
    if (peekchar( xs ) == '[') {
      has_subtype = TRUE_OBJ;
    }
  }
 done:
  if (EQ( has_subtype, FALSE_OBJ ) && (peekchar( xs ) != '>')) {
    xs_error( xs, 28, "Expected '>' at end of DOCTYPE decl" );
  }
  xs->ptr++;

  xs->result = make6( DOCTYPE_DECL_CLASS(xs),
                      FALSE_OBJ,
                      EMPTY_VECTOR(xs),
                      atom_to_symbol( &n ),
                      doctype_sys,
                      doctype_pub,
                      has_subtype );
  return TC_DOCTYPE_DECL;
}


static struct _Special {
  const char *tag;
  int (*handler)( XScan *xs );
} specials[] = { { "DOCTYPE", xs_doctype },
                 { "--", xs_comment },
                 { NULL, NULL } };

static int xs_special( XScan *xs )      /* Handle all things '<!' */
{
  unsigned i, n = 0;
  int some_ok;
  unsigned char *start;
  
  xs->ptr++;            /* eat the '!' that got us here */

  start = xs->ptr;
 another:
  some_ok = 0;
  for (i=0; specials[i].tag; i++) {
    if (memcmp( start, specials[i].tag, n ) == 0) {
      some_ok = 1;
      if (specials[i].tag[n] == '\0') {
        return specials[i].handler( xs );
      }
    }
  }
  if (some_ok) {
    /* eat another... */
    readchar( xs );
    n++;
    goto another;
  }

  xs->ptr = start;
  xs_error( xs, 0, "Strange thing after '<!'" );
  return -1;
}

static int xs_pi( XScan *xs )
{
  Atom n;
  unsigned char *start;

  xs->ptr++;            /* eat the '?' that got us here */
  if (!xs_Name( xs, &n )) {
    xs_error( xs, 16, "Expected PITarget Name" );
  }
  xs_Space( xs );

  start = xs->ptr;
 body:
  if (readchar( xs ) == '?') {
    goto sawq;
  }
  goto body;

 sawq:
  if (readchar( xs ) != '>') {
    goto body;
  }
  xs->result = make4( PI_CLASS(xs), 
                      FALSE_OBJ,
                      EMPTY_VECTOR(xs),
                      atom_to_symbol( &n ),
                      make3( vector_class,
                             xs->buffer,
                             int2fx( start - xs->base ),
                             int2fx( (xs->ptr-2) - xs->base ) ) );
  if (atom_is_xml( &n )) {
    return TC_XML;
  } else {
    return TC_PI;
  }
}

static int xs_elem( XScan *xs )
{
  Atom n;
  obj attrs = NIL_OBJ;
  int is_end_tag = 0;
  int is_empty = 0;

  xs->ptr++;    /* eat the '<' that got us here */

  if (peekchar( xs ) == '/') {
    xs->ptr++;          /* eat the '/' */
    is_end_tag = 1;
  } else if (peekchar( xs ) == '!') {
    return xs_special( xs );
  } else if (peekchar( xs ) == '?') {
    return xs_pi( xs );
  }

  if (!xs_Name( xs, &n )) {
    xs_error( xs, 40, "expected a Name at start of STag" );
  }

  if (is_end_tag) {
    xs_Space( xs );
    if (peekchar( xs ) != '>') {
      xs_error( xs, 42, "expected a '>' at end of ETag" );
    }
    xs->ptr++;
    crack_atom_ns( &n );
    xs->result = make4( END_ELEMENT_CLASS(xs),
                        FALSE_OBJ,                      /* location */
                        EMPTY_VECTOR(xs),               /* properties */
                        atom_to_symbol_with_ns( &n ),
                        atom_to_namespace( &n ) );
    return TC_END_ELEMENT;
  }

  while (xs_Space( xs )) {
    Atom k;
    obj v;
    obj this_attr;

    if ((peekchar( xs ) == '>') || (peekchar( xs ) == '/')) {
      break;
    }
    if (!xs_Name( xs, &k )) {
      xs_error( xs, 41, "expected a Name at start of Attribute" );
    }

    xs_Space( xs );           /* [25] Eq... optional space */
    if (peekchar( xs ) != '=') {
      xs_error( xs, 25, "expected a '=' in Eq (in Attribute)" );
    }
    xs->ptr++;
    xs_Space( xs );           /* [25] Eq... optional space */

    if (!xs_AttValue( xs, &v )) {
      xs_error( xs, 41, "expected an AttValue" );
    }

    crack_atom_ns( &k );
    this_attr = make5( ATTR_CLASS(xs),
                       FALSE_OBJ,               /* location */
                       EMPTY_VECTOR(xs),        /* properties */
                       atom_to_symbol_with_ns( &k ),
                       atom_to_namespace( &k ),
                       v );
    attrs = cons( this_attr, attrs );
  }
  
  if (peekchar( xs ) == '/') {
    xs->ptr++;
    if (peekchar( xs ) != '>') {
      xs_error( xs, 44, "expected a '>' at end of EmptyElemTag" );
    }
    is_empty = 1;
  } else if (peekchar( xs ) != '>') {
    xs_error( xs, 40, "expected a '>' at end of STag" );
  }

  xs->ptr++;    /* already peeked the '>' */
    
  crack_atom_ns( &n );
  xs->result = make5( is_empty ? ELEMENT_CLASS(xs) : START_ELEMENT_CLASS(xs),
                      FALSE_OBJ,                      /* location */
                      EMPTY_VECTOR(xs),               /* properties */
                      atom_to_symbol_with_ns( &n ),
                      attrs,
                      atom_to_namespace( &n ) );
  return is_empty ? TC_EMPTY_ELEMENT : TC_START_ELEMENT;
}

static int xs_text( XScan *xs )
{
  unsigned char *c, *run;
  obj head, tail, p;
  unsigned char *space_run_end = NULL;
  int allspace = 0;

  head = cons( FALSE_OBJ, NIL_OBJ );
  tail = head;

  run = c = xs->ptr;

  while (1) {
    if (c >= xs->limit) {
      if (xs->end_of_entity) {
        break;
      }
      /*  By signalling TC_MORE, we ensure that a contiguous TEXT region
       *  in the input XML will result in a single TEXT token, and hence
       *  a single <string> in the SXML.  On the down side, we will wind
       *  up re-scanning the prefix of the text area as many times as
       *  we have to read additional input blocks.
       */
      xs_eof( xs );
    }
    switch (*c) {
    case '&':
      {
        obj ref;

        if (c > run) {
          p = cons( make3( vector_class, 
                           xs->buffer,
                           int2fx( run - xs->base ),
                           int2fx( c - xs->base ) ),
                    NIL_OBJ );
          pair_set_cdr( tail, p );
          tail = p;
        }

        xs->ptr = c+1;
        if (!xs_ReferenceRest( xs, &ref )) {
          xs_error( xs, 67, 
                    "saw '&' in CData; expected valid Reference to follow" );
        }
        p = cons( ref, NIL_OBJ );
        pair_set_cdr( tail, p );
        tail = p;
        c = xs->ptr;
        run = c;
        break;
      }
    case '<':
      goto done;
    case ' ':
    case '\r':
    case '\n':
    case '\t':
      c++;
      break;
    default:
      if (!space_run_end) {
        space_run_end = c;
      }
      c++;
      break;
    }
  }
  done:

  if (!space_run_end) {
    if (EQ( tail, head )) {
      allspace = 1;
    }
    space_run_end = c;
  }

  if (c > run) {
    obj p = cons( make4( vector_class, 
                         xs->buffer,
                         int2fx( run - xs->base ),
                         int2fx( c - xs->base ),
                         int2fx( space_run_end - xs->base ) ),
                  NIL_OBJ );
    pair_set_cdr( tail, p );
    tail = p;
  }

  xs->ptr = c;

  xs->result = make3( TEXT_CLASS(xs),
                      FALSE_OBJ,                      /* location */
                      EMPTY_VECTOR(xs),               /* properties */
                      pair_cdr( head ) );
  return allspace ? TC_WHITESPACE : TC_TEXT;
}

static int rsxml_name( XScan *xs )
{
  Atom n;

  if (xs_Name( xs, &n )) {
    xs->result = atom_to_symbol( &n );
    return TC_NAME;
  }
  return TC_NO_MATCH;
}

static int rsxml_entityvalue( XScan *xs )
{
  if (xs_EntityValue( xs, &xs->result )) {
    return TC_ENTITY_VALUE;
  }
  return TC_NO_MATCH;
}

static int rsxml_content( XScan *xs )
{
  switch (*(xs->ptr)) {
  case '<':
    return xs_elem( xs );

  default:
    return xs_text( xs );
  }
}

static int rsxml_comment( XScan *xs )
{
  if (xs_comment( xs )) {
    return TC_COMMENT;
  }
  return TC_NO_MATCH;
}

static int rsxml_externalid( XScan *xs )
{
  obj sys, pub;
  
  if (xs_externalid( xs, &sys, &pub )) {
    xs->result = make2( vector_class, sys, pub );
    return TC_EXTERNAL_ID;
  }
  return TC_NO_MATCH;
}

int rscheme_xml_scanner( obj scanner_state, 
                         obj buffer,
                         unsigned *poffset,
                         unsigned len,
                         obj class_vec,
                         int is_last,
                         obj *token,
                         int goal )
{
  XScan xs;
  int tc = TC_NO_MATCH;

  if (setjmp( xs.esc ) != 0) {
    return TC_MORE;
  }

  xs.base = (unsigned char *)PTR_TO_DATAPTR(buffer);
  xs.ptr = xs.base + *poffset;
  xs.limit = xs.base + len;
  xs.classv = class_vec;
  xs.buffer = buffer;
  xs.end_of_entity = is_last;
  xs.result = FALSE_OBJ;

  if (xs.ptr < xs.limit) {
    switch (goal) {
    case 0: tc = rsxml_content( &xs ); break;
    case TC_NAME: tc = rsxml_name( &xs ); break;
    case TC_ENTITY_VALUE: tc = rsxml_entityvalue( &xs ); break;
    case TC_EXTERNAL_ID: tc = rsxml_externalid( &xs ); break;
    case TC_COMMENT: tc = rsxml_comment( &xs ); break;
    }
  } else {
    if (!is_last) {
      return TC_MORE;
    }
    tc = TC_EOF;
  }
  *poffset = xs.ptr - xs.base;
  *token = xs.result;
  return tc;
}
