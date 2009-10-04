#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include "freemap.h"

#define ALLOCN(t,n) ((t *)malloc( sizeof(t)*n ))
#define ALLOC(t)    ALLOCN(t,1)

struct Interval {
  unsigned from, to;
};

int initFreeMap( struct FreeMap *fmap )
{
  struct Interval *map;

  map = ALLOCN( struct Interval, 4 );
  if (!map) {
    return -ENOMEM;
  }
  fmap->map = map;
  fmap->count = 1;
  fmap->capacity = 4;
  fmap->last = 0;
  map[0].from = 0;
  map[0].to = ~0U;
  return 0;
}

struct FreeMap *newFreeMap( void )
{
  struct FreeMap *fmap;

  fmap = ALLOC( struct FreeMap );
  if (!fmap) {
    return NULL;
  }
  if (initFreeMap( fmap ) < 0) {
    free( fmap );
    return NULL;
  }
  return fmap;
}

void fmapInsertEntry( struct FreeMap *self, unsigned i )
{
  if (self->capacity <= self->count) {
#ifdef DEBUG
    printf( "(grow) " );
#endif
    self->capacity *= 2;
    self->map = (struct Interval *)realloc( self->map, 
                                            sizeof( struct Interval ) 
                                            * self->capacity );
  }
  memmove( &self->map[i+1],
           &self->map[i],
           sizeof( struct Interval ) * (self->count - i) );
  self->count++;
}

int fmapDeleteQuick( struct FreeMap *self, unsigned key )
{
  unsigned i = self->last;

  assert( i < self->count );
#ifdef DEBUG
  printf( "delquick [%u] %u... ", i, key );
#endif

  if (self->map[i].from == key) {
    if (self->map[i].to == key) {
      memmove( &self->map[i], &self->map[i+1],
               sizeof( struct Interval ) * (self->count - i) );
      self->count--;
#ifdef DEBUG
      printf( "collapse\n" );
#endif
      if (self->last) {
        self->last--;
      }
      assert( self->last < self->count );
      return 1;
    }
    self->map[i].from++;
#ifdef DEBUG
    printf( "bump from\n" );
#endif
    return 1;
  } else if (self->map[i].to == key) {
#ifdef DEBUG
    printf( "shrink to\n" );
#endif
    self->map[i].to--;
    return 1;
  } else if ((self->map[i].from <= key)
            && (self->map[i].to >= key)) {
    fmapInsertEntry( self, i );
    self->map[i].to = key - 1;
    self->map[i+1].from = key + 1;
#ifdef DEBUG
    printf( "split\n" );
#endif
    return 1;
  } else {
#ifdef DEBUG
    printf( "---\n" );
#endif
    return 0;
  }
}

int cmpkeyi( const void *key, const void *entry )
{
  const struct Interval *e = entry;
  unsigned k = (unsigned)key;

  if (k < e[-1].to) {
    return -1;
  } else if (k > e[0].from) {
    return 1;
  } else {
    return 0;
  }
}

int cmpkey( const void *key, const void *entry )
{
  const struct Interval *e = entry;
  unsigned k = (unsigned)key;

  if (k < e->from) {
    return -1;
  } else if (k > e->to) {
    return 1;
  } else {
    return 0;
  }
}

int fmapDeleteRange( struct FreeMap *self, unsigned key0, unsigned key1 )
{
  if (key0 <= key1) {
    while (key0 != key1) {
      fmapDelete( self, key0 );
      key0++;
    }
    fmapDelete( self, key1 );
  }
  return 1;
}

int fmapDelete( struct FreeMap *self, unsigned key )
{
  struct Interval *p;
  
  if (fmapDeleteQuick( self, key )) {
    return 1;
  }

  if ((self->last + 1) < self->count) {
    self->last++;
    assert( self->last < self->count );
    if (fmapDeleteQuick( self, key )) {
      assert( self->last < self->count );
      return 1;
    }
  }
  
  p = bsearch( (void *)key, self->map, 
               self->count, 
               sizeof( struct Interval ),
               cmpkey );

  if (!p) {
    return 0;
  }
  self->last = p - &self->map[0];
  assert( self->last < self->count );
  if (fmapDeleteQuick( self, key )) {
    assert( self->last < self->count );
    return 1;
  }
  assert(0);
  return 0;
}

int fmapFindAndDeleteRange( struct FreeMap *fmap, unsigned minr, unsigned cnt, 
                            unsigned *rec )
{
  unsigned i;
  struct Interval *m;

  /*  We could use binary search to find the starting point,
   *  but in the common case that will probably save us almost
   *  nothing
   */
  for (i=0, m=fmap->map; i<fmap->count; i++, m++) {
    if ((m->to >= minr) && ((m->to - m->from) >= (cnt - 1))) {
      if (m->from >= minr) { 
        *rec = m->from;
        m->from += cnt;
        assert( (m->to == ~0U) || (m->from <= (m->to + 1)) );
        if (m->from == (m->to + 1)) {
          memmove( &fmap->map[i], &fmap->map[i+1],
                   sizeof( struct Interval ) * (fmap->count - i) );
          fmap->count--;
          fmap->last = i;
          assert( fmap->last < fmap->count );
        }
        return 0;
      } else if ((m->to - minr) >= (cnt - 1)) {
        *rec = minr;
        /* 
         *  This is the only place where we are motivated to 
         *  not honor the rule that we always return the lowest-
         *  numbered value that satisifies the request.
         *  If we didn't want to follow that rule here,
         *  then we could take the latter part of the segment
         *  instead and never break this interval into two.
         *  Of course, then the common case is that we would
         *  be allocating from the top down!  (i.e., start
         *  out with [0,MAX].  First allocation leaves it
         *  at [0,MAX-n].  etc.
         */
        if (minr + cnt > m->to) {
          m->to -= cnt;
          assert( m->to >= m->from );
        } else {
          fmapInsertEntry( fmap, i );
          m = &fmap->map[i];     /* Reestablish `m'; was invalidated by call */
          m[1].to = m[0].to;
          m[0].to = minr - 1;
          m[1].from = minr + cnt;
        }
        return 0;
      }
    }
  }
  return -1;
}


int fmapInsertQuick( struct FreeMap *self, unsigned key )
{
  unsigned i = self->last;

  assert( i < self->count );
#ifdef DEBUG
  printf( "insquick [%u] %u...  ", i, key );
#endif

  if ((key < self->map[i].from)
      && ((i == 0) || (key > self->map[i-1].to))) {
    /*
     *  We have found the place for it; it's just before the
     *  current entry
     */
#ifdef DEBUG
    printf( "ok " );
#endif

    if (key == (self->map[i].from - 1)) {
      /* And we can merge it with the current entry */
      self->map[i].from--;
      if ((i > 0) && ((self->map[i].from - 1) == self->map[i-1].to)) {
        /* these two entries are now adjacent; merge them */
        self->map[i-1].to = self->map[i].to;
        self->count--;
        memmove( &self->map[i],
                 &self->map[i+1],
                 sizeof( struct Interval ) * (self->count-i) );
        if (self->last >= self->count) {
          self->last--;
#ifdef DEBUG
          printf( "(crunch--) " );
#endif
        } else {
#ifdef DEBUG
          printf( "(crunch) " );
#endif
        }
      }
      assert( self->last < self->count );
#ifdef DEBUG
      printf( "shrink from\n" );
#endif
      return 1;
    } else if ((i > 0) && (key == (self->map[i-1].to + 1))) {
      /* And we can merge it with the previous entry */
      self->map[i-1].to++;
#ifdef DEBUG
      printf( "bump to\n" );
#endif
      return 1;
    } else {
      /* Cannot merge it either way; need to insert a new entry */
      fmapInsertEntry( self, i );
      self->map[i].from = self->map[i].to = key;
#ifdef DEBUG
      printf( "all-new\n" );
#endif
      return 1;
    }
  } 
#ifdef DEBUG
  printf( "---\n" );
#endif
  return 0;
}

int fmapInsert( struct FreeMap *self, unsigned key )
{
  struct Interval *p;
  if (fmapInsertQuick( self, key )) {
    return 1;
  }
  if (key < self->map[0].from) {
    self->last = 0;
    if (fmapInsertQuick( self, key )) {
      return 1;
    }
    assert( 0 );
  }
  assert( key <= self->map[self->count-1].to ); /* not implemented yet */

  assert( self->count >= 2 );

  p = bsearch( (void *)key,
               &self->map[1], 
               (self->count - 1), 
               sizeof( struct Interval ),
               cmpkeyi );
  if (!p) {
    return 0;
  }
  self->last = p - &self->map[0];
  if (fmapInsertQuick( self, key )) {
    return 1;
  }
  return 0;
}

void fmapPrint( struct FreeMap *imap )
{
  int i;

  for (i=0; i<imap->count; i++) {
    printf( "  %u.%c %9u .. %-9u\n", i, 
            (i == imap->last) ? '*' : ' ',
            imap->map[i].from, 
            imap->map[i].to );
  }
}

#ifdef UNIT_TEST

int main( int argc, const char **argv )
{
  struct FreeMap *m = newFreeMap();
  int i;
  int kset[500];

  if ((argc == 3) && (strcmp( argv[1], "-r" ) == 0)) {
    unsigned ops = 0;
    unsigned maxcount = 0;

    for (i=0; i<atoi(argv[2]); i++) {
      int n, j, nk, k, rc;

      m = newFreeMap();

      n = random() % 150;
      nk = 0;

      for (j=0; j<n; j++) {
        /*printf( "\n" );
          print( m );*/
        if ((nk == 0) || ((random() % 99) > 20)) {
          k = random() % 100;
          ops++;
          if (fmapDelete( m, k )) {
            kset[nk++] = k;
          }
        } else {
          k = random() % nk;
          ops++;
          rc = fmapInsert( m, kset[k] );
          assert( rc );
          kset[k] = kset[--nk];
        }
        if (m->count > maxcount) {
          maxcount = m->count;
        }
      }
      for (j=0; j<nk; j++) {
        /*printf( "\n(want to insert %d)\n", kset[j] );
        print( m );
        */
        rc = fmapInsert( m, kset[j] );
        assert( rc );
      }
      /*print( m );*/
      assert( m->count == 1 );
      assert( m->map[0].from == 0 );
    }
    printf( "%u operations passed, max count = %u\n", ops, maxcount );
    return 0;
  }

  for (i=1; i<argc; i++) {
    unsigned x, y;

    if (argv[i][0] == '+') {
      unsigned k = atoi( argv[i] );
      int rc = fmapInsert( m, k );
      printf( " insert %u => %s\n", k, rc ? "ok" : "failed" );
    } else if (sscanf( argv[i], "?%u,%u", &x, &y ) == 2) {
      unsigned r;
      printf( " find %u qty %u", x, y );
      if (fmapFindAndDeleteRange( m, x, y, &r ) < 0) {
        printf( " -- none found\n" );
      } else {
        printf( " -- found %u\n", r );
      }
    } else {
      unsigned k = atoi( argv[i] );
      int rc = fmapDelete( m, k );
      printf( " delete %u => %s\n", k, rc ? "ok" : "failed" );
    }
  }
  fmapPrint( m );
  return 0;
}

/*
Unit test cases:

  gcc -DDEBUG -g -Wall -DUNIT_TEST freemap.c

  ./a.out ?100,10 ?70,10 ?70,10 ?70,10 ?70,10 129 ?70,10 +129 ?70,10

*/

#endif



