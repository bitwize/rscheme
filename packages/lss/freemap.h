#ifndef _H_FREEMAP
#define _H_FREEMAP

struct Interval;

struct FreeMap {
  struct Interval *map;
  unsigned count, capacity, last;
};

int initFreeMap( struct FreeMap *fmap );

int fmapDelete( struct FreeMap *fmap, unsigned key );
int fmapInsert( struct FreeMap *fmap, unsigned key );
int fmapDeleteRange( struct FreeMap *fmap, unsigned from, unsigned to );
int fmapFindAndDeleteRange( struct FreeMap *fmap, unsigned minr, unsigned cnt,
                            unsigned *rec );
void fmapPrint( struct FreeMap *imap );

#endif /* _H_FREEMAP */
