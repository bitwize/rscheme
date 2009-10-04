#include <string.h>
#include "alloc.h"
#include "htable.h"

static unsigned ptr_hash( void *ptr )
{
unsigned long h = (unsigned long)ptr;

    h = h + (h >> 8) + (h >> 16) + (h >> 24);
    return h;
}

void htable_free( struct htable *tbl )
{
  unsigned i;

  for (i=0; i<=tbl->table_mask; i++)
    {
      struct htent *j, *n;

      for (j=tbl->table[i]; j;)
        {
          n = j->next;
          free( j );
          j = n;
        }
    }
  free( tbl->table );
}

void htable_init( struct htable *tbl )
{
  tbl->table_mask = 511;
  tbl->table = ALLOCN( struct htent *, tbl->table_mask + 1 );
  memset( tbl->table, 0, 
          sizeof( struct htent * ) * (tbl->table_mask + 1) );
}

struct htent *htable_lookup( struct htable *tbl, void *key )
{
struct htent *e;
unsigned h;

    h = ptr_hash(key) & tbl->table_mask;
    e = tbl->table[h];
    while (e)
    {
	if (e->key == key)
	    return e;
	e = e->next;
    }
    return NULL;
}

struct htent *htable_insert( struct htable *tbl, void *key )
{
struct htent *e;
unsigned h;

    h = ptr_hash(key) & tbl->table_mask;
    e = ALLOC(struct htent);
    e->next = tbl->table[h];
    e->key = key;
    e->value = NULL;
    tbl->table[h] = e;
    return e;
}

void *htable_remove( struct htable *tbl, void *key )
{
struct htent *prev, *e;
unsigned h;

    h = ptr_hash(key) & tbl->table_mask;
    e = tbl->table[h];
    prev = NULL;
    while (e)
    {
	if (e->key == key)
	{
	void *v = e->value;
	
	    if (prev)
		prev->next = e->next;
	    else
		tbl->table[h] = e->next;
	    free(e);
	    return v;
	}
	prev = e;
	e = e->next;
    }
    return NULL;
}

#ifdef UNIT_TEST

int main( int argc, const char **argv )
{
struct htable t;
int i;

    htable_init( &t );
    
    for (i=1; i<argc; i++)
    {
	void *k, *v;
	
	if (strchr(argv[i],'='))
	{
	    sscanf( argv[i], "%d=%d", &k, &v );
	    htable_insert( &t, k )->value = v;
	    printf( "insert %d => %d\n", k, v );
	}
	else if (argv[i][0] == '-')
	{
	    sscanf( argv[i]+1, "%d", &k );
	    v = htable_remove( &t, k );
	    printf( "remove %d => %d\n", k, v );
	}
	else
	{
	struct htent *e;
	
	    sscanf( argv[i], "%d", &k );
	    printf( "lookup %d => ", k );
	    e = htable_lookup( &t, k );
	    if (e)
		printf( "%d\n", e->value );
	    else
		printf( "not found\n" );
	}
    }
    return 0;
}

#endif
