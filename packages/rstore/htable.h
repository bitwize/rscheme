#ifndef _H_HTABLE
#define _H_HTABLE

struct htent {
    struct htent *next;
    void	 *key;
    void	 *value;
};

struct htable {
    unsigned		table_mask;
    struct htent	**table;
};

void htable_init( struct htable *tbl );
void htable_free( struct htable *tbl );

struct htent *htable_lookup( struct htable *tbl, void *key );
struct htent *htable_insert( struct htable *tbl, void *key );
void *htable_remove( struct htable *tbl, void *key );   /* returns the value */

#endif /* _H_HTABLE */
