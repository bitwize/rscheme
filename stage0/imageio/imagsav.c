/**********************************************
THIS FILE WAS AUTOMATICALLY COPIED FROM THE
RSCHEME SOURCE TREE, AND THE ORIGINAL MAY CHANGE.
HENCE, DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

#line 1 "modules/imageio/imagsav.c"
/*-----------------------------------------------------------------*-C-*---
 * File:    modules/imageio/imagsav.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.7
 * File mod date:    1997-11-29 23:10:51
 * System build:     v0.7.3.4-b7u, 2007-05-30
 * Owned by module:  imageio
 *
 *------------------------------------------------------------------------*/

#include <string.h>
#include <rscheme.h>
#include <rscheme/hashmain.h>
#include <rscheme/hashfn.h>
#include <rscheme/scheme.h>
#include <rscheme/allocns.h>
#include "imaglue.h"

enum {
    IMAGE_MODE_REF,
    IMAGE_MODE_GVEC,
    IMAGE_MODE_BVEC,
    NUM_IMAGE_MODES
};

struct image_header {
    UINT_32	num_objects;
    UINT_32	root_object;
};

struct section_header {
    UINT_32	num_objects;
    UINT_32	sectn_length;
};

struct iob_hdr {
    UINT_32	iob_size;
    obj		iob_class;
};


struct buffer_chain {
    obj		cur_buffer;
    UINT_32	total_len;
    char	*buf_ptr;	/* these should stay word aligned */
    char	*buf_lim;
    obj		first_cell;
    obj		prev_cell;
};

typedef struct image_save_context {
    obj		id_table;
    obj		slot_rewrite_table;
    obj		illegal_refs;
    struct buffer_chain	buf;
} save_ctx_t;


static obj marshall_id( save_ctx_t *ctx, obj thing )
{
obj id;

    if (OBJ_ISA_PTR(thing))
    {
	id = objecttable_lookup( ctx->id_table, obj_hash(thing), thing );
	if (EQ(id,FALSE_OBJ)) {
	    ctx->illegal_refs = cons( thing, ctx->illegal_refs );
#ifdef DEBUG_IMAGEIO
	    printf( "%#x invalid\n", thing );
#endif /* DEBUG_IMAGEIO */
	    return ZERO;
	}
	id = OBJ(VAL(id)+POINTER_TAG);
	/* printf( "%#x => %#x\n", thing, id ); */
    }
    else
	id = thing;
    return id;
}

#define BUF_DATA(p) ((char *)PTR_TO_DATAPTR(p) + sizeof(UINT_32))

static UINT_32 buf_posn( struct buffer_chain *buf )
{
    if (EQ(buf->cur_buffer,ZERO))
	return 0;
    else
	return buf->total_len 
	       + (buf->buf_ptr - BUF_DATA(buf->cur_buffer));
}

#define MIN_BUF_ALLOC  (48-4)

static void flush( struct buffer_chain *buf )
{
/* flush the current buffer */
UINT_32 N, *p;
obj cell;
    
    if (EQ(buf->cur_buffer,ZERO))
	return;
	
    cell = cons( buf->cur_buffer, NIL_OBJ );
    
    p = (UINT_32 *)PTR_TO_DATAPTR(buf->cur_buffer);
    N = buf->buf_ptr - (char *)(p+1);
    p[0] = N;
    buf->total_len += N;
    
    if (EQ(buf->prev_cell,ZERO))
	buf->first_cell = cell;
    else
	gvec_write(buf->prev_cell,SLOT(1),cell);
    buf->prev_cell = cell;
}

static void *reserve( struct buffer_chain *buf, UINT_32 bytes )
{
UINT_32 *place;

    if (buf->buf_ptr + bytes >= buf->buf_lim)
    {
    UINT_32 n;
    
	flush( buf );
	n = ((bytes < MIN_BUF_ALLOC) ? MIN_BUF_ALLOC : bytes);
	
	buf->cur_buffer = bvec_alloc( n + sizeof(UINT_32), byte_vector_class );
	buf->buf_ptr = BUF_DATA(buf->cur_buffer);
	memset( buf->buf_ptr, 0, n );
	buf->buf_lim = buf->buf_ptr + n;
    }
    place = (UINT_32 *)buf->buf_ptr;
    buf->buf_ptr += bytes;
    return place;
}

static void init_buf_chain( struct buffer_chain *buf )
{
    buf->first_cell = NIL_OBJ;
    buf->prev_cell = ZERO;
    buf->cur_buffer = ZERO;
    buf->total_len = 0;
    buf->buf_ptr = NULL;
    buf->buf_lim = NULL;
}

static void write_refs( save_ctx_t *ctx, obj sectn )
{
}

static void write_io_hdr( save_ctx_t *ctx, obj item )
{
struct iob_hdr *h;

    h = (struct iob_hdr *)reserve( &ctx->buf, sizeof(struct iob_hdr) );

    h->iob_size = SIZEOF_PTR(item);
    h->iob_class = marshall_id( ctx, CLASSOF_PTR(item) );
}

static void write_gvecs( save_ctx_t *ctx, obj sectn )
{
obj *body, item, rewrites, rplc = ZERO;
UINT_32 i, j, n, nxtj;

    for (i=0; i<SIZEOF_PTR(sectn); i+=SLOT(1))
    {
	item = gvec_read( sectn, i );
	write_io_hdr( ctx, item );
	body = (obj *)reserve( &ctx->buf, SIZEOF_PTR(item) );
	
	rewrites = objecttable_lookup( ctx->slot_rewrite_table,
				       obj_hash(item),
				       item );
	if (EQ(rewrites,FALSE_OBJ))
	    rewrites = NIL_OBJ;

	j = 0;
	n = SIZEOF_PTR(item);
	
	while (1)
	{
	    if (!EQ(rewrites,NIL_OBJ))
	    {
	    obj one_rewrite;
	
		assert(PAIR_P(rewrites));
	    
		one_rewrite = pair_car( rewrites );
		rewrites = pair_cdr( rewrites );
		
		assert(PAIR_P(one_rewrite));
		assert(OBJ_ISA_FIXNUM(pair_car( one_rewrite )));
		
		nxtj = FXWORDS_TO_RIBYTES( pair_car( one_rewrite ) );
		rplc = pair_cdr( one_rewrite );
		
#ifdef DEBUG_IMAGEIO
		printf( "Rewriting %#x[%d] to %#x\n", item, j/4, rplc );
#endif /* DEBUG_IMAGEIO */
		if (nxtj >= n)
		    nxtj = n;
	    }
	    else
		nxtj = n;
	    for (;j < nxtj; j += SLOT(1))
	    {
#ifdef DEBUG_IMAGEIO
		printf( "%#x[%d] : marshalling %#x\n", 
			item, j/4, gvec_read(item,j) );
#endif /* DEBUG_IMAGEIO */
		*body++ = marshall_id( ctx, gvec_read(item,j) );
	    }
	    if (nxtj < n)
	    {
#ifdef DEBUG_IMAGEIO
		printf( "%#x[%d] : marshalling %#x\n", 
			item, j/4, rplc );
#endif /* DEBUG_IMAGEIO */
		*body++ = marshall_id( ctx, rplc );
		j += SLOT(1);
		continue;
	    }
	    break;
	}
    }
}

static UINT_32 round_up( UINT_32 n )
{
    return ((n-1)|3)+1;
}

static void write_bvecs( save_ctx_t *ctx, obj sectn )
{
obj item;
UINT_32 i, reserve_len, *p;
void *body;

    for (i=0; i<SIZEOF_PTR(sectn); i+=SLOT(1))
    {
	item = gvec_read( sectn, i );
	write_io_hdr( ctx, item );
	reserve_len = round_up(SIZEOF_PTR(item));
	body = reserve( &ctx->buf, reserve_len );
	memcpy( body, PTR_TO_DATAPTR(item), SIZEOF_PTR(item) );
	memset( (char *)body + SIZEOF_PTR(item),
	  	0,
		reserve_len - SIZEOF_PTR(item) );
#ifdef PLATFORM_IS_LITTLE_ENDIAN
	for (p = body; p < (UINT_32 *)((char *)body + reserve_len); p++)
	  *p = BIG_ENDIAN_TO_HOST_32(*p);
#endif
    }
}

obj rs_save_image( obj sections, obj marshall_table,
			      obj fix_slot_table, obj root )
{
save_ctx_t ctx;
UINT_32 i, num_sects;
struct image_header *ihdr;

    init_buf_chain( &ctx.buf );
    ctx.id_table = marshall_table;
    ctx.illegal_refs = NIL_OBJ;
    ctx.slot_rewrite_table = fix_slot_table;
    
    ihdr = (struct image_header *)reserve( &ctx.buf, sizeof(struct image_header) );
    ihdr->num_objects = 0;
    
    num_sects = SIZEOF_PTR(sections)/SLOT(1);
    for (i=0; i<num_sects; i++)
    {
    obj s = gvec_read( sections, SLOT(i) );
    UINT_32 start_posn, num_items;
    struct section_header *sh;
    
	num_items = SIZEOF_PTR(s)/SLOT(1);
	ihdr->num_objects += num_items;
	
#ifdef DEBUG_IMAGEIO
	printf( "section %u: %u objects\n", i, num_items );
#endif /* DEBUG_IMAGEIO */
	
	sh = (struct section_header *)
		    reserve( &ctx.buf, sizeof(struct section_header) );
	sh->num_objects = num_items;
	start_posn = buf_posn( &ctx.buf );

	switch (i)
	{
	    case IMAGE_MODE_REF:	
		write_refs( &ctx, s );
		break;
	    case IMAGE_MODE_GVEC:
		write_gvecs( &ctx, s );
		break;
	    case IMAGE_MODE_BVEC:
		write_bvecs( &ctx, s );
		break;
	}
	sh->sectn_length = buf_posn( &ctx.buf ) - start_posn;
#ifdef DEBUG_IMAGEIO
	printf( "closing section %u.  %u bytes written\n",
		i, sh->sectn_length );
#endif /* DEBUG_IMAGEIO */
    }
    ihdr->root_object = VAL(marshall_id( &ctx, root ));
    
    flush( &ctx.buf );
    /* printf( "%u total bytes\n", ctx.buf.total_len ); */
    return make3( vector_class,
    		  int2fx(ctx.buf.total_len), 
		  ctx.buf.first_cell, 
		  ctx.illegal_refs );
}


#include "compres.ci"
#include "uncompr.ci"

obj rs_extract( UINT_8 *source, UINT_32 source_cnt )
{
struct image_source src;
struct buffer_chain buf;

    init_buf_chain( &buf );
    
    src.ptr = source;
    src.limit = src.ptr + source_cnt;
    init_hist( src.hist );
    
    while (src.ptr < src.limit)
    {
    UINT_32 *w;
    
	get_words( &src, w=(UINT_32*)reserve( &buf, 4 ), 4 );
#ifdef DEBUG_COMPRESSION
	printf( " %08x\n", *w );
#endif /* DEBUG_COMPRESSION */
    }
    flush( &buf );
    return make2( vector_class,
    		  int2fx(buf.total_len), 
		  buf.first_cell );
}

#include "imaglod.ci"

obj rs_load_image( UINT_8 *source, UINT_32 source_cnt, obj refs )
{
struct image_source src;

    src.ptr = source;
    src.limit = src.ptr + source_cnt;
    init_hist( src.hist );

    return load_image( &src, 
    			  (obj *)PTR_TO_DATAPTR(refs), 
			  SIZEOF_PTR(refs)/SLOT(1) );
}

