#include "config.h"
#include <gd.h>
#include <rscheme.h>

/* from cports.h, which is not exported beyond iolib... */

void SOP_write( obj port, const char *src, UINT_32 len );

struct GD_SOP_IOCtx{
  struct gdIOCtx        ctx;
  obj                   port;
};

static void rs_gd_strout_free( gdIOCtx *_ctx )
{
  /* do nothing */
}

static void rs_gd_strout_putc( gdIOCtx *_ctx, int c )
{
  struct GD_SOP_IOCtx *ioc_p = (struct GD_SOP_IOCtx *)_ctx;
  char tmp[1];

  tmp[0] = c;
  SOP_write( ioc_p->port, &tmp[0], 1 );
}

static int rs_gd_strout_putbuf( gdIOCtx *_ctx, const void *buf, int len )
{
  struct GD_SOP_IOCtx *ioc_p = (struct GD_SOP_IOCtx *)_ctx;

  SOP_write( ioc_p->port, (const char *)buf, len );
  return len;
}

obj rs_gd_strout( obj port )
{
  obj ioc;
  struct GD_SOP_IOCtx *ioc_p;

  ioc = bvec_alloc( sizeof( struct GD_SOP_IOCtx ), byte_vector_class );
  ioc_p = (struct GD_SOP_IOCtx *)PTR_TO_DATAPTR( ioc );
  ioc_p->port = port;

  ioc_p->ctx.getC = NULL;               /* needed by read operations */
  ioc_p->ctx.getBuf = NULL;
  ioc_p->ctx.putC = rs_gd_strout_putc;
  ioc_p->ctx.putBuf = rs_gd_strout_putbuf;
  ioc_p->ctx.seek = NULL;               /* only needed by Gd2 */
  ioc_p->ctx.tell = NULL;               /* " */
#if LIBGD_IOCTX_HAS_FREE
  ioc_p->ctx.free = rs_gd_strout_free;
#else
  ioc_p->ctx.gd_free = rs_gd_strout_free;
#endif
  return ioc;
}
