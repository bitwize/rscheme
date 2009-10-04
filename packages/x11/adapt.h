#ifndef _H_XADAPT
#define _H_XADAPT

#include <X11/Xlib.h>
#include <rscheme/obj.h>
#ifdef __cplusplus
extern "C" {
#endif
#include <gd.h>
#ifdef __cplusplus
}
#endif

typedef unsigned long color_t;

#define DISPLAY_SCREEN(d) (fx2int(gvec_read((d),SLOT(3))))
#define DISPLAY_COLORMAP(d) FX_TO_XID(gvec_read((d),SLOT(4)))

#define OBJ_TO_DISPLAY(x) ((Display *)OBJ_TO_RAW_PTR(x))

#define FX_TO_XID(fx) ((XID)(fx2int(fx)))
#define XID_TO_FX(xid)  int2fx((int)(xid))

#define FX_TO_GC(fx) ((GC)(OBJ_TO_RAW_PTR(fx)))

#define RECT_X(r)  (fx2int(gvec_read((r),SLOT(0))))
#define RECT_Y(r)  (fx2int(gvec_read((r),SLOT(1))))
#define RECT_W(r)  (fx2int(gvec_read((r),SLOT(2))))
#define RECT_H(r)  (fx2int(gvec_read((r),SLOT(3))))

#define POINT_X(p) (fx2int(gvec_read((p),SLOT(0))))
#define POINT_Y(p) (fx2int(gvec_read((p),SLOT(1))))

#define SIZE_W(s)  (fx2int(gvec_read((s),SLOT(0))))
#define SIZE_H(s)  (fx2int(gvec_read((s),SLOT(1))))

#define FX_TO_COLOR(fx) fx2int(fx)

/* manually kept up-to-date, for now... */

#define X_DRAWING_CONTEXT__ORIGIN_X    SLOT(3)
#define X_DRAWING_CONTEXT__ORIGIN_Y    SLOT(4)
#define X_DRAWING_CONTEXT__ORIGIN      SLOT(6)
#define X_DRAWING_CONTEXT__CLIP_RECT   SLOT(7)

void put_pixel( XImage *image, int x, int y, color_t pixel );

void PerCharWidths( XFontStruct *font, 
		       unsigned char *text_str, 
		       unsigned *widths, 
		       UINT_32 n );


struct text_rendition {
  Display *ctx_dsp;
  Drawable ctx_win;
  GC ctx_gc;
  XFontStruct *font;
  int origin_x, origin_y;
  int line_height;
  int line_width;
  obj text;
  color_t text_color;
  color_t selection_color;
  UINT_32 sel_from;
  UINT_32 sel_to;
  obj     sel_bleed;
};


int layout_text( struct text_rendition *info );

#endif /* _H_XADAPT */
