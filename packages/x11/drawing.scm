
(define-class <X-drawing-context> (<object>)
  x-display-ptr
  x-window-id
  xid
  origin-x
  origin-y
  x-window
  origin
  clip-rect)

;;

(define-X-glue (fill-rectangle (ctx <X-drawing-context>)
				  (rect <rect>))
{
  XFillRectangle( ctx_dsp, ctx_win, ctx_gc,
		  RECT_X(rect) + ctx_origin_x, 
		  RECT_Y(rect) + ctx_origin_y,
		  RECT_W(rect), RECT_H(rect) );
  RETURN0();
})

(define-X-glue (draw-rectangle (ctx <X-drawing-context>)
				  (rect <rect>))
{
  XDrawRectangle( ctx_dsp, ctx_win, ctx_gc,
		  RECT_X(rect) + ctx_origin_x,
		  RECT_Y(rect) + ctx_origin_y,
		  RECT_W(rect), RECT_H(rect) );
  RETURN0();
})

(define-X-glue (draw-line (ctx <X-drawing-context>)
			  (from <point>)
			  (to <point>))
{
   XDrawLine( ctx_dsp, ctx_win, ctx_gc,
	      POINT_X(from) + ctx_origin_x,
	      POINT_Y(from) + ctx_origin_y,
	      POINT_X(to) + ctx_origin_x,
	      POINT_Y(to) + ctx_origin_y );
   RETURN0();
})

(define-X-glue (draw-box (ctx <X-drawing-context>)
			    (frame <rect>)
			    (edge_info <raw-string>)
			    (colors <vector>))
{
unsigned char edge;
unsigned char *info = edge_info;
color_t c, prev;

int x = RECT_X(frame) + ctx_origin_x;
int y = RECT_Y(frame) + ctx_origin_y;
int w = RECT_W(frame);
int h = RECT_H(frame);

  prev = 0xFFFFFFFF; /* impossible color */
  while (1)
    {
      edge = *info++;
      c = fx2int( gvec_read( colors, SLOT(*info++ - '0') ) );
      if (c != prev)
      {
	XSetForeground( ctx_dsp, ctx_gc, c );
	prev = c;
      }
      switch (edge)
	{
	case '.':
	  goto box_done;
	case 't':
	  XFillRectangle( ctx_dsp, ctx_win, ctx_gc, x, y, w, 1 );
	  y++;
	  h--;
	  break;
	case 'b':
	  h--;
	  XFillRectangle( ctx_dsp, ctx_win, ctx_gc, x, y+h, w, 1 );
	  break;
	case 'l':
	  XFillRectangle( ctx_dsp, ctx_win, ctx_gc, x, y, 1, h );
	  x++;
	  w--;
	  break;
	case 'r':
	  w--;
	  XFillRectangle( ctx_dsp, ctx_win, ctx_gc, x+w, y, 1, h );
	  break;
	}
    }
 box_done:
  XFillRectangle( ctx_dsp, ctx_win, ctx_gc, x, y, w, h );
  RETURN0();
})

(define-X-glue (set-tiled (ctx <X-drawing-context>) (tile <X-pixmap>))
{
  XSetTile( ctx_dsp, ctx_gc, tile_xid );
  RETURN0();
})

(define-X-glue (set-stipple (ctx <X-drawing-context>) (stipple <X-pixmap>))
{
  XSetStipple( ctx_dsp, ctx_gc, stipple_xid );
  RETURN0();
})

(define-X-glue (set-tile-origin (ctx <X-drawing-context>) (at <point>))
{
  XSetTSOrigin( ctx_dsp, ctx_gc, 
	       POINT_X(at) + ctx_origin_x,
	       POINT_Y(at) + ctx_origin_y );
  RETURN0();
})
			
(define-X-glue (set-clip-mask (ctx <X-drawing-context>) 
		       (mask <X-pixmap>)
		       (at <point>))
{
  XSetClipMask( ctx_dsp, ctx_gc, mask_xid );
  XSetClipOrigin( ctx_dsp, ctx_gc, 
		  POINT_X(at) + ctx_origin_x,
		  POINT_Y(at) + ctx_origin_y );
  RETURN0();
})

(define-X-glue (set-no-clip-mask (ctx <X-drawing-context>))
{
  XSetClipMask( ctx_dsp, ctx_gc, None );
  RETURN0();
})

(define-X-glue (set-foreground (ctx <X-drawing-context>)
				  (color <raw-int>))
{
  XSetForeground( ctx_dsp, ctx_gc, color );
  RETURN0();
})

(define-X-glue (set-background (ctx <X-drawing-context>)
				  (color <raw-int>))
{
  XSetBackground( ctx_dsp, ctx_gc, color );
  RETURN0();
})


(define-X-glue (copy-area (from <X-drawable>)
			     (from_rect <rect>)
			     (to <X-drawing-context>)
			     (to_point <point>))
{
  XCopyArea( to_dsp, from_xid, to_win, to_gc,
		     RECT_X(from_rect),
		     RECT_Y(from_rect),
		     RECT_W(from_rect),
		     RECT_H(from_rect),
		     POINT_X(to_point) + to_origin_x,
		     POINT_Y(to_point) + to_origin_y );
  RETURN0();
})

(define-X-glue (set-fill-style (ctx <X-drawing-context>) style)
  literals: ('FillSolid 
	     'FillOpaqueStippled 
	     'FillStippled 
	     'FillTiled)
{
  int style_choice = FillSolid;

  if (EQ(style,LITERAL(1)))
    style_choice = FillOpaqueStippled;
  else if (EQ(style,LITERAL(2)))
    style_choice = FillStippled;
  else if (EQ(style,LITERAL(3)))
    style_choice = FillTiled;
  
  XSetFillStyle( ctx_dsp, ctx_gc, style_choice );
  RETURN0();
})

(define-method initialize ((self <X-drawing-context>))
  (set-origin! self (make <point>
			  x: 0
			  y: 0))
  (set-clip-rect! self (make-rect -8000 -8000 16000 16000))
  self)

(define-X-glue (create-drawing-context (window <X-drawable>))
  literals: ((& <X-drawing-context>)
	     (& initialize))
{
obj gc;
GC the_gc;

  the_gc = XCreateGC( window_dsp, window_xid, 0, 0 );

  gc = alloc( SLOT(8), TLREF(0) );
  gvec_write_fresh( gc, SLOT(0), gvec_read( raw_window, SLOT(0) ) );
  gvec_write_fresh( gc, SLOT(1), gvec_read( raw_window, SLOT(1) ) );
  gvec_write_fresh( gc, SLOT(2), RAW_PTR_TO_OBJ(the_gc) );
  gvec_write_fresh( gc, SLOT(3), ZERO );
  gvec_write_fresh( gc, SLOT(4), ZERO );
  gvec_write_fresh( gc, SLOT(5), raw_window );
  gvec_write_fresh( gc, SLOT(6), UNINITIALIZED_OBJ );
  gvec_write_fresh( gc, SLOT(7), UNINITIALIZED_OBJ );

  REG0 = gc;
  APPLY(1, TLREF(1));
})

(define-X-glue (set-dashes (ctx <X-drawing-context>)
			      (dash_offset <raw-int>)
			      (dashes <vector>))
{
  char dash_list[10];
  int i, n;

  n = SIZEOF_PTR( dashes ) / SLOT(1);
  if (n > 10)
    n = 10;

  for (i=0; i<n; i++)
    dash_list[i] = fx2int( gvec_read( dashes, SLOT(i) ) );
  
  XSetDashes( ctx_dsp, ctx_gc, dash_offset, dash_list, n );
  RETURN0();
})


(define-X-glue (set-line-attributes (ctx <X-drawing-context>)
				       (line_width <raw-int>)
				       line_style
				       cap_style
				       join_style)
  literals: ('LineSolid 
	     'LineOnOffDash 
	     'LineDoubleDash
	     'CapButt
	     'CapNotLast
	     'CapRound
	     'CapProjecting
	     'JoinMiter
	     'JoinRound
	     'JoinBevel)
{
 int line_style_choice, cap_style_choice, join_style_choice;

 line_style_choice = LineSolid;
 if (EQ(line_style,LITERAL(1)))
   line_style_choice = LineOnOffDash;
 else if (EQ(line_style,LITERAL(2)))
   line_style_choice = LineDoubleDash;

 cap_style_choice = CapButt;
 if (EQ(cap_style,LITERAL(4)))
   cap_style_choice = CapNotLast;
 else if (EQ(cap_style,LITERAL(5)))
   cap_style_choice = CapRound;
 else if (EQ(cap_style,LITERAL(6)))
   cap_style_choice = CapProjecting;

 join_style_choice = JoinMiter;
 if (EQ(join_style,LITERAL(8)))
   join_style_choice = JoinRound;
 else if (EQ(join_style,LITERAL(9)))
   join_style_choice = JoinBevel;

 XSetLineAttributes( ctx_dsp, ctx_gc, line_width, 
		     line_style_choice, 
		     cap_style_choice,
		     join_style_choice );
 RETURN0();
})

