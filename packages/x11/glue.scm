
(define-class <X-display> (<object>)
  x-display-ptr
  x-display-default-root-window
  xid-table
  x-screen
  x-color-map
  x-default-depth)

(define-class <X-drawable> (<object>) :abstract
  x-display-ptr
  xid
  x-display
  local-object)

(define-class <X-window> (<X-drawable>))

(define-class <X-pixmap> (<X-drawable>)
  pixmap-owner)

(define *all-displays* '())

(define-method initialize ((self <X-display>))
  (set-xid-table! self (make-table eq? integer->hash))
  (set! *all-displays* (cons self *all-displays*))
  self)

(define-method initialize ((self <X-window>))
 (table-insert! (xid-table (x-display self)) (xid self) self)
 self)

(define-glue (open-display display_name)
  literals: ((& <X-display>)
	     (& initialize))
{
  Display *dsp;

  COUNT_ARGS(1);
  if (STRING_P(display_name))
    {
      dsp = XOpenDisplay( string_text( display_name ) );

      if (dsp)
	{
	  int screen;
	  screen = DefaultScreen(dsp);

	  REG0 = alloc( SLOT(6), TLREF(0) );
	  gvec_write_non_ptr( REG0, SLOT(0), 
			      RAW_PTR_TO_OBJ( dsp ) );
	  gvec_write_non_ptr( REG0, SLOT(1), 
			      XID_TO_FX( DefaultRootWindow(dsp) ) );
	  gvec_write_non_ptr( REG0, SLOT(2), UNINITIALIZED_OBJ );
	  gvec_write_non_ptr( REG0, SLOT(3), int2fx(screen) );
	  gvec_write_non_ptr( REG0, SLOT(4), 
			      XID_TO_FX(DefaultColormap(dsp,screen)) );
	  gvec_write_non_ptr( REG0, SLOT(5),
			      int2fx(DefaultDepth(dsp,screen)) );
	  APPLY(1,TLREF(1));
	}
      else
	{
	  REG0 = FALSE_OBJ;
	  RETURN1();
	}
    }
  else
    {
      REG0 = FALSE_OBJ;
      RETURN1();
    }
})

(define-X-glue (create-pixmap (assoc <X-window>)
				 (size <size>)
				 (depth <raw-int>))
 literals: ((& <X-pixmap>) (& initialize))
{
  XID pm;
  obj the_pm;

  pm = XCreatePixmap( assoc_dsp, assoc_xid, 
		      SIZE_W(size), SIZE_H(size), 
		      depth );

  the_pm = alloc( SLOT(5), TLREF(0) );
  gvec_write_fresh( the_pm, SLOT(0), RAW_PTR_TO_OBJ(assoc_dsp) );
  gvec_write_fresh( the_pm, SLOT(1), XID_TO_FX(pm) );
  gvec_write_fresh( the_pm, SLOT(2), gvec_read(raw_assoc,SLOT(2)) );
  gvec_write_fresh( the_pm, SLOT(3), FALSE_OBJ );
  gvec_write_fresh( the_pm, SLOT(4), raw_assoc );
  REG0 = the_pm;
  APPLY(1,TLREF(1));
})

(define-glue (set-window-attributes win)
  literals: ('#(background-pixmap:
                override-redirect?:
		background-color:)
             (& <X-window>))
{
  XSetWindowAttributes a;
  unsigned long mask = 0;
  obj key, value, keys = LITERAL(0);
  Window win_xid;
  Display *win_dsp;
  unsigned i, j;

  if (!OBJ_ISA_PTR_OF_CLASS(win,TLREF(1)))
    {
      scheme_error( "set-window-attributes: ~s is not an <X-window>", 1, win );
    }
  win_dsp = OBJ_TO_DISPLAY( gvec_read( win, SLOT(0) ) );
  win_xid = FX_TO_XID( gvec_read( win, SLOT(1) ) );

  for (i=2; i<arg_count_reg; i+=2)
    {
      key = reg_ref(i-1);
      value = reg_ref(i);
      
      for (j=0; j<3; j++) 
	{
	  if (EQ(gvec_read(keys,SLOT(j)),key))
	    {
	      switch (j)
		{
		case 0: /* background-pixmap */
		  mask |= CWBackPixmap;
		  a.background_pixmap = FX_TO_XID(value);
		  break;
		case 1: /* override-redirect? */
		  mask |= CWOverrideRedirect;
		  a.override_redirect = truish(value);
		  break;
		case 2: /* background-color: */
		  mask |= CWBackPixel;
		  a.background_pixel = FX_TO_COLOR(value);
		  break;
		}
	      goto ok;
	    }
	}
      scheme_error( "set-window-attributes: `~s' is not an accepted keyword",
		    1, key );
    ok: /* nothing */;
    }
  XChangeWindowAttributes( win_dsp, win_xid, mask, &a );
  RETURN0();
})

(define-X-glue (create-window (parent <X-window>)
				 (frame <rect>)
				 (borderw <raw-int>)
				 (depth <raw-int>)
				 input_only_q)
 literals: ((& <X-window>) (& initialize))
{
  Window win;
  obj the_win;

  win = XCreateWindow( parent_dsp, parent_xid,
		       RECT_W(frame), RECT_H(frame), 
		       RECT_X(frame), RECT_Y(frame),
		       borderw, depth,
		       truish(input_only_q) ? InputOnly : InputOutput,
		       CopyFromParent,
		       0, NULL );

  the_win = alloc( SLOT(4), TLREF(0) );
  gvec_write_fresh( the_win, SLOT(0), RAW_PTR_TO_OBJ(parent_dsp) );
  gvec_write_fresh( the_win, SLOT(1), XID_TO_FX(win) );
  gvec_write_fresh( the_win, SLOT(2), gvec_read(raw_parent,SLOT(2)) );
  gvec_write_fresh( the_win, SLOT(3), FALSE_OBJ );
  REG0 = the_win;
  APPLY(1,TLREF(1));		   
})
				 
(define-glue (create-simple-window display parent frame borderw bg fg)
 literals: ((& <X-display>)
	    (& <rect>)
	    (& <X-window>)
	    (& initialize))
{
  COUNT_ARGS(6);
  if (OBJ_ISA_PTR_OF_CLASS( display, TLREF(0) )
      && OBJ_ISA_FIXNUM( parent )
      && OBJ_ISA_PTR_OF_CLASS( frame, TLREF(1) )
      && OBJ_ISA_FIXNUM( borderw )
      && OBJ_ISA_FIXNUM( bg )
      && OBJ_ISA_FIXNUM( fg ))
    {
      Display *dsp;
      Window win;
      obj the_win;

      dsp = OBJ_TO_DISPLAY( gvec_read( display, SLOT(0) ) );
      win = XCreateSimpleWindow( dsp, 
				FX_TO_XID(parent),
				RECT_X(frame),
				RECT_Y(frame),
				RECT_W(frame),
				RECT_H(frame),
				fx2int(borderw),
				FX_TO_COLOR(bg),
				FX_TO_COLOR(fg) );

      the_win = alloc( SLOT(4), TLREF(2) );
      gvec_write_fresh( the_win, SLOT(0), RAW_PTR_TO_OBJ(dsp) );
      gvec_write_fresh( the_win, SLOT(1), XID_TO_FX(win) );
      gvec_write_fresh( the_win, SLOT(2), display );
      gvec_write_fresh( the_win, SLOT(3), FALSE_OBJ );
      REG0 = the_win;
    }
  else
    {
      scheme_error( "bad arg", 0 );
    }
  APPLY(1,TLREF(3));
})

(define-X-glue (set-window-frame (window <X-window>) (frame <rect>))
{
  XMoveResizeWindow( window_dsp, window_xid, 
		     RECT_X(frame), RECT_Y(frame),
		     RECT_W(frame), RECT_H(frame) );
  RETURN0();
})

(define-X-glue (get-geometry (window <X-window>))
 literals: ((& <rect>))
{
  int x, y;
  Window root;
  unsigned w, h, bw, d;

  if (XGetGeometry( window_dsp, window_xid, 
		    &root, &x, &y, &w, &h, 
		    &bw, &d ))
    {
      REG0 = make4( TLREF(0), int2fx(x), int2fx(y), int2fx(w), int2fx(h) );
      REG1 = int2fx(d);
      REG3 = XID_TO_FX(root);
      REG2 = int2fx(bw);
    }
  else
    {
      REG0 = FALSE_OBJ;
      REG1 = FALSE_OBJ;
      REG2 = FALSE_OBJ;
      REG3 = FALSE_OBJ;
    }
  RETURN(4);
})

(define-X-glue (window-op (window <X-window>)) :template
{
  USE_FUNCTION_ENVT();
  switch (fx2int(envt_reg))
    {
    case 0: 
      XMapRaised( window_dsp, window_xid );
      break;
    case 1:
      XMapWindow( window_dsp, window_xid );
      break;
    case 2:
      XUnmapWindow( window_dsp, window_xid );
      break;
    case 3:
      { obj the_display;

	the_display = gvec_read( raw_window, SLOT(2) );
	objecttable_remove( gvec_read(the_display, SLOT(2)),
			    rehash_fixnum( int2fx(window_xid) ),
			    int2fx(window_xid) );
	XDestroyWindow( window_dsp, window_xid );
      }
      break;
    }
  RETURN0();
})

(define map-raised (make <closure> environment: 0 template: window-op))
(define map-window (make <closure> environment: 1 template: window-op))
(define unmap-window (make <closure> environment: 2 template: window-op))
(define destroy-window (make <closure> environment: 3 template: window-op))

#|
(define-glue (map-raised w)
  literals: ((& <X-window>))
{
  COUNT_ARGS(1);
  if (OBJ_ISA_PTR_OF_CLASS( w, TLREF(0) ))
    {
      XMapRaised( OBJ_TO_DISPLAY(gvec_read(w,SLOT(0))),
		  FX_TO_XID(gvec_read(w,SLOT(1))) );
    }
  else
    {
      scheme_error( "bad arg", 0 );
    }
  RETURN0();
})
|#

(define-class <X-event> (<object>) :bvec)

(define-X-glue (event-getter (dsp <X-display>)) 
  :template
  literals: ((& <X-event>))
{
XEvent *event;

  USE_FUNCTION_ENVT();
  REG0 = alloc( sizeof( XEvent ), TLREF(0) );
  event = (XEvent *)PTR_TO_DATAPTR(REG0);
  memset( event, 0, sizeof(XEvent) );

  if (EQ(envt_reg,ZERO)) 
    { 
      XNextEvent( dsp, event );
    }
  else
    {
      if (!XCheckMaskEvent( dsp, ~0, event ))
	REG0 = FALSE_OBJ;
    }
  RETURN1();
})

(define get-event 
  (make <closure> environment: 0 template: event-getter))
(define get-event/no-block 
  (make <closure> environment: 1 template: event-getter))

(define *last-display* #f)

(define-X-glue (event-info (event <X-event>))
 literals: ((& <X-display>) 
	    (& *last-display*)
	    (& *all-displays*)
	    (& $event-codes))
{
  Display *dsp;
  Window win;
  
  dsp = event->xany.display;
  win = event->xany.window;
      
  {
    UINT_32 offset = SLOT( event->xany.type );
    obj tbl = TLREF(3);

    if (offset < SIZEOF_PTR(tbl))
      REG0 = gvec_read( tbl, offset );
    else 
      REG0 = FALSE_OBJ;
    if (EQ(REG0,FALSE_OBJ))
      REG0 = int2fx( event->xany.type );
  }

  if (dsp)
    {
      REG1 = TLREF(1);
      
      if (!OBJ_ISA_PTR_OF_CLASS(REG1,TLREF(0))
	  || (dsp != OBJ_TO_DISPLAY(gvec_read(REG1,SLOT(0)))))
	{
	  obj p;
	  
	  for (p = TLREF(2); PAIR_P(p); p=pair_cdr(p))
	    {
	      if (dsp == OBJ_TO_DISPLAY(gvec_read(pair_car(p),SLOT(0))))
		{
		  REG1 = pair_car(p);
		  TLSET(1,REG1);
		  goto ok;
		}
	    }
	  scheme_error( "~s: x display not found!", 1, RAW_PTR_TO_OBJ(dsp) );
	}
    ok:
      REG2 = objecttable_lookup( gvec_read( REG1, SLOT(2) ),
				 rehash_fixnum( int2fx(win) ),
				 int2fx(win) );
    }
  else
    {
      REG1 = FALSE_OBJ;
      REG2 = FALSE_OBJ;
    }
  RETURN(3);
})
#|
(define-X-glue (destroy-window (window <X-window>))
{
  obj the_display = gvec_read( raw_window, SLOT(2) );

  objecttable_remove( gvec_read(the_display, SLOT(2)),
		      rehash_fixnum( int2fx(window_win) ),
		      int2fx(window_win) );
  XDestroyWindow( window_dsp, window_win );
  RETURN0();
})
|#

(define-glue (display-op display fn_id)
{
  Display *dsp = OBJ_TO_DISPLAY(gvec_read(display,SLOT(0)));

  switch (fx2int(fn_id))
    {
    case 0:
      XSync( dsp, 0 );
      break;
    case 1:
      XFlush( dsp );
      break;
    }
  RETURN0();
})

(define (x-sync (display <X-display>))
 (display-op display 0))

(define (x-flush (display <X-display>))
 (display-op display 1))

(define-X-glue (select-input (window <X-window>)
				(event_mask <raw-int>))
{
  XSelectInput( window_dsp, window_xid, event_mask );
  RETURN0();
})

(define-X-glue (set-std-properties (window <X-window>)
				      (title <raw-string>)
				      (icon_title <raw-string>)
				      (frame <rect>)
				      movableq
				      sizeableq)
{
XSizeHints hint;
  
  hint.x = RECT_X(frame);
  hint.y = RECT_Y(frame);
  hint.width = RECT_W(frame);
  hint.height = RECT_H(frame);
  hint.flags = (truish(movableq) ? PPosition : 0)
               | (truish(sizeableq) ? PSize : 0);
  XSetStandardProperties( window_dsp,
			  window_xid,
			  title,
			  icon_title,
			  None,
			  NULL,
			  0,
			  &hint );
  RETURN0();
})

#|
(define-ffi (create-pixmap (display <raw-ptr>)
			   (associate <xid>)
			   (w <raw-int>)
			   (h <raw-int>)
			   (depth <raw-int>)) => <xid>
  "XCreatePixmap")

(define-ffi (move-window (display <raw-ptr>)
			 (win <xid>)
			 (x <raw-int>)
			 (y <raw-int>))
  "XMoveWindow")

(define-ffi (free-gc (display <raw-ptr>)
		     (gc <raw-ptr>))
  "XFreeGC")

(define-ffi (close-display (display <raw-ptr>))
  "XCloseDisplay")

(define-ffi (set-std-properties (display <raw-ptr>)
				(window <xid>)
				(title <raw-string>)
				(icon-title <raw-string>)
				(x <raw-int>)
				(y <raw-int>)
				(w <raw-int>)
				(h <raw-int>))
  "setStdProperties")

(define-ffi (create-gc (display <raw-ptr>)
		       (window <xid>)) => <raw-ptr>
  "createGC")

(define-struct-size <color-scheme> "ColorScheme")
(define-struct-size <box-scheme> "BoxScheme")
(define-struct-size <box> "Box")

(define-ffi (color-scheme-init (color-scheme <struct>)
			       (display <raw-ptr>)
			       (colors <raw-string>))
  "initColorScheme")

(define-ffi (color-scheme-get-color (color-scheme <struct>)
				    (color-index <raw-int>)) => <raw-int>
  "ColorScheme_getColor")
				    

(define-ffi (box-scheme-init-from-string (box-scheme <struct>)
					 (color-scheme <struct>)
					 (layout <raw-string>))
  "initBoxSchemeFromString")

(define-ffi (box-init (box <struct>)
		      (box-scheme <struct>)
		      (window <xid>)
		      (gc <raw-ptr>)
		      (x <raw-int>)
		      (y <raw-int>)
		      (w <raw-int>)
		      (h <raw-int>)
		      (fill <raw-int>))
  "initBox")

(define-ffi (box-draw (box <struct>))
  "drawBox")

;; returns 0 or the event type (see events.scm)

(define-ffi (get-event (display <raw-ptr>)
		       (event <struct>)) => <raw-int>
  "getEvent")

(define-ffi (wait-event (display <raw-ptr>)
			(event <struct>)) => <raw-int>
  "waitEvent")

(define-ffi (select-input* (display <raw-ptr>)
			   (window <xid>)
			   (event-mask <raw-int>))
  "XSelectInput")

(define-struct-size <event> "XEvent")

(define-ffi (key-event->key (event <struct>)) => <raw-string>
  "decodeKeyEvent")

(define-ffi (refresh-keyboard-mapping (event <struct>))
  "XRefreshKeyboardMapping")

(define-ffi (set-foreground (display <raw-ptr>)
			    (gc <raw-ptr>)
			    (color <raw-int>))
  "XSetForeground")

(define-ffi (set-background (display <raw-ptr>)
			    (gc <raw-ptr>)
			    (color <raw-int>))
  "XSetBackground")

#|
(define-ffi (draw-string (display <raw-ptr>)
			 (window <xid>)
			 (gc <raw-ptr>)
			 (x <raw-int>)
			 (y <raw-int>)
			 (string <raw-string>)
			 (length <raw-int>))
  "XDrawString")
|#


(define-accessor (crossing-event->window (event <struct>)) => <xid>
  ("XEvent" "xcrossing.window"))

(define-accessor (expose-event->display (event <struct>)) => <raw-ptr>
  ("XEvent" "xexpose.display"))

(define-accessor (expose-event->window (event <struct>)) => <xid>
  ("XEvent" "xexpose.window"))

(define-accessor (expose-event->count (event <struct>)) => <raw-int>
  ("XEvent" "xexpose.count"))

(define-accessor (key-event->window (event <struct>)) => <xid>
  ("XEvent" "xkey.window"))


(define-accessor (button-event->window (event <struct>)) => <xid>
  ("XEvent" "xbutton.window"))

(define-accessor (button-event->x (event <struct>)) => <raw-int>
  ("XEvent" "xbutton.x"))

(define-accessor (button-event->y (event <struct>)) => <raw-int>
  ("XEvent" "xbutton.y"))

(define-accessor (button-event->modifiers (event <struct>)) => <raw-int>
  ("XEvent" "xbutton.state"))


(define-ffi (x-flush (display <raw-ptr>))
  "XFlush")

(define-ffi (x-sync (display <raw-ptr>))
  "XSync")

(define-ffi (draw-rectangle (display <raw-ptr>)
			    (drawable <xid>)
			    (gc <raw-ptr>)
			    (x <raw-int>)
			    (y <raw-int>)
			    (w <raw-int>)
			    (h <raw-int>))
    "XDrawRectangle")

(define-ffi (fill-rectangle (display <raw-ptr>)
			    (drawable <xid>)
			    (gc <raw-ptr>)
			    (x <raw-int>)
			    (y <raw-int>)
			    (w <raw-int>)
			    (h <raw-int>))
    "XFillRectangle")

#|
(define-ffi (draw-line (display <raw-ptr>)
			    (drawable <xid>)
			    (gc <raw-ptr>)
			    (x1 <raw-int>)
			    (y1 <raw-int>)
			    (x2 <raw-int>)
			    (y1 <raw-int>))
    "XDrawLine")
|#

(define-ffi (set-clip-rectangles (display <raw-ptr>)
				 (gc <raw-ptr>)
				 (x-origin <raw-int>)
				 (y-origin <raw-int>)
				 (rectangles <struct>)
				 (num-rects <raw-int>)
				 (ordering <raw-int>))
    "XSetClipRectangles")

;; note: XRectangle's are 16-bit rects (ie, X,Y,W,H ea. 16 bits)


;;;;;;  Scheme Tookit V.2

(define-ffi (lock-context! (context <obj>)) => <obj>
    "skit_lock_context")

(define-ffi (draw-box (box <struct>) (frame <struct>) (fill <raw-int>))
    "skit_draw_box")

(define-ffi (draw-string (str <raw-string>) (offset <raw-int>) (len <raw-int>)
	                 (x <raw-int>) (y <raw-int>))
    "skit_draw_string")

(define-ffi (draw-line (x1 <raw-int>)
		       (y1 <raw-int>)
		       (x2 <raw-int>)
		       (y1 <raw-int>))
    "skit_draw_line")

(define-ffi (draw-rect-frame (frame <struct>))
    "skit_frame_rect")

(define-ffi (draw-rounded-rect-frame (r <raw-int>) (frame <struct>))
    "skit_frame_round_rect")

(define-ffi (draw-filled-rect (frame <struct>))
    "skit_fill_rect")

(define-ffi (draw-filled-rounded-rect (r <raw-int>) (frame <struct>))
    "skit_fill_round_rect")

(define-ffi (set-drawing-color! (color <raw-int>))
    "skit_set_color")

(define-ffi (set-background-color! (color <raw-int>))
    "skit_set_bg_color")

(define-ffi (set-drawing-stipple! (stipple <xid>))
  "skit_set_stipple")

(define-ffi (set-drawing-tile! (tile <xid>))
  "skit_set_tile")

(define-ffi (set-clip-mask! (mask <xid>))
  "skit_set_clip_mask")

(define-ffi (query-pointer (pointer-query <struct>)
			   (display <raw-ptr>)
			   (win <xid>))
  "skit_query_pointer")

(define-struct-size <pointer-query> "SKIT_PointerQuery")

(define-accessor (pointer-query->window (query <struct>)) => <xid>
  ("SKIT_PointerQuery" "current_win"))

(define-accessor (pointer-query->win-x (query <struct>)) => <raw-int>
  ("SKIT_PointerQuery" "win_rel_x"))

(define-accessor (pointer-query->win-y (query <struct>)) => <raw-int>
  ("SKIT_PointerQuery" "win_rel_y"))


(define-ffi (default-depth (display <raw-ptr>)) => <raw-int>
  "skit_default_depth")


(define-ffi (copy-area (pixmap <xid>)
		       (from-rect <struct>)
		       (to-point <struct>))
  "skit_composite")

;; sets the coordinate system and the clipping area
;;   rect is in window (absolute) coords.

(define-ffi (set-drawing-area (rect <struct>)
			      (x <raw-int>)
			      (y <raw-int>))
  "skit_set_drawing_area")

;; similar to above, but clipping is INTERSECTION'ed
;; and coords are relative

;(define-ffi (set-sub-drawing-area (rect <struct>))
;  "skit_sub_drawing_area")

(define-ffi (intersect-rect (accum <struct>) (rect <struct>))
  "skit_intersect_rect")

(define-ffi (reset-color (display <raw-ptr>)
			 (color <raw-int>)
			 (name <raw-string>))
  "skit_reset_color")

;;

(define-ffi (keysym->string (keysym <raw-int>)) => <raw-string>
  "XKeysymToString")

(define-ffi (string->keysym (str <raw-string>)) => <raw-int>
  "XStringToKeysym")

(define-ffi (keycode->keysym (display <raw-ptr>)
		 	     (keycode <raw-int>)
			     (index <raw-int>)) => <raw-int>
  "XKeycodeToKeysym")


(define-ffi (keysym->keycode (display <raw-ptr>)
		 	     (keysym <raw-int>)) => <raw-int>
  "XKeysymToKeycode")

(define-ffi (key-event->keysym (event <struct>)
			       (index <raw-int>)) => <raw-int>
  "XLookupKeysym")
|#

