
(define-class <X-font> (<object>)
  (raw-font-ptr allocation: internal)
  x-display)

  
(define-X-glue (load-font (display <X-display>) (font <raw-string>))
  literals: ((& <X-font>))
{
  XFontStruct *f;

  f = XLoadQueryFont( display, font );
  if (f)
    REG0 = make2( TLREF(0),
		  RAW_PTR_TO_OBJ(f),
		  raw_display );
  else
    REG0 = FALSE_OBJ;
  RETURN1();
})

;;

(define-X-glue (set-font (ctx <X-drawing-context>)
			    (font <X-font>))
{
  XSetFont( ctx_dsp, ctx_gc, font->fid );
  RETURN0();
})

(define-X-glue (draw-string (ctx <X-drawing-context>)
			       (str <raw-string>)
			       (offset <raw-int>)
			       (len <raw-int>)
			       (point <point>))
{
   XDrawString( ctx_dsp, ctx_win, ctx_gc,
                POINT_X(point) + ctx_origin_x, 
		POINT_Y(point) + ctx_origin_y,
		str + offset, len );
   RETURN0();
})

(define-X-glue (text-width (font <X-font>)
			      (str <raw-string>)
			      (offset <raw-int>)
			      (len <raw-int>))
{
   REG0 = int2fx( XTextWidth( font, str + offset, len ) );
   RETURN1();
})

(define-X-glue (font-info (font <X-font>))
{
  REG0 = int2fx( font->min_char_or_byte2 );
  REG1 = int2fx( font->max_char_or_byte2 );
  REG2 = font->all_chars_exist ? TRUE_OBJ : FALSE_OBJ;
  REG3 = int2fx( font->default_char );
  RETURN(4);
})
