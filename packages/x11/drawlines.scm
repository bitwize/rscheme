
;; selection-bleed and margins are encoded as <rect>'s which
;; report the size of the area in question with respect to a
;; zero-sized base region at (0,0)

(define-class <text-layout> (<object>)
  font
  selection-bleed
  line-height
  margins
  text-color
  selection-color)

(define-X-glue (draw-lines/selection (ctx <X-drawing-context>)
					(frame <rect>)
					(layout <text-layout>)
					(text <string>)
					(sel_from <raw-int>)
					(sel_to <raw-int>))
{
  struct text_rendition info;
  obj raw_font, margins;

  info.ctx_dsp = ctx_dsp;
  info.ctx_win = ctx_win;
  info.ctx_gc = ctx_gc;

  raw_font = gvec_read( layout, SLOT(0) );
  margins = gvec_read( layout, SLOT(3) );

  info.font = (XFontStruct *)OBJ_TO_RAW_PTR( gvec_read( raw_font, SLOT(0) ) );

  info.line_height = fx2int( gvec_read( layout, SLOT(2) ) );
  info.line_width = RECT_W(frame) - RECT_W(margins);
  info.text = text;
  info.text_color = FX_TO_COLOR( gvec_read( layout, SLOT(4) ) );
  info.selection_color = FX_TO_COLOR( gvec_read( layout, SLOT(5) ) );
  info.sel_from = sel_from;
  info.sel_to = sel_to;
  info.sel_bleed = gvec_read( layout, SLOT(1) );

  info.origin_x = RECT_X(frame) + ctx_origin_x;
  info.origin_y = RECT_Y(frame) + ctx_origin_y + info.line_height;

  REG0 = int2fx( layout_text( &info ) );
  RETURN1();
})

(define-X-glue (draw-lines/no-selection (ctx <X-drawing-context>)
					(frame <rect>)
					(layout <text-layout>)
					(text <string>))
{
  struct text_rendition info;
  obj raw_font, margins;

  info.ctx_dsp = ctx_dsp;
  info.ctx_win = ctx_win;
  info.ctx_gc = ctx_gc;

  raw_font = gvec_read( layout, SLOT(0) );
  margins = gvec_read( layout, SLOT(3) );

  info.font = (XFontStruct *)OBJ_TO_RAW_PTR( gvec_read( raw_font, SLOT(0) ) );

  info.line_height = fx2int( gvec_read( layout, SLOT(2) ) );
  info.line_width = RECT_W(frame) - RECT_W(margins);
  info.text = text;
  info.text_color = FX_TO_COLOR( gvec_read( layout, SLOT(4) ) );
  info.selection_color = FX_TO_COLOR( gvec_read( layout, SLOT(5) ) );
  info.sel_from = 0;
  info.sel_to = 0;
  info.sel_bleed = FALSE_OBJ;

  info.origin_x = RECT_X(frame) + ctx_origin_x;
  info.origin_y = RECT_Y(frame) + ctx_origin_y + info.line_height;

  REG0 = int2fx( layout_text( &info ) );
  RETURN1();
})

#|

(define-X-glue (draw-lines/no-selection (ctx <X-drawing-context>)
					   (font <X-font>)
					   (origin <point>)
					   (line_height <raw-int>)
					   (line_width <raw-int>)
					   (text <string>))
{
UINT_32 i, n, cur_w, last_line_start, last_line_end;
int base_x, base_y;
unsigned char *text_str;
unsigned *widths, w_temp[300];

    n = string_length(text);
    if (n < 300)
	widths = w_temp;
    else
	widths = (unsigned *)malloc( sizeof(unsigned) * n );

    text_str = (unsigned char *)string_text(text);
    
    PerCharWidths( font, text_str, widths, n );

    base_x = POINT_X(origin) + ctx_origin_x;
    base_y = POINT_Y(origin) + ctx_origin_y;
    
    i = 0;
    cur_w = 0;
    last_line_start = last_line_end = 0;
    
    while (i < n)
    {
    unsigned char ch = text_str[i];
    
	if (ch == '\n')
	{
	    XDrawString( ctx_dsp, ctx_win, ctx_gc, 
			 base_x, base_y, 
			 text_str + last_line_start, 
			 i - last_line_start );
	    base_y += line_height;
	    i++;
	    last_line_start = i;
	    cur_w = 0;
	}
	else if (ch == '\t' || ch == ' ')
	{
	    cur_w += widths[i];
	    i++;
	    last_line_end = i;
	}
	else
	{
	    cur_w += widths[i];
	    if (cur_w <= line_width)
	    {
		i++;
	    }
	    else if (last_line_start == last_line_end)
	    {
		/* break on characters 
		   -- there was no good place to break... */
		XDrawString( ctx_dsp, ctx_win, ctx_gc, 
			     base_x, base_y, 
			     text_str + last_line_start, 
			     i-last_line_start );
		base_y += line_height;
		last_line_start = i;
		cur_w = 0;
	    }
	    else
	    {
		/* break on the last good place to break */
		XDrawString( ctx_dsp, ctx_win, ctx_gc, 
			     base_x, base_y, 
			     text_str + last_line_start, 
			     last_line_end - last_line_start );
		base_y += line_height;
		last_line_start = last_line_end;
		i = last_line_end;
		cur_w = 0;
	    }
	}
    }
    if (n != last_line_start)
    {
	/* flush the current line */
	XDrawString( ctx_dsp, ctx_win, ctx_gc, 
			base_x, base_y,
			text_str + last_line_start,
			n - last_line_start );
	base_y += line_height;
    }
    REG0 = int2fx( base_y );
    if (widths != w_temp)
	free(widths);
    RETURN1();
})
|#
#|
void PerCharWidths( XFontStruct *font, 
		    unsigned char *text_str, 
		    unsigned *widths, 
		    UINT_32 n )
{
unsigned char ch;
unsigned i, default_w, w, min_ch, max_ch;

    min_ch = font->min_char_or_byte2;
    max_ch = font->max_char_or_byte2;
    
    ch = font->default_char;
    if (ch >= min_ch && ch <= max_ch)
	default_w = font->per_char[ ch - min_ch ].width;
    else
	default_w = 0;
    
    for (i=0; i<n; i++)
    {
	ch = *text_str++;
	if (ch >= min_ch && ch <= max_ch)
	    w = font->per_char[ ch - min_ch ].width;
	else
	    w = default_w;
	*widths++ = w;
    }
}
|#

(define-X-glue (map-width (font <X-font>) (str <string>))
{
unsigned *widths, w_temp[300];
unsigned char *text_str;
unsigned i, n;
obj wl;

    text_str = string_text(str);
    n = string_length(str);
    
    if (n <= 300)
	widths = w_temp;
    else
	widths = (unsigned *)malloc( sizeof(unsigned) * n );

    PerCharWidths( font, text_str, widths, n );

    wl = NIL_OBJ;
    for (i=n; i>0;)
      {
        wl = cons( int2fx( widths[--i] ), wl );
      }
    REG0 = wl;
    if (widths != w_temp)
	free(widths);
    RETURN1();
})

(define-X-glue (locate-text-hit (ctx_origin <point>)
				    (font <X-font>)
				    (origin <point>)
				    (line_height <raw-int>)
				    (line_width <raw-int>)
				    (text <string>)
				    (hit_point <point>))
{
UINT_32 i, n, last_line_start, last_line_end;
int cur_w, this_line, base_x, base_y, hit_x, hit_y;
unsigned char *text_str;
unsigned *widths, w_temp[300];

    n = string_length(text);
    if (n < 300)
	widths = w_temp;
    else
	widths = (unsigned *)malloc( sizeof(unsigned) * n );

    text_str = (unsigned char *)string_text(text);
    
    PerCharWidths( font, text_str, widths, n );

    base_x = POINT_X(origin) + POINT_X(ctx_origin);
    base_y = POINT_Y(origin) + POINT_Y(ctx_origin);
    
    hit_x = POINT_X(hit_point) + POINT_X(origin);
    hit_y = POINT_Y(hit_point) + POINT_Y(ctx_origin);
    
    i = 0;
    cur_w = 0;
    last_line_start = last_line_end = 0;
    
    this_line = (hit_y < base_y);
    
    printf( "base=(%d,%d)  hit=(%d,%d)\n", base_x, base_y, hit_x, hit_y );
    while (i < n)
    {
    unsigned char ch = text_str[i];
    
	printf( "[%u]='%c' (%sthis line) cur_w=%d, hit_x=%d\n",
	    i, ch, this_line ? "on " : "not ", cur_w, hit_x );
	if (ch == '\n')
	{
	    /* if the hit was on this line, then it was a hit
	       here if nowhere else */
	       
	    if (this_line)
	    {
		goto found_hit;
	    }
	    base_y += line_height;
	    this_line = (hit_y < base_y);
	    i++;
	    last_line_start = i;
	    cur_w = 0;
	}
	else if (ch == '\t' || ch == ' ')
	{
	  int W = widths[i];
	  
	  if (this_line)
	    {		
	      if (hit_x < W/2)
		goto found_hit;
	      hit_x -= W;
	    }
	  cur_w += widths[i];
	  i++;
	  last_line_end = i;
	}
	else
	{
	int W = widths[i];
	
	    /* hit_x has been adjusted for the origin_x */

	    if (this_line)
	    {		
		if (hit_x < W/2)
		    goto found_hit;
		hit_x -= W;
	    }

	    cur_w += W;
	    if (cur_w <= line_width)
	    {
		i++;
	    }
	    else if (last_line_start == last_line_end)
	    {
		/* break on characters 
		   -- there was no good place to break... */
		if (this_line)
		    goto found_hit;
		base_y += line_height;
		this_line = (hit_y < base_y);
		last_line_start = i;
		cur_w = 0;
	    }
	    else
	    {
		/* break on the last good place to break */
		if (this_line)
		    goto found_hit;
		base_y += line_height;
		this_line = (hit_y < base_y);
		last_line_start = last_line_end;
		i = last_line_end;
		cur_w = 0;
	    }
	}
    }
found_hit:
    REG0 = int2fx(i);
    if (widths != w_temp)
	free(widths);
    RETURN1();
})
