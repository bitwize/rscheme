#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "adapt.h"
#include <rscheme/smemory.h>
#include <rscheme/scheme.h>

struct line_layout {
  int  base_x, base_y;
  UINT_32  line_start, line_end;
  int  sel_start_x, sel_end_x;
};

void render_line_sel( struct text_rendition *info, struct line_layout *ll )
{
  int base_x = ll->base_x;
  int base_y = ll->base_y;
  UINT_32 line_start = ll->line_start;
  UINT_32 line_end = ll->line_end;
  int sel_start_x = ll->sel_start_x;
  int sel_end_x = ll->sel_end_x;

  if (info->sel_from <= line_end && info->sel_to >= line_start)
    {
      int x, y, w, h;

      if (info->sel_from >= line_start)
	x = sel_start_x;
      else
	x = base_x;

      if (info->sel_to <= line_end)
	w = sel_end_x - x;
      else
	w = (base_x + info->line_width) - x;
      x += info->origin_x;
      h = info->line_height;
      y = base_y - h;

      XFillRectangle( info->ctx_dsp, info->ctx_win, info->ctx_gc,
		      x + RECT_X(info->sel_bleed),
		      y + RECT_Y(info->sel_bleed),
		      w + RECT_W(info->sel_bleed),
		      h + RECT_H(info->sel_bleed) );
    }
}


void render_line( struct text_rendition *info, struct line_layout *ll )
{
  int base_x = ll->base_x;
  int base_y = ll->base_y;
  UINT_32 line_start = ll->line_start;
  UINT_32 line_end = ll->line_end;
  int sel_start_x = ll->sel_start_x;
  int sel_end_x = ll->sel_end_x;

  XDrawString( info->ctx_dsp, info->ctx_win, info->ctx_gc,
	       base_x, base_y,
	       ((unsigned char *)PTR_TO_DATAPTR(info->text)) + line_start,
	       line_end - line_start );
}

int layout_text( struct text_rendition *info )
{
UINT_32 i, n, cur_w, last_line_start, last_line_end;
int base_x, base_y, sel_start_x, sel_end_x;
unsigned char *text_str;
unsigned *widths, w_temp[300];
struct line_layout *lines, *cur_ll, *ll_limit, ll_temp[30];

int line_height = info->line_height;
int line_width = info->line_width;
UINT_32 sel_from = info->sel_from;
UINT_32 sel_to = info->sel_to;

    n = string_length(info->text);
    if (n < 300)
      {
	widths = w_temp;
      }
    else
      {
	widths = (unsigned *)malloc( sizeof(unsigned) * n );
      }

    cur_ll = lines = ll_temp;
    ll_limit = lines + 30;

    text_str = (unsigned char *)string_text(info->text);
    
    PerCharWidths( info->font, text_str, widths, n );

    base_x = info->origin_x;
    base_y = info->origin_y;

    i = 0;
    cur_w = 0;
    last_line_start = last_line_end = 0;
    
    sel_start_x = sel_end_x = -1;

    while (i < n)
    {
    unsigned char ch = text_str[i];
    
        if (i == sel_from)
	  {
	    cur_ll->sel_start_x = cur_w;
	  }
        if (i == sel_to)
	  {
	    cur_ll->sel_end_x = cur_w;
	  }
	if (ch == '\n')
	{
	  cur_ll->base_x = base_x;
	  cur_ll->base_y = base_y;
	  cur_ll->line_start = last_line_start;
	  cur_ll->line_end = i;
	  cur_ll++;
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
	      cur_ll->base_x = base_x;
	      cur_ll->base_y = base_y;
	      cur_ll->line_start = last_line_start;
	      cur_ll->line_end = i;
	      cur_ll++;
		base_y += line_height;
		last_line_start = i;
		cur_w = 0;
	    }
	    else
	    {
		/* break on the last good place to break */
	      cur_ll->base_x = base_x;
	      cur_ll->base_y = base_y;
	      cur_ll->line_start = last_line_start;
	      cur_ll->line_end = last_line_end;
	      cur_ll++;
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
      cur_ll->base_x = base_x;
      cur_ll->base_y = base_y;
      cur_ll->line_start = last_line_start;
      cur_ll->line_end = n;
      cur_ll++;
      base_y += line_height;
    }

    /* render the lines */

    ll_limit = cur_ll;

#if 0
    for (n=0, cur_ll=lines; cur_ll<ll_limit; cur_ll++,n++)
      {
	printf( "line[%u]: base=(%d,%d) line=(%u,%u) sel=(%d,%d)\n",
	        n, cur_ll->base_x, cur_ll->base_y,
	        cur_ll->line_start, cur_ll->line_end,
	        cur_ll->sel_start_x, cur_ll->sel_end_x );
      }
#endif
    if (truish(info->sel_bleed))
      {
	XSetForeground( info->ctx_dsp, info->ctx_gc, info->selection_color );
	
	for (cur_ll=lines; cur_ll<ll_limit; cur_ll++)
	  render_line_sel( info, cur_ll );
      }

    XSetForeground( info->ctx_dsp, info->ctx_gc, info->text_color );

    for (cur_ll=lines; cur_ll<ll_limit; cur_ll++)
      render_line( info, cur_ll );

    if (widths != w_temp)
	free(widths);
    if (lines != ll_temp)
        free(lines);
    return base_y;
}
