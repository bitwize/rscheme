#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "adapt.h"

void put_pixel( XImage *i, int x, int y, unsigned long p )
{
   XPutPixel( i, x, y, p );
}

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
