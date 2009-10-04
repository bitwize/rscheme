
(define-X-glue (alloc-read-only-color (display <X-display>)
				      (red <raw-int>)
				      (green <raw-int>)
				      (blue <raw-int>))
{
  XColor c;

  c.red = red;
  c.green = green;
  c.blue = blue;
  c.flags = DoRed | DoBlue | DoGreen;
  c.pixel = 0;

  if (XAllocColor( display, DISPLAY_COLORMAP(raw_display), &c ) == 0)
    {
      REG0 = FALSE_OBJ;
    }
  else
    {
      REG0 = int2fx( c.pixel );
    }
  RETURN1();
})

(define-X-glue (alloc-colors (display <X-display>)
				(num_colors <raw-int>))
{
  obj color_vec;
  color_t *colors;
  int i;

  color_vec = alloc( SLOT(num_colors), vector_class );
  colors = (color_t *)PTR_TO_DATAPTR(color_vec);

  if (XAllocColorCells( display, 
		        DISPLAY_COLORMAP(raw_display), 
		        0/*FALSE*/, NULL, 0, 
		        colors,
		        num_colors ) == 0) 
    {
      REG0 = FALSE_OBJ;
    }
  else 
    {
      for (i=0; i<num_colors; i++)
	gvec_write_fresh( color_vec, SLOT(i), int2fx(colors[i]) );
      REG0 = color_vec;
    }
  RETURN1();
})

(define-X-glue (assign-color (display <X-display>)
				(color <color>)
				(color_name <raw-string>))
{
  XColor c;
  if (XParseColor( display, DISPLAY_COLORMAP(raw_display), color_name, &c ))
    {
      c.pixel = color;
      c.flags = DoRed | DoGreen | DoBlue;
      XStoreColor( display, DISPLAY_COLORMAP(raw_display), &c );
      REG0 = TRUE_OBJ;
    }
  else
    REG0 = FALSE_OBJ;
  RETURN1();
})

(define-X-glue (white-pixel (display <X-display>))
{
  color_t c;
  c = WhitePixel( display, DISPLAY_SCREEN(raw_display) );
  REG0 = int2fx( c );
  RETURN1();
})

(define-X-glue (black-pixel (display <X-display>))
{
  REG0 = int2fx( BlackPixel( display, DISPLAY_SCREEN(raw_display) ) );
  RETURN1();
})
