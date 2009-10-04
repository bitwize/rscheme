
(define-class <X-image> (<object>)
  x-image-ptr
  image-size
  image-data)

(define-X-glue (make-image (dsp <X-display>) 
			      (size <size>) 
			      (depth <raw-int>))
  literals: ((& <X-image>))
{			      
  int screen = DefaultScreen( dsp );
  XImage *img;
  obj data;
  unsigned bytes_per_line;

  if (depth != 1 && depth != 8 && depth != 16 && depth != 32 && depth != 24)
    {
      scheme_error( "make-image: depth of ~s is invalid", 1, raw_depth );
    }

  bytes_per_line = SIZE_W(size) * depth / 8;
  bytes_per_line = (((bytes_per_line - 1)|3)+1);

  data = bvec_alloc( bytes_per_line * SIZE_H(size), byte_vector_class );

  img = XCreateImage( dsp,
		      DefaultVisual( dsp, screen ),
		      DefaultDepth( dsp, screen ),
		      ZPixmap,
		      0, (char *)PTR_TO_DATAPTR(data),
		      SIZE_W(size), SIZE_H(size),
		      32, bytes_per_line );
  REG0 = make3( TLREF(0), RAW_PTR_TO_OBJ(img), raw_size, data );
  RETURN1();
})

(define-X-glue (image-set-pixel (image <X-image>)
				   (x <raw-int>)
				   (y <raw-int>)
				   (pix <color>))
{
   put_pixel( image, x, y, pix );
   RETURN0();
})

;; restricted to source 1-byte/pixel

(define-X-glue (image-fill-pixels (image <X-image>)
				     (source <raw-string>)
				     (fill <rect>)
				     (colors <vector>))
{
  int x, y, ix, iy, w = RECT_W(fill), h = RECT_H(fill);
  unsigned char code, *s;
  int (*f)( XImage *image, int x, int y, color_t p );

  f = image->f.put_pixel;

  s = source;

  for (iy=0, y=RECT_Y(fill); iy<h; iy++, y++)
    {
      for (ix=0, x = RECT_X(fill); ix<w; ix++, x++)
	{
	  code = *s++;
	  f( image, x, y, fx2int( gvec_read( colors, SLOT(code) ) ) );
	}
    }
  RETURN0();
})

;;  code <00> in the source string denotes a transparent pixel 

(define-X-glue (image-fill-mask (image <X-image>)
				   (source <raw-string>)
				   (fill <rect>))
{
  int x, y, ix, iy, w = RECT_W(fill), h = RECT_H(fill);
  unsigned char code, *s;
  int (*f)( XImage *image, int x, int y, color_t p );

  f = image->f.put_pixel;

  s = source;

  for (iy=0, y=RECT_Y(fill); iy<h; iy++, y++)
    {
      for (ix=0, x = RECT_X(fill); ix<w; ix++, x++)
	{
	  code = (*s++) ? 1 : 0;
	  f( image, x, y, code );
	}
    }
  RETURN0();
})

(define-X-glue (image-get-pixel (image <X-image>)
				   (x <raw-int>)
				   (y <raw-int>))
{
   REG0 = int2fx( XGetPixel( image, x, y ) );
   RETURN1();
})

(define-X-glue (draw-image (ctx <X-drawing-context>) 
			      (image <X-image>)
			      (from <rect>)
			      (to <point>))
{
  XPutImage( ctx_dsp, ctx_win, ctx_gc,
	     image,
	     RECT_X(from), RECT_Y(from), 
	     POINT_X(to) + ctx_origin_x,
	     POINT_Y(to) + ctx_origin_y,
	     RECT_W(from), RECT_H(from) );
  RETURN0();
})
