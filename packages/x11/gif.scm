
(define-class <gd-image> (<object>)
  (gd-image-raw-ptr allocation: internal))


(define-X-glue (read-gd-image/gif (port <std-input-port>))
  literals: ((& <gd-image>))
{
  FILE *f = (FILE *)OBJ_TO_RAW_PTR( gvec_read( port, SLOT(1) ) );
  gdImagePtr gd;
      
  gd = gdImageCreateFromGif( f );
  REG0 = make1( TLREF(0), RAW_PTR_TO_OBJ(gd) );
  RETURN1();
})

(define-X-glue (gd-image-size (image <gd-image>))
  literals: ((& <size>))
{
  REG0 = make2( TLREF(0),
	        int2fx( gdImageSX(image) ),
	        int2fx( gdImageSY(image) ) );
  RETURN1();
})

(define-X-glue (gd-image-get-pixel (image <gd-image>) 
				      (x <raw-int>) 
				      (y <raw-int>))
{
  REG0 = int2fx( gdImageGetPixel( image, x, y ) );
  RETURN1();
})


(define-X-glue (gd-image-get-colors (image <gd-image>))
{
  int i, n = gdImageColorsTotal(image);
  obj colors = NIL_OBJ;

  for (i=n; i>0;)
    {
      i--;
      colors = cons( make3( vector_class, 
			    int2fx( gdImageRed( image, i ) ),
			    int2fx( gdImageGreen( image, i ) ),
			    int2fx( gdImageBlue( image, i ) ) ),
		     colors );
    }
  REG0 = colors;
  REG1 = int2fx( gdImageGetTransparent(image) );
  RETURN(2);
})

(define-X-glue (gd-image-free (image <gd-image>))
{
   gdImageDestroy(image);
   gvec_write( raw_image, SLOT(0), ZERO );
   RETURN0();
})
