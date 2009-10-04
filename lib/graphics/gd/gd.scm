(define-class <gd-image-ptr> (<object>) :bvec)

(define-class <gd-io-ctx> (<object>) :abstract
  raw-gd-io-ctx
  underlying-port)
  
(define-macro (define-gd-glue args . body)
  `(define-safe-glue ,args
     properties: ((other-h-files "<gd.h>" "config.h" "gdio.h")
                  (other-c-files "gdio.c")
		  (other-libs "gd" "png" "z" "jpeg" "freetype"))
     type-handler: (<gd-image-ptr> 
                    (direct-instance? <gd-image-ptr>)
                    ("gdImagePtr ~a" "(*(gdImagePtr *)PTR_TO_DATAPTR(~a) )"))
     type-handler: (<gd-io-ctx>
                    (instance? <gd-io-ctx>)
                    ("gdIOCtx *~a"
                     "((gdIOCtx *)PTR_TO_DATAPTR(gvec_ref(~a,SLOT(0))))"))
     ,@body))

(define-gd-glue (gd-image-create (sx <raw-int>) (sy <raw-int>))
  literals: ((& <gd-image-ptr>))
{
  gdImagePtr p;

  p = gdImageCreate( sx, sy );
  if (p) {
    obj m = bvec_alloc( sizeof( gdImagePtr ), TLREFB(0) );
    (*(gdImagePtr *)PTR_TO_DATAPTR(m) ) = p;
    REG0 = m;
  } else {
    REG0 = FALSE_OBJ;
  }
  RETURN1();
})

(define-gd-glue (gd-image-png* (img <gd-image-ptr>) (file <fixnum>))
{
  gdImagePng( img, FX_AS_FILE( file ) );
  RETURN0();
})

(define-gd-glue (gd-image-png-ctx (img <gd-image-ptr>) (ctx <gd-io-ctx>))
{
  gdImagePngCtx( img, ctx );
  RETURN0();
})


(define-gd-glue (gd-image-create-from-png* (file <fixnum>))
  literals: ((& <gd-image-ptr>))
{
  FILE *f = FX_AS_FILE(file);
  gdImagePtr p;

  p = gdImageCreateFromPng( f );
  if (p) {
    obj m = bvec_alloc( sizeof( gdImagePtr ), TLREFB(0) );
    (*(gdImagePtr *)PTR_TO_DATAPTR(m) ) = p;
    REG0 = m;
  } else {
    REG0 = FALSE_OBJ;
  }
  RETURN1();
})

(define-gd-glue (gd-image-destroy (img <gd-image-ptr>))
{
  gdImageDestroy( img );
  RETURN0();
})

(define-gd-glue (gd-image-set-pixel (img <gd-image-ptr>)
                                    (x <raw-int>)
                                    (y <raw-int>)
                                    (color <raw-int>))
{
  gdImageSetPixel( img, x, y, color );
  RETURN0();
})

(define-class <gd-font-error> (<condition>)
  message)

(define-gd-glue (gd-image-string-ft-size (fontlist <raw-string>)
                                         (ptsize <raw-float>)
                                         (angle <raw-float>)
                                         (x <raw-int>)
                                         (y <raw-int>)
                                         (string <raw-string>))
  literals: ((& <gd-font-error>))
{
  char *ret;
  int brect[8];

  ret = gdImageStringFT( NULL, &brect[0], 0,
                         fontlist, ptsize, angle,
                         x, y, string );
  if (ret) {
    raise_error( make2( TLREF(0), NIL_OBJ, make_string( ret ) ) );
  }
  REG0 = int2fx( brect[0] );  /* llx */
  REG1 = int2fx( brect[1] );  /* lly */
  REG2 = int2fx( brect[2] );  /* lrx */
  REG3 = int2fx( brect[3] );  /* lry */
  REG4 = int2fx( brect[4] );  /* urx */
  REG5 = int2fx( brect[5] );  /* ury */
  REG6 = int2fx( brect[6] );  /* ulx */
  REG7 = int2fx( brect[7] );  /* uly */
  RETURN(8);
})

(define-gd-glue (gd-image-string-ft (img <gd-image-ptr>)
                                    (color <raw-int>)
                                    (fontlist <raw-string>)
                                    (ptsize <raw-float>)
                                    (angle <raw-float>)
                                    (x <raw-int>)
                                    (y <raw-int>)
                                    (string <raw-string>))
  literals: ((& <gd-font-error>))
{
  char *ret;
  int brect[8];

  ret = gdImageStringFT( img, &brect[0], color,
                         fontlist, ptsize, angle,
                         x, y, string );
  if (ret) {
    raise_error( make2( TLREF(0), NIL_OBJ, make_string( ret ) ) );
  }
  RETURN0();
})

(define-gd-glue (gd-image-get-pixel (img <gd-image-ptr>)
                                    (x <raw-int>)
                                    (y <raw-int>))
{
  REG0 = int2fx( gdImageGetPixel( img, x, y ) );
  RETURN1();
})

(define-gd-glue (gd-image-color-resolve (img <gd-image-ptr>)
                                        (r <raw-int>)
                                        (g <raw-int>)
                                        (b <raw-int>))
{
  REG0 = int2fx( gdImageColorResolve( img, r, g, b ) );
  RETURN1();
})

(define-gd-glue (gd-image-color-get-rgba (img <gd-image-ptr>)
                                         (color <raw-int>))
{
#define TRUE_COLOR_SUPPORT (0)
#define ALPHA_CHANNEL_SUPPORT (0)

#if TRUE_COLOR_SUPPORT
  if (img->truecolor) {
    REG0 = int2fx( gdTrueColorGetRed( color ) );
    REG1 = int2fx( gdTrueColorGetGreen( color ) );
    REG2 = int2fx( gdTrueColorGetBlue( color ) );
    REG3 = int2fx( gdTrueColorGetAlpha( color ) );
    RETURN(4);
  } else 
#endif
  if ((color > 0) && (color < img->colorsTotal) 
      && !img->open[color]) {
    REG0 = int2fx( img->red[color] );
    REG1 = int2fx( img->green[color] );
    REG2 = int2fx( img->blue[color] );
    if (color == img->transparent) {
      REG3 = int2fx( 127 );             /* transparent */
    } else {
#if ALPHA_CHANNEL_SUPPORT
      REG3 = int2fx( img->alpha[color] );
#else
      REG3 = int2fx( 0 );               /* opaque */
#endif
    }
    RETURN(4);
  } else {
    RETURN0();
  }
})

(define (gd-image-png (self <gd-image-ptr>) file)
  (let* ((f (fopen file "wb")))
    (gd-image-png* self f)
    (fclose f)
    (values)))

(define (gd-image-create-from-png file)
  (let* ((f (or (fopen file "rb")
                (error "gd-image-create-from-png: Could not open ~s" file)))
         (img (gd-image-create-from-png* f)))
    (fclose f)
    img))

(define-gd-glue (gd-image-color-allocate (img <gd-image-ptr>)
                                         (r <raw-int>)
                                         (g <raw-int>)
                                         (b <raw-int>))
{
  REG0 = int2fx( gdImageColorAllocate( img, r, g, b ) );
  RETURN1();
})

(define-macro (define-gd-graphics-primop (name c-name . args))
  `(define-gd-glue (,name ,@args)
     ,(make <curly-braced>
            text: (call-with-output-string
                   (lambda (p)
                     (format p "\n")
                     (format p "  ~a( ~a );\n"
                             c-name
                             (string-join ", " (map (lambda (a)
                                                      (symbol->string (car a)))
                                                    args)))
                     (format p "  RETURN0();\n"))))))

(define-gd-glue (gd-image-size (img <gd-image-ptr>))
{
  REG0 = int2fx( gdImageSX(img) );
  REG1 = int2fx( gdImageSX(img) );
  RETURN(2);
})

(define (gd-image-sx img) (bind ((sx sy (gd-image-size img))) sx))
(define (gd-image-sy img) (bind ((sx sy (gd-image-size img))) sy))

(define-gd-graphics-primop (gd-image-line "gdImageLine"
                                          (img <gd-image-ptr>)
                                          (x1 <raw-int>)
                                          (y1 <raw-int>)
                                          (x2 <raw-int>)
                                          (y2 <raw-int>)
                                          (color <raw-int>)))

(define-gd-graphics-primop (gd-image-rectangle "gdImageRectangle"
                                               (img <gd-image-ptr>)
                                               (x1 <raw-int>)
                                               (y1 <raw-int>)
                                               (x2 <raw-int>)
                                               (y2 <raw-int>)
                                               (color <raw-int>)))

(define-gd-graphics-primop (gd-image-filled-rectangle "gdImageFilledRectangle"
                                                      (img <gd-image-ptr>)
                                                      (x1 <raw-int>)
                                                      (y1 <raw-int>)
                                                      (x2 <raw-int>)
                                                      (y2 <raw-int>)
                                                      (color <raw-int>)))

#|
These are GD 2.x...

(define-gd-graphics-primop (gd-image-set-thickness "gdImageSetThickness"
                                                   (img <gd-image-ptr>)
                                                   (thickness <raw-int>)))

(define-gd-graphics-primop (gd-image-set-clip "gdImageSetClip"
                                              (img <gd-image-ptr>)
                                              (x1 <raw-int>)
                                              (y1 <raw-int>)
                                              (x2 <raw-int>)
                                              (y2 <raw-int>)))
(define-gd-graphics-primop (gd-image-filled-ellipse "gdImageFilledEllipse"
                                                    (img <gd-image-ptr>)
                                                    (cx <raw-int>)
                                                    (cy <raw-int>)
                                                    (w <raw-int>)
                                                    (h <raw-int>)
                                                    (color <raw-int>)))

(define-gd-graphics-primop (gd-image-filled-arc "gdImageFilledArc"
                                                (img <gd-image-ptr>)
                                                (cx <raw-int>)
                                                (cy <raw-int>)
                                                (w <raw-int>)
                                                (h <raw-int>)
                                                (s <raw-int>)
                                                (e <raw-int>)
                                                (color <raw-int>)
                                                (style <raw-int>)))


|#

(define-gd-graphics-primop (gd-image-copy "gdImageCopy"
                                          (dst <gd-image-ptr>)
                                          (src <gd-image-ptr>)
                                          (dstX <raw-int>)
                                          (dstY <raw-int>)
                                          (srcX <raw-int>)
                                          (srcY <raw-int>)
                                          (w <raw-int>)
                                          (h <raw-int>)))

(define-gd-graphics-primop (gd-image-arc "gdImageArc"
                                         (img <gd-image-ptr>)
                                         (cx <raw-int>)
                                         (cy <raw-int>)
                                         (w <raw-int>)
                                         (h <raw-int>)
                                         (s <raw-int>)
                                         (e <raw-int>)
                                         (color <raw-int>)))

;;;


(define-gd-glue (string-output-port->gd-io-ctx (port <string-output-port>))
{
  REG0 = rs_gd_strout( port );
  RETURN1();
})

(define-class <gd-strout-io-ctx> (<gd-io-ctx>))

(define-method open-gd-io-ctx ((self <string-output-port>))
  (make <gd-strout-io-ctx>
        raw-gd-io-ctx: (string-output-port->gd-io-ctx self)
        underlying-port: self))
