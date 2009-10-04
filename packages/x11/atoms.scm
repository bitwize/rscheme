#|
   This is part of the toolkit because it's damn annoying
   to make is multiple-display compatible
|#

(define *all-atoms* '#uninit)
(%early-once-only
 (set! *all-atoms* (make-table eq? identity)))

(define-class <X-atom> (<object>)
  atom-value
  atom-text)

(define-method write-object ((self <X-atom>) port)
  (format port "#[<X-atom> ~a]" (atom-text self)))

(define-method initialize ((self <X-atom>))
  (table-insert! *all-atoms* (atom-value self) self)
  self)

(define-rewriter (define-atom form)
  `(begin
     (define 
       ,(cadr form)
       '#uninit)
     (%early-once-only
      (set! ,(cadr form)
	    (make <X-atom> 
		  atom-value: ,(caddr form)
		  atom-text: ,(substring (symbol->string (cadr form)) 3))))))

(define-atom XA_PRIMARY 1)
(define-atom XA_SECONDARY 2)
(define-atom XA_ARC 3)
(define-atom XA_ATOM 4)
(define-atom XA_BITMAP 5)
(define-atom XA_CARDINAL 6)
(define-atom XA_COLORMAP 7)
(define-atom XA_CURSOR 8)
(define-atom XA_CUT_BUFFER0 9)
(define-atom XA_CUT_BUFFER1 10)
(define-atom XA_CUT_BUFFER2 11)
(define-atom XA_CUT_BUFFER3 12)
(define-atom XA_CUT_BUFFER4 13)
(define-atom XA_CUT_BUFFER5 14)
(define-atom XA_CUT_BUFFER6 15)
(define-atom XA_CUT_BUFFER7 16)
(define-atom XA_DRAWABLE 17)
(define-atom XA_FONT 18)
(define-atom XA_INTEGER 19)
(define-atom XA_PIXMAP 20)
(define-atom XA_POINT 21)
(define-atom XA_RECTANGLE 22)
(define-atom XA_RESOURCE_MANAGER 23)
(define-atom XA_RGB_COLOR_MAP 24)
(define-atom XA_RGB_BEST_MAP 25)
(define-atom XA_RGB_BLUE_MAP 26)
(define-atom XA_RGB_DEFAULT_MAP 27)
(define-atom XA_RGB_GRAY_MAP 28)
(define-atom XA_RGB_GREEN_MAP 29)
(define-atom XA_RGB_RED_MAP 30)
(define-atom XA_STRING 31)
(define-atom XA_VISUALID 32)
(define-atom XA_WINDOW 33)
(define-atom XA_WM_COMMAND 34)
(define-atom XA_WM_HINTS 35)
(define-atom XA_WM_CLIENT_MACHINE 36)
(define-atom XA_WM_ICON_NAME 37)
(define-atom XA_WM_ICON_SIZE 38)
(define-atom XA_WM_NAME 39)
(define-atom XA_WM_NORMAL_HINTS 40)
(define-atom XA_WM_SIZE_HINTS 41)
(define-atom XA_WM_ZOOM_HINTS 42)
(define-atom XA_MIN_SPACE 43)
(define-atom XA_NORM_SPACE 44)
(define-atom XA_MAX_SPACE 45)
(define-atom XA_END_SPACE 46)
(define-atom XA_SUPERSCRIPT_X 47)
(define-atom XA_SUPERSCRIPT_Y 48)
(define-atom XA_SUBSCRIPT_X 49)
(define-atom XA_SUBSCRIPT_Y 50)
(define-atom XA_UNDERLINE_POSITION 51)
(define-atom XA_UNDERLINE_THICKNESS 52)
(define-atom XA_STRIKEOUT_ASCENT 53)
(define-atom XA_STRIKEOUT_DESCENT 54)
(define-atom XA_ITALIC_ANGLE 55)
(define-atom XA_X_HEIGHT 56)
(define-atom XA_QUAD_WIDTH 57)
(define-atom XA_WEIGHT 58)
(define-atom XA_POINT_SIZE 59)
(define-atom XA_RESOLUTION 60)
(define-atom XA_COPYRIGHT 61)
(define-atom XA_NOTICE 62)
(define-atom XA_FONT_NAME 63)
(define-atom XA_FAMILY_NAME 64)
(define-atom XA_FULL_NAME 65)
(define-atom XA_CAP_HEIGHT 66)
(define-atom XA_WM_CLASS 67)
(define-atom XA_WM_TRANSIENT_FOR 68)

(define (string->atom str)
 (intern-atom *X-display* str #t))


(define-X-glue (window-properties (win <X-window>))
 literals: ((& *all-atoms*) (& load-atoms))
{
  Atom *a;
  int i, n;
  obj atom, lst, tbl = TLREF(0);

  a = XListProperties( win_dsp, win_xid, &n );
  if (a)
    {
      lst = NIL_OBJ;
      for (i=n; i>0; )
	{
	  obj key = int2fx(a[--i]);
	  atom = objecttable_lookup( tbl, key, key );
	  if (EQ(atom,FALSE_OBJ))
	    atom = key;
	  lst = cons( atom, lst );
	}
      XFree(a);
      REG0 = lst;
      APPLY(1,TLREF(1));
    }
  else
    {
      REG0 = FALSE_OBJ;
      RETURN1();
    }
})

(define (load-atoms lst)
  (map (lambda (x)
	 (if (fixnum? x)
	     (lookup-atom *X-display* x)
	     x))
       lst))

(define-X-glue (lookup-atom (display <X-display>)
			       (atom <raw-int>))
  literals: ((& <X-atom>)
	     (& initialize));
{
  char *n;

  n = XGetAtomName( display, atom );
  if (!n)
    {
      REG0 = FALSE_OBJ;
      RETURN1();
    }
  else
    {
      REG0 = make2( TLREF(0), raw_atom, make_string(n) );
      XFree(n);
      APPLY(1,TLREF(1));
    }
})

(define-X-glue (intern-atom (display <X-display>)
			       (name <raw-string>)
			       createq)
  literals: ((& <X-atom>)
	     (& initialize))
{
  Atom a;

  a = XInternAtom( display, name, truish(createq) );

  if (a == None) {
    REG0 = FALSE_OBJ;
    RETURN1();
  } else {
    REG0 = make2( TLREF(0),
		  int2fx( a ),
		  raw_name );
    APPLY(1,TLREF(1));
  }
})

(define-X-glue (change-property (win <X-window>)
				   (property <X-atom>)
				   (type <X-atom>)
				   (format <raw-int>)
				   edit_mode
				   (data <raw-string>)
				   (count <raw-int>))
  literals: ('replace 'prepend 'append)
{
  int mode;
  if (EQ(edit_mode,TLREF(0)))
    mode = PropModeReplace;
  else if (EQ(edit_mode,TLREF(1)))
    mode = PropModePrepend;
  else if (EQ(edit_mode,TLREF(2)))
    mode = PropModeAppend;
  else
    scheme_error( "change-property: edit-mode ~s invalid", 1, edit_mode );

  XChangeProperty( win_dsp, win_xid,
		   property,
		   type,
		   format,
		   mode,
		   data,
		   count );
  RETURN0();
})
