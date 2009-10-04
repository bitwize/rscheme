#|------------------------------------------------------------*-Scheme-*--|
 | File:	    packages/x11/safeglue.scm
 |
 |          Copyright (C)1998 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |	    See <http://www.rscheme.org/> for the latest info.
 |
 | File version:     1.2
 | File mod date:    1998-12-03 17:33:53
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  x11
 |
 | Purpose:          Provide glue conversions for X code
 `------------------------------------------------------------------------|#

(define-macro (define-X-glue args . body)
  `(define-safe-glue ,args
     ;;
     ;; types
     ;;
     type-handler: (<color>
		    (primitive "OBJ_ISA_FIXNUM" <fixnum>)
		    ("color_t ~a"
		     "(color_t)fx2int(~a)"))
     ;;
     type-handler: (<X-display> 
		    (direct-instance? <X-display>)
		    ("Display *~a" 
		     "OBJ_TO_DISPLAY( gvec_read(~a,SLOT(0)) )"))
     ;;
     type-handler: (<X-font> 
		    (direct-instance? <X-font>)
		    ("XFontStruct *~a"
		     "(XFontStruct *)OBJ_TO_RAW_PTR( gvec_read(~a,SLOT(0)) )"))
     ;;
     type-handler: (<X-drawable> 
		    (instance? <X-drawable>) display/0 xid/1)
     ;;
     type-handler: (<X-window> 
		    (direct-instance? <X-window>) display/0 xid/1)
     ;;
     type-handler: (<X-pixmap> 
		    (direct-instance? <X-pixmap>) display/0 xid/1)
     ;;
     type-handler: (<X-drawing-context>
		    (direct-instance? <X-drawing-context>)
		    display/0
		    win/1
		    ("GC ~a" "FX_TO_GC( gvec_read(~a,SLOT(2)) )" "~a_gc")
		    ("int ~a" "fx2int( gvec_read(~a,SLOT(3)) )" "~a_origin_x")
		    ("int ~a" "fx2int( gvec_read(~a,SLOT(4)) )" "~a_origin_y"))
     ;;
     type-handler: (<X-atom>
		    (direct-instance? <X-atom>)
		    ("Atom ~a" "(Atom)fx2int( gvec_read( ~a, SLOT(0) ) )"))
     ;;
     type-handler: (<X-event>
		    (direct-instance? <X-event>)
		    ("XEvent *~a" "(XEvent *)PTR_TO_DATAPTR(~a)"))
     ;;
     type-handler: (<X-image>
		    (direct-instance? <X-image>)
		    ("XImage *~a" "(XImage *)OBJ_TO_RAW_PTR( gvec_read(~a,SLOT(0)) )"))
     ;;
     type-handler: (<gd-image>
		    (direct-instance? <gd-image>)
		    ("gdImage *~a" "(gdImage *)OBJ_TO_RAW_PTR( gvec_read(~a,SLOT(0)) )"))
     ;;
     ;; view macros
     ;;
     type-view: (display/0
		 "Display *~a" 
		 "OBJ_TO_DISPLAY( gvec_read(~a,SLOT(0)) )"
		 "~a_dsp")
     ;
     type-view: (xid/1 
		 "XID ~a"
		 "FX_TO_XID( gvec_read(~a,SLOT(1)) )"
		 "~a_xid")
     ;
     type-view: (win/1 
		 "XID ~a"
		 "FX_TO_XID( gvec_read(~a,SLOT(1)) )"
		 "~a_win")
     ,@body))
