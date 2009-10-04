;; calls the procedure with a drawing context focussed on the given view

(define (call-with-locked-focus (view <view>) proc)
    (if (private-context view)
	(proc (private-context view))
	(with-focus-switched (shared-context (window view))
			     (origin view)
			     (clip-rect view)
			     proc)))

;; calls the proc with a drawing context locked
;; on the sub-area

(define (call-with-sub-area (ctx <X-drawing-context>) (area <rect>) proc)
    (let ((x (origin-x ctx))
    	  (y (origin-y ctx)))
	(with-focus-switched ctx
			     (offset-point (rect-origin area) x y)
			     (offset-rect area x y)
			     proc)))

;; may want to write this in C...

(define (with-focus-switched ctx new-origin new-clip proc)
   (bind ((old-origin old-clip (switch-focus ctx new-origin new-clip))
	; (ignore (begin
	;	    (format #t "switching focus to: origin=~s clip=~s\n" 
	;		    new-origin new-clip)
	;	    1))
   	  (#rest r (proc ctx)))
     ;(format #t "switching focus back to: origin=~s clip=~s\n" 
     ;	     old-origin old-clip)
     (switch-focus ctx old-origin old-clip)
     (list->values r)))

;;

(define (get-focus (self <X-drawing-context>))
    (values (origin self) (clip-rect self)))

;; may want to write this as "with-focus-switched" and 
;; have it call the procedure itself

(define-X-glue (switch-focus (ctx <X-drawing-context>)
			        (origin <point>)
			        (clip <rect>))
#|
    slots: ((<X-drawing-context> origin)
    	    (<X-drawing-context> origin-x)
	    (<X-drawing-context> origin-y)
    	    (<X-drawing-context> clip-rect))
|#
{
obj old_origin, old_clip;
XRectangle clip_rect;

    old_origin = gvec_read( raw_ctx, X_DRAWING_CONTEXT__ORIGIN );
    old_clip = gvec_read( raw_ctx, X_DRAWING_CONTEXT__CLIP_RECT );
    
    /* install the new focus values... */
    
    gvec_write( raw_ctx, X_DRAWING_CONTEXT__ORIGIN, origin );
    gvec_write( raw_ctx, X_DRAWING_CONTEXT__CLIP_RECT, clip );
    
    gvec_write_non_ptr( raw_ctx, 
			X_DRAWING_CONTEXT__ORIGIN_X, 
			gvec_read(origin,SLOT(0)) );
    gvec_write_non_ptr( raw_ctx, 
			X_DRAWING_CONTEXT__ORIGIN_Y, 
			gvec_read(origin,SLOT(1)) );
    
    clip_rect.x = RECT_X(clip);
    clip_rect.y = RECT_Y(clip);
    clip_rect.width = RECT_W(clip);
    clip_rect.height = RECT_H(clip);

    XSetClipRectangles( ctx_dsp, ctx_gc, 0, 0, &clip_rect, 1, 0 );
    
    /* return the old values... */
    
    REG0 = old_origin;
    REG1 = old_clip;
    RETURN(2);
})

