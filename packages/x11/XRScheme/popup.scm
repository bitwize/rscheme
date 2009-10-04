
(define-class <menu-cell> (<object>)
    title)

(define-class <popup-cover> (<control>)
    (value init-value: 0)
    (items init-value: '#uninit))

(define-method draw-view ((self <popup-cover>) (ctx <X-drawing-context>))
    (draw-control-cell ctx
    			(bounds self)
			(if (enabled? self)
	0
	1)
    (draw-string ctx (title (vector-ref (items self) (value self))) ...)
    (draw-icon ctx *popup-icon*
    	       (make <point>
	       	     x: (- (limit-x (bounds self)) 
			   2 
			   (width (icon-size *popup-icon*)))
		     y: 


;;

(define-safe-glue (draw-control-cell (ctx <X-drawing-context>)
				     (frame <rect>)
				     title (font <X-font>)
				     (icon <icon>)
				     (style_bits <raw-int>))
    literals: ((& *toolkit-colors*)
    	       '#(#f #f #f #f #f #f #f #f))
{
    text_color = gvec_read( TLREF(0), SLOT( style_bits & 15 ) );
    
    icon_posn = (style_bits >> 4) & 15;
    /* X: left/center/right  Y: top/center/bottom */
    
    text_posn = (style_bits >> 8) & 3;
    /* left/center/right */
    
    bezel = (style_bits >> 11) & 7;
    
    draw_bezel( ctx_dsp, ctx_win, ctx_gc, 
    		&R, TLREF(0),
		gvec_read( LITERAL(1), SLOT(bezel) ) );
    
    /* lay out other stuff based on R */
})
