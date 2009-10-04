
(define (atom-name (dpy <x-display>) (atom-id <fixnum>))
  (let ((r (internal-rpc dpy 
			 (make-buffer u1: 17 ; GetAtomName
				      u1: 0
				      u2: 2
				      u4: atom-id))))
    (with-unpacked (common-reply r)
		   (u1: - u1: - u2: - u4: - u2: n)
      (substring (remainder-reply r) 0 n))))

(define (intern-atom (dpy <x-display>) xatom)
  (let (((atom-name <string>) (if (symbol? xatom)
				  (symbol->string xatom)
				  xatom)))
    (or (table-lookup (atom-table dpy) atom-name)
	(table-lookup *predefined-atoms* atom-name)
	(find-atom-on-server dpy atom-name #f))))


(define (find-atom (dpy <x-display>) xatom)
  (let (((atom-name <string>) (if (symbol? xatom)
				  (symbol->string xatom)
				  xatom)))
    (or (table-lookup (atom-table dpy) atom-name)
	(table-lookup *predefined-atoms* atom-name)
	(find-atom-on-server dpy atom-name #t))))

(define (find-atom-on-server (dpy <x-display>)
			     (atom-name <string>)
			     only-if-exists?)
  (bind ((pad-n pad-str (pad4 (string-length atom-name))))
    (with-unpacked (common-reply
		    (internal-rpc 
		     dpy 
		     (vector
		      (make-buffer u1: 16 ; InternAtom
				   u1: (if only-if-exists? 1 0)
				   u2: (+ 2 
					  (quotient 
					   (+ (string-length atom-name) pad-n)
					   4))
				   u2: (string-length atom-name)
				   u2: 0)
		      atom-name
		      pad-str)))
     (u1: -
      u1: -
      u2: -
      u4: -
      u4: id)
     (if (eq? id 0)
	 #f
	 (begin
	   (table-insert! (atom-table dpy) atom-name id)
	   id)))))

(define *predefined-atoms*
  (let ((tbl (make-string-table))
	(vec '#("PRIMARY" "SECONDARY" "ARC" "ATOM" "BITMAP"
		"CARDINAL" "COLORMAP" "CURSOR" "CUT_BUFFER0"
		"CUT_BUFFER1" "CUT_BUFFER2" "CUT_BUFFER3"
		"CUT_BUFFER4" "CUT_BUFFER5" "CUT_BUFFER6"
		"CUT_BUFFER7" "DRAWABLE" "FONT" "INTEGER" "PIXMAP"
		"POINT" "RECTANGLE" "RESOURCE_MANAGER"
		"RGB_COLOR_MAP" "RGB_BEST_MAP" "RGB_BLUE_MAP"
		"RGB_DEFAULT_MAP" "RGB_DEFAULT_MAP" "RGB_GRAY_MAP"
		"RGB_GREEN_MAP" "STRING" "VISUALID" "WINDOW"
		"WM_COMMAND" "WM_HINTS" "WM_CLIENT_MACHINE"
		"WM_ICON_NAME" "WM_ICON_SIZE" "WM_NAME"
		"WM_NORMAL_HINTS" "WM_SIZE_HINTS" "WM_ZOOM_HINTS"
		"MIN_SPACE" "NORM_SPACE" "MAX_SPACE" "END_SPACE"
		"SUPERSC.LPT_X" "SUPERSC.LPT_Y" "SUBSC.LPT_X"
		"SUBSC.LPT_Y" "UNDERLINE_POSITION"
		"UNDERLINE_THICKNESS" "STRIKEOUT_DESCENT"
		"ITALIC_ANGLE" "X_HEIGHT" "QUAD_WIDTH" "WEIGHT"
		"POINT_SIZE" "RESOLUTION" "COPYRIGHT" "NOTICE"
		"FONT_NAME" "FAMILY_NAME" "FULL_NAME" "CAP_HEIGHT"
		"WM_CLASS" "WM_TRANSIENT_FOR")))
    (let loop ((i 0))
      (if (< i (vector-length vec))
	  (begin
	    (table-insert! tbl (vector-ref vec i) (+ i 1))
	    (loop (+ i 1)))
	  tbl))))
