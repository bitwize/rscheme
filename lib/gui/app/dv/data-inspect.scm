#|------------------------------------------------------------*-Scheme-*--|
 | File:	    gui/app/dv/data-inspect.scm
 |
 |          Copyright (C)1998 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |	    See <http://www.rscheme.org/> for the latest info.
 |
 | File version:     1.2
 | File mod date:    2005-02-18 14:32:20
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  &module;
 |
 | Purpose:          A general-purpose X-GUI data inspector
 `------------------------------------------------------------------------|#

(define (di:line-box font text-line y)
  (if (string=? text-line "")
      (make-rect 0 y 0 0)
      (bind ((w a d l r fa (text-extents font text-line)))
	(make-rect 0 y w fa))))

(define (di:text-box font text-lines)
  (bind ((box0 (di:line-box font (car text-lines) 0))
	 (fa (font-ascent font))
	 (lh (+ fa (font-descent font))))
    (let loop ((lines (cdr text-lines))
	       (box box0)
	       (y 0))
      (if (null? lines)
	  (values box fa lh)
	  (loop (cdr lines)
		(union-rect box (di:line-box font (car lines) (+ y fa)))
		(+ y lh))))))

(define (data-inspect obj #optional from)
  (bind ((scrn (on-screen (current-client)))
	 (dpy (on-display (current-client)))
	 (fnt (get-property dpy 'inspection-font))
	 (box font-ascent line-height
	      (di:text-box fnt (string-split
                                (with-output-to-string
                                  (lambda ()
                                    (print obj)))
                                #\newline)))
	 (win (create-window parent: (screen-root scrn)
			     x: 100
			     y: 100
			     width: (max 30 (min (+ (width box) 10) 500))
			     height: (max 30 (min (+ (height box) 4) 400))
			     event-mask: (if (gvec? obj)
					     '(exposure
					       button-press
					       key-press)
					     '(exposure
					       key-press))
			     background: (screen-white-pixel scrn)))
	 (gc (create-gcontext drawable: win
			      foreground: (screen-black-pixel scrn)
			      background: (screen-white-pixel scrn))))
    ;
    (define (follow slot)
      (if (list? obj)
	  (if (and (>= slot 0)
		   (< slot (length obj)))
	      (begin
		;(format #t "follow element[~d]\n" slot)
		(data-inspect (list-ref obj slot) win)))
	  (if (and (gvec? obj)
		   (>= slot 0)
		   (< slot (gvec-length obj)))
	      (begin
		;(format #t "follow slot[~d]\n" slot)
		(data-inspect (gvec-ref obj slot) win)))))
    ;
    (set-gcontext-font! gc fnt)
    ;
    (set-property! 
     win
     'exposure-thunk
     (lambda ()
       (clear-area win)
       (let ((lines (string-split
		     (with-output-to-string
		       (lambda ()
			 (print obj)))
		     #\newline)))
	 (let loop ((y 2)
		    (l lines))
	   (if (pair? l)
	       (begin
		 (draw-glyphs win gc 5 (+ y font-ascent) (car l))
		 (loop (+ y line-height) (cdr l))))))))
    ;
    (set-property! win
		   'button-press
		   (lambda (win pt state)
		     (follow (- (quotient (- (y pt) 2) line-height) 1))))
    ;
    (set-property! 
     win
     'key-press-handler
		   (lambda (ch state)
		     (if (char-numeric? ch)
			 (follow (- (char->integer ch) (char->integer #\0)))
			 (case ch
			   ((#\M-x #\x #\M-w)
			    (unmap-window win))
			   ;(display-force-output dpy)
			   ((#\C-c)
			    (let close-loop ((w win))
			      (unmap-window w)
			      (if (get-property w 'from #f)
				  (close-loop (get-property w 'from))
				  #|(display-force-output dpy)|#)))))))
    (if from
	(set-property! win 'from from))
    ;
    (map-window win)
    (display-force-output dpy)))
