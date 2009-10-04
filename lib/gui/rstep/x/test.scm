
,(use gui.rstep.x)
,(use graphics.geometry)

(define c (open-client))

(with-module repl
 (add-cmd-loop-prompt-hook!
  (lambda ()
    (flush-client c))))

,(use rs.sys.threads.manager rs.util.msgs)

(define (t)
  (let ((w (make-window frame: (make-rect 0 0 120 176)
			title: "Test1"
			client: c))
	(pause #f))
    (set! pause (make-button frame: (make-rect 10 40 50 21)
			     parent: (content-view w)
			     title: "Pause"))
    (let ((s (make-horz-scroller parent: (content-view w)
				 frame: (make-rect 3 153 110 20)))
	  (b (make-box frame: (make-rect 10 100 80 46)
		       parent: (content-view w)
		       resize-flags: #b010010)))
      (make-button frame: (make-rect 0 0 20 20)
		   resize-flags: #b100100
		   parent: b)
      (make-button frame: (make-rect 20 0 20 20)
		   resize-flags: #b100100
		   parent: b)
      (make-button frame: (make-rect 40 0 20 20)
		   resize-flags: #b100100
		   parent: b))
#|    (make-color-picker frame: (make-rect 10 170 80 100)
		       parent: (content-view w))|#
    (let ((p (make-progress-bar frame: (make-rect 5 5 90 20)
				resize-flags: #b100010
				parent: (content-view w)))
	  (fast (make-button frame: (make-rect 60 40 50 21)
			     parent: (content-view w)
			     title: "Fast"))
	  (cw1 (make-color-well frame: (make-rect 10 70 30 30)
				parent: (content-view w)))
	  (cw2 (make-color-well frame: (make-rect 40 70 30 30)
				parent: (content-view w)))
	  (cw3 (make-color-well frame: (make-rect 70 70 30 30)
				parent: (content-view w))))
#|
      (thread-resume
       (make-thread
	(lambda ()
	  (let loop ((thou 0))
	    (if (<= thou 300)
		(begin
		  (thread-sleep (if (state fast) 0.1 0.333))
		  (if (not (state pause))
		      (begin
			(set-value! p (/ thou 300))
			(loop (+ thou 1)))
		      (loop thou))))))))
|#
    (cons w p))))
