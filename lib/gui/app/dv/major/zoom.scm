

(define (zoom-tool-button-press (in-view <open-view>)
				(at <point>) ;; window device coords
				modifier-state)
  (bind ((at (window->user-point in-view at)))
    ;; should adjust to zoom into the click point..
    ;; should also handle "zoom rects"
    (if (meta-state? modifier-state)
	(zoom-out in-view at)
	(zoom-in in-view at))))

(add-major-mode!
 (make <major-mode>
       name: 'zoom
       button-press-proc: zoom-tool-button-press))
