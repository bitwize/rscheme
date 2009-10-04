
(define *focus-window* #f)

(define (my-focus-in-handler display #rest ignore #key window mode kind)
  (dm 132 "~s: focus in: ~s ~s ~s" (current-client) mode kind window)
#|
  (if (and (not *focus-window*) window)
      (begin
	(map-window (main-menu (display->client display)))
	(flush-client)))
|#
  (let* ((v (get-property window 'open-view #f)))
    (if v
	(begin
	  (dm 139 "~s: new next-owner" v)
	  (set-next-owner! (current-client) v))))
  (let ((h (get-property window 'focus-in #f)))
    (if h
	(h window mode kind)))
  (set! *focus-window* window)
  #t)

(define (my-focus-out-handler display #rest ignore #key window mode kind)
  (dm 133 "~s: focus out: ~s ~s ~s" (current-client) mode kind window)
  ;; should probably reset the keystate of the de-focussed window
  (let ((h (get-property window 'focus-out #f)))
    (if h
	(h window mode kind)))
  (set! *focus-window* #f)
  ;(unmap-window (main-menu (display->client display)))
  ;(flush-client)
  #t)
