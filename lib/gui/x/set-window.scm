
(define-method set-window-cursor! ((self <x-window>) cursor)
  (internal-send
   (x-display self)
   (make-buffer u1: 2 ; ChangeWindowAttributes
		u1: 0
		u2: (+ 3 1)
		u4: (x-id self)
		u4: #x4000
		u4: (if (eq? cursor 'none)
			0
			(x-id cursor)))))

(define-macro (chwindow self k value)
  `(internal-send
    (x-display self)
    (make-buffer u1: 2 ; ChangeWindowAttributes
		 u1: 0
		 u2: (+ 3 1)
		 u4: (x-id self)
		 u4: ,k
		 u4: ,value)))

(define (set-window-event-mask! (self <x-window>) mask)
  (assert (event-mask? mask))
  (chwindow self #x800 (make-event-mask mask)))

#|
(define-macro (configwin self k value)
  `(internal-send
    (x-display self)
    (make-buffer u1: 12 ; ConfigureWindow
		 u1: 0
		 u2: (+ 3 1)
		 u4: (x-id self)
		 u4: ,k
		 u4: ,value)))

(define (set-drawable-x! (self <x-drawable>) x) (configwin self #x1 x))
(define (set-drawable-y! (self <x-drawable>) y) (configwin self #x2 y))
(define (set-drawable-width! (self <x-drawable>) w) (configwin self #x4 w))
(define (set-drawable-height! (self <x-drawable>) h) (configwin self #x8 h))
|#

(define (set-drawable-frame! (self <x-drawable>) x y w h)  ; CLX extn
  (internal-send
   (x-display self)
   (make-buffer u1: 12 ; ConfigureWindow
		u1: 0
		u2: (+ 3 4)
		u4: (x-id self)
		u4: #xF
		u4: x
		u4: y
		u4: w
		u4: h)))
