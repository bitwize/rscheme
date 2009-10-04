
(define (query-tree (window <x-window>))
  (let ((r (internal-rpc
	    (x-display window)
	    (make-buffer u1: 15 ; QueryTree
			 u1: 0
			 u2: 2
			 u4: (x-id window)))))
    (with-unpacked
     (common-reply r)
     (u1: -
      u1: -
      u2: -
      u4: n
      u4: root-win
      u4: parent-win)
     (let ((src (open-input-string (remainder-reply r)))
	   (dpy (x-display window)))
       (values 
	(vector->list
	 (unpack-list-of 
	  n
	  (lambda ()
	    (with-unpacked-from-input-string
	     src (u4: id)
	     (id->window dpy id)))))
	(if (eq? parent-win 0)
	    #f
	    (id->window dpy parent-win))
	(id->window dpy root-win))))))

(define (id->window dpy id)
  (let ((w (table-lookup (xid-table dpy) id)))
    (if w
	w
	(let ((w (make <x-window>
		   x-id: id
		   x-display: dpy
		   drawable-root: #f)))
	  (table-insert! (xid-table dpy) id w)
	  w))))

(define (translate-coordinates (source <x-window>)
			       (source-x <fixnum>)
			       (source-y <fixnum>)
			       destination)
  (with-unpacked
   (common-reply
    (internal-rpc (x-display source)
		  (make-buffer u1: 40 ; TranslateCoordinates
			       u1: 0
			       u2: 4
			       u4: (x-id source)
			       u4: (if destination (x-id destination) 0)
			       s2: source-x
			       s2: source-y)))
   (u1: -
    u1: same-screen
    u2: -
    u4: -
    u4: child-id
    s2: dest-x
    s2: dest-y)
   (values dest-x
	   dest-y
	   (if (eq? child-id 0)
	       #f
	       (id->window (x-display source) child-id)))))

(define (query-pointer (window <x-window>))
  (with-unpacked
   (common-reply
    (internal-rpc (x-display window)
		  (make-buffer u1: 38 ; QueryPointer
			       u1: 0
			       u2: 2
			       u4: (x-id window))))
   (u1: -
    u1: same-screen
    u2: -
    u4: -
    u4: root-id
    u4: child-id
    s2: root-x
    s2: root-y
    s2: win-x
    s2: win-y
    u2: state-mask)
   (values win-x win-y 
	   (not (eq? same-screen 0))
	   (if (eq? child-id 0)
	       #f
	       (id->window (x-display window) child-id))
	   state-mask
	   root-x
	   root-y
	   (id->window (x-display window) root-id))))
