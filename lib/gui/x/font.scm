(define-method font-display ((self <x-font>))
  (x-display self))

(define-method font-id ((self <x-font>))
  (x-id self))

;;;


(define (open-font (display <x-display>) (name <string>))
  (bind ((pad-n pad-str (pad4 (string-length name)))
	 (fid (alloc-x-id display)))
    (internal-send
     display
     (vector
      (make-buffer u1: 45 ; OpenFont
		   u1: 0
		   u2: (+ 3 (quotient (+ pad-n (string-length name)) 4))
		   u4: fid
		   u2: (string-length name)
		   u2: 0)
      name
      pad-str))
    (make <x-font>
	  x-id: fid
	  x-display: display)))

(define (close-font (font <x-font>))
  (internal-send
   (x-display font)
   (make-buffer u1: 46 ; CloseFont
		u1: 0
		u2: 2
		u4: (x-id font)))
  (values))

;;;

(define translate-default #f)

(define (draw-glyphs (drawable <x-drawable>)
		     (gcontext <x-gcontext>)
		     (x <fixnum>)
		     (y <fixnum>)
		     sequence
		     #key (translate default: translate-default)
		          (width default: #f)
			  (size default: 'default)
			  (start default: 0)
			  (end default: (cond
					 ((string? sequence)
					  (string-length sequence))
					 ((list? sequence)
					  (length sequence)))))
  ;; NOTE -- the treatment of narrow/wide characters, as well
  ;;         as `translate' in general, is quite crude
  (bind ((narrow? (string? sequence))
	 (data (if narrow?
		   (gen-poly-text8 sequence start end translate size)
		   (gen-poly-text16 sequence start end translate size)))
	 (n (string-length data))
	 (pad-n pad-str (pad4 (string-length data))))
    (internal-send
     (x-display drawable)
     (vector
      (make-buffer u1: (if narrow? 
			   74  ; PolyText8
			   75) ; PolyText16
		   u1: 0
		   u2: (+ 4 (quotient (+ n pad-n) 4))
		   u4: (x-id drawable)
		   u4: (x-id gcontext)
		   s2: x
		   s2: y)
      data
      pad-str))))

(define (gen-poly-text8 sequence start end translate size)
  (string-append (make-buffer u1: (- end start)
			      u1: 0) ; "delta"?
		 (substring sequence start end)))

(define (gen-poly-text16 sequence start end translate size)
  ; ignore start/end/translate/size
  (let* ((n (length sequence))
	 (str (make-string (+ 2 (* 2 n)))))
    (bvec-write-unsigned-8 str 0 n) 
    (bvec-write-unsigned-8 str 1 0)
    (let loop ((lst sequence)
	       (i 2))
      (if (null? lst)
	  str
	  (let ((code (char->integer (car lst))))
	    (bvec-write-unsigned-8 str i (logical-shift-right code 8))
	    (bvec-write-unsigned-8 str (+ i 1) (bitwise-and code #xFF))
	    (loop (cdr lst) (+ i 2)))))))

#|

(define ov (car (open-views *open-doc*)))
(define xw (status-window ov))
(define s1 (drawable-screen xw))
(define fnt (open-font *dpy* "-adobe-helvetica-medium-r-normal--10-*-*-*-*-*-*-*"))

(define mygc (create-gcontext drawable: xw
			      foreground: (screen-black-pixel s1)
			      background: (screen-black-pixel s1)))
(set-font! mygc fnt)
(display-force-output *dpy*)

(clear-area xw)
(draw-glyphs xw mygc 5 12 "line")
(display-force-output *dpy*)

|#

(define-class <x-char-info> (<object>)
  (char-left-bearing type: <fixnum>)
  (char-right-bearing type: <fixnum>)
  (char-ascent type: <fixnum>)
  (char-descent type: <fixnum>)
  (char-width type: <fixnum>)
  (char-attributes type: <fixnum>))

(define-class <x-font-info> (<object>)
  (char-info type: <vector>)
  (font-all-chars-exist? type: <boolean>)
  (font-ascent type: <fixnum>)
  (font-default-char type: <fixnum>)
  (font-descent type: <fixnum>)
  font-direction ; one of (left-to-right right-to-left)
  (font-max-byte1 type: <fixnum>)
  (font-max-byte2 type: <fixnum>)
  (font-max-char type: <fixnum>)
  (font-min-byte1 type: <fixnum>)
  (font-min-byte2 type: <fixnum>)
  (font-min-char type: <fixnum>)
  (font-properties type: <list>)
  (max-char-info type: <x-char-info>)
  (min-char-info type: <x-char-info>))

;; id may refer to either a GC or a FONT; if it refers to a GC,
;; then info for the current font is returned

(define (query-font dpy id)
  (let* ((r (internal-rpc dpy
			  (make-buffer u1: 47 ; QueryFont
				       u1: 0
				       u2: 2
				       u4: id)))
	 (minb (decode-charinfo (common-reply r) 8))
	 (maxb (decode-charinfo (string-append (common-reply r)
					       (substring (remainder-reply r)
							  0 8))
				24))
	 ((rr <string>) (remainder-reply r)))
    (with-unpacked rr
		   (u4: -
		    u4: -
		    u2: min-char-or-byte2
		    u2: max-char-or-byte2
		    u2: default-char
		    u2: num-font-props
		    u1: draw-direction
		    u1: min-byte1
		    u1: max-byte1
		    u1: all-char-exist
		    u2: font-ascent
		    u2: font-descent
		    u4: num-char-infos)
      (let (((v <vector>) (make-vector num-char-infos)))
	(let loop ((i 0)
		   (k (+ 28 (* 8 num-font-props))))
	  (if (< i num-char-infos)
	      (begin
		(vector-set! v i (decode-charinfo rr k))
		(loop (+ i 1) (+ k 12)))
	      (make <x-font-info>
		    char-info: v
		    font-all-chars-exist?: (eq? all-char-exist 1)
		    font-ascent: font-ascent
		    font-descent: font-descent
		    font-default-char: default-char
		    font-direction: (case draw-direction 
				      ((0) 'left-to-right)
				      ((1) 'right-to-left))
		    font-max-byte1: max-byte1
		    font-max-byte2: max-char-or-byte2
		    font-max-char: max-char-or-byte2
		    font-min-byte1: min-byte1
		    font-min-byte2: min-char-or-byte2
		    font-min-char: min-char-or-byte2
		    font-properties: '() ; XXX stubbed out for now
		    max-char-info: maxb
		    min-char-info: minb)))))))

(define (list-font-names (dpy <x-display>)
                         (pattern <string>)
                         #key (max-fonts default: 65535)
                              (result-type default: 'list))
  (bind ((n (string-length pattern))
         (pad-n pad-str (pad4 n))
         (r (internal-rpc dpy
                          (vector
                           (make-buffer u1: 49  ; ListFonts
                                        u1: 0
                                        u2: (+ 2 (quotient (+ pad-n n) 4))
                                        u2: max-fonts
                                        u2: n)
                           pattern
                           pad-str))))
    (with-unpacked (common-reply r)
                   (u1: - 
                    u1: -
                    u2: -
                    u4: reply-len
                    u2: num-names)
      (vector->named-type 
       result-type
       (unpack-list-of-str num-names (remainder-reply r))))))

                    

(define (decode-charinfo (str <string>) (offset <fixnum>))
  (bind ((lsb rsb cw asc desc attr (unpack* str offset 
					    s2: s2: s2: s2: s2: u2:)))
    (make <x-char-info>
	  char-left-bearing: lsb
	  char-right-bearing: rsb
	  char-descent: desc
	  char-ascent: asc
	  char-width: cw
	  char-attributes: attr)))

(define (fill-in-font-info (f <x-font>))
  (let ((i (query-font (x-display f) (x-id f))))
    (set-font-info! f i)
    i))

(define-syntax (get-font-info f)
  (or (font-info f)
      (fill-in-font-info f)))

(define (get-char-info font index)
  (let (((i <x-font-info>) (get-font-info font)))
    (vector-ref (char-info i) 
		(- index (font-min-char i)))))

;;;

(define-macro (define-font-info-wrapper name)
  `(define-method ,name ((self <x-font>))
     (,name (get-font-info self))))

(define-font-info-wrapper font-all-chars-exist?)
(define-font-info-wrapper font-ascent)
(define-font-info-wrapper font-default-char)
(define-font-info-wrapper font-descent)
(define-font-info-wrapper font-direction)
(define-font-info-wrapper font-max-byte1)
(define-font-info-wrapper font-max-byte2)
(define-font-info-wrapper font-max-char)
(define-font-info-wrapper font-min-byte1)
(define-font-info-wrapper font-min-byte2)
(define-font-info-wrapper font-min-char)
(define-font-info-wrapper font-properties)

;;

(define-macro (define-char-info-wrapper name)
  `(define-method ,name ((self <x-font>) index)
     (,name (get-char-info self (if (fixnum? index)
                                    index
                                    (char->integer index))))))

(define-char-info-wrapper char-ascent)
(define-char-info-wrapper char-left-bearing)
(define-char-info-wrapper char-right-bearing)
(define-char-info-wrapper char-ascent)
(define-char-info-wrapper char-descent)
(define-char-info-wrapper char-width)
(define-char-info-wrapper char-attributes)

;;;
;;; XXX a partial implementation...
;;;

(define (text-extents font (sequence <string>))
  (assert (or (instance? font <x-font>)
	      (instance? font <x-gcontext>)))
  (bind ((n (string-length sequence))
	 (data (bvec-alloc <byte-vector> (* n 2)))
	 (pad-n pad-str (pad4 (bvec-length data))))
    (let loop ((i 0))
      (if (< i n)
	  (begin
	    (bvec-set! data (* i 2) 0)
	    (bvec-set! data (+ 1 (* i 2)) (bvec-ref sequence i))
	    (loop (+ i 1)))
	  (let ((r (common-reply
		    (internal-rpc
		     (x-display font)
		     (vector
		      (make-buffer u1: 48 ; QueryTextExtents
				   u1: (bitwise-and 1 n)
				   u2: (+ 2 (quotient (+ (* n 2) pad-n) 4))
				   u4: (x-id font))
		      data
		      pad-str)))))
	    (with-unpacked r
	      (u1: -
	       u1: draw-direction
	       u2: -
	       u4: -
	       s2: font-ascent
	       s2: font-descent
	       s2: overall-ascent
	       s2: overall-descent
	       s4: overall-width
	       s4: overall-left
	       s4: overall-right)
	      (values overall-width
		      overall-ascent
		      overall-descent
		      overall-left
		      overall-right
		      font-ascent
		      (case draw-direction
			((0) 'left-to-right)
			((1) 'right-to-left))
		      #f)))))))
