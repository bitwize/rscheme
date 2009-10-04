(define-class <gs-device> (<graphics-device>)
  (raw-ps-port init-value: #f)
  (ctm type: <transform>)
  (owner init-value: #f)
  (ps-currentlinewidth init-value: 1))

(define-method ps-port ((self <gs-device>))
  (or (raw-ps-port self)
      (let ((p (open-output-process "gs >/tmp/gs.log")))
        (set-raw-ps-port! self p)
        (reset self)
        (raw-ps-port self))))

(define-method reset ((self <gs-device>))
  (write-string (ps-port self) (text *ghostscript-preamble*)))

(define-method flush-output-port ((self <gs-device>))
  (if (raw-ps-port self)
      (flush-output-port (raw-ps-port self))))

(define (open-gs-device)
  (make <gs-device>
        ctm: (make-affine-transform)))

(define *ghostscript-preamble* {
erasepage initgraphics
/f0 /Courier-Bold findfont 10 scalefont def
/f1 /Times-Roman findfont 48 scalefont def
/f2 /Times-Bold findfont 48 scalefont def
/mx 6 array currentmatrix def

%     label x y width   linestart   - 
%     x                 tabtic      -

/linetic 20 def

/linestart {
  /width exch def
  gsave
    translate
    0.25 setlinewidth
    gsave
      [0.5 2] 0 setdash
      0 linetic moveto
      0 0 lineto
      width 0 lineto
      width linetic lineto stroke
      f0 setfont
      width 0 moveto 30 5 rmoveto show
    grestore
} def

/lineend {
  0 25 rmoveto 0 -50 rlineto 0.25 setlinewidth stroke
  grestore
} def

/ticmark {
  gsave
    0 translate
    0 0 moveto 3 -4 lineto -3 -4 lineto closepath fill
  grestore
} def

/tabtic {
  gsave
    [0.25 1] 0 setdash
    -10 moveto 0 linetic rlineto stroke
  grestore
} def

})

(define-method psq ((self <fixnum>)) self)

(define-method psq ((self <number>)) 
  (if (exact? self)
      (exact->inexact self)
      self))

(define-method moveto ((self <gs-device>) (pt <point>))
  (format (ps-port self) "~a ~a moveto\n" (psq (x pt)) (psq (y pt)))
  (values))

(define-method lineto ((self <gs-device>) (pt <point>))
  (format (ps-port self) "~a ~a lineto\n" (psq (x pt)) (psq (y pt)))
  (values))

(define-method curveto ((self <gs-device>) (h1 <point>)
					   (h2 <point>) 
					   (pt <point>))
  (format (ps-port self)
	  "~d ~d  ~d ~d  ~d ~d curveto\n"
	  (psq (x h1)) (psq (y h1))
	  (psq (x h2)) (psq (y h2))
	  (psq (x pt)) (psq (y pt)))
  (values))

(define-method arc ((self <gs-device>) (center <point>)
                                       (radius <real>) 
                                       (start-angle <real>)
                                       (end-angle <real>))
  (format (ps-port self)
	  "~d ~d  ~d ~d ~d arc\n"
	  (psq (x center)) (psq (y center))
	  (psq radius)
	  (psq start-angle)
          (psq end-angle))
  (values))

(define-method arcn ((self <gs-device>) (center <point>)
                                        (radius <real>) 
                                        (start-angle <real>)
                                        (end-angle <real>))
  (format (ps-port self)
	  "~d ~d  ~d ~d ~d arcn\n"
	  (psq (x center)) (psq (y center))
	  (psq radius)
	  (psq start-angle)
          (psq end-angle))
  (values))

(define-method setdash ((self <gs-device>) (segments <vector>)
                                           (offset <real>))
  (format (ps-port self)
          "[~a] ~d setdash\n" 
          (string-join " " (map number->string (vector->list segments)))
          offset))

(define-method closepath ((self <gs-device>))
  (format (ps-port self) "closepath\n")
  (values))

(define-method stroke ((self <gs-device>))
  (format (ps-port self) "stroke\n")
  (values))

(define-method fill ((self <gs-device>))
  (format (ps-port self) "fill\n")
  (values))

(define (write-ps-string port (str <string>))
  (write-char #\( port)
  (let loop ((i 0))
    (if (eq? i (string-length str))
	(write-char #\) port)
	(let* ((ch (string-ref str i))
	       (esc (assq ch '((#\( "\\(")
			       (#\) "\\)")
			       (#\nl "\\n")
			       (#\cr "\\r")
			       (#\tab "\\t")
			       (#\bs "\\b")
			       (#\ff "\\f")
			       (#\\ "\\\\")))))
	  (if esc
	      (write-string port (cadr esc))
	      (if (and (>= (char->integer ch) 32)
		       (< (char->integer ch) 127))
		  (write-char ch port)
		  (format port "\\~03o" (char->integer ch))))
	  (loop (+ i 1))))))
  
(define-method show ((self <gs-device>) (str <string>))
  (write-ps-string (ps-port self) str)
  (format (ps-port self) " show\n")
  (values))

(define-method xshow ((self <gs-device>) (str <string>) (x-widths <list>))
  (write-ps-string (ps-port self) str)
  (format (ps-port self) " [~j] xshow\n" x-widths)
  (values))

(define-method setfont ((self <gs-device>) (font <text-font>))
  (format (ps-port self) "/~a findfont ~d scalefont setfont\n"
	  (get-ps-font-name font)
	  (psq (font-size font))))

(define-method setlinewidth ((self <gs-device>) width)
  (set-ps-currentlinewidth! self width)
  (format (ps-port self) "~d setlinewidth\n" (psq width)))

(define-method setcolor ((self <gs-device>) colspec)
  (case colspec
    ((black) (format (ps-port self) "0 setgray\n"))
    ((white) (format (ps-port self) "1 setgray\n"))
    (else
     (case (car colspec)
       ((rgb) (format (ps-port self) "~d ~d ~d setrgbcolor\n"
		      (cadr colspec)
		      (caddr colspec)
		      (cadddr colspec)))
       ((gray) (format (ps-port self) "~d setgray\n" (cadr colspec)))))))

(define-method device-color ((dev <gs-device>) colorspec)
  ;; identity xform -- let `setcolor' deal with it
  colorspec)

(define-method translate ((self <gs-device>) (delta <point>))
  (format (ps-port self) "~d ~d translate\n" (psq (x delta)) (psq (y delta))))

(define-method rotate ((self <gs-device>) (angle <real>))
  (format (ps-port self) "~d rotate\n" (psq angle)))

(define-method scale ((self <gs-device>) (sx <real>) (sy <real>))
  (format (ps-port self) "~d ~d scale\n" (psq sx) (psq sy)))

(define-method concat ((self <gs-device>) tm)
  (let ((v (vector-map psq (matrix tm))))
    (format (ps-port self)
	    "[~d ~d ~d ~d ~d ~d] concat\n"
	    (vector-ref v 0)
	    (vector-ref v 1)
	    (vector-ref v 2)
	    (vector-ref v 3)
	    (vector-ref v 4)
	    (vector-ref v 5))
    (values)))

;;;

(define-method with-gstate-saved ((self <gs-device>) thunk)
  (format (ps-port self) "gsave\n")
  (thunk)
  (format (ps-port self) "grestore\n")
  (values))

