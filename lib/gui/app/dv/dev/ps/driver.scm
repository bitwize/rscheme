
(define-class <ps-device> (<graphics-device>)
  (ps-port type: <output-port>)
  (ctm type: <transform>)
  (current-point init-value: #f)
  (owner init-value: #f)
  (ps-currentlinewidth init-value: 1)
  (used-fonts init-value: '()))

(define (close-ps-device (dev <ps-device>))
  (if (and (not (get-property dev 'eps? #f))
           (not (get-property dev 'num-pages #f)))
      (format (ps-port dev) "showpage\n"))
  (format (ps-port dev) "%%EOF\n")
  (close-output-port (ps-port dev)))

(define (open-ps-device (file <string>) #optional npages)
  (let* ((port (open-output-file file))
	 (dev (make <ps-device>
		    ps-port: port
		    ctm: (make-affine-transform))))
    (format port "%!PS-Adobe-3.0\n")
    (format port "%%Title: ~a\n" file)
    (format port "%%Creator: ~a (dv ~a)\n" 
	    (or (getenv "USER") "unknown user")
	    $dv-version)
    (format port "%%CreationDate: ~a\n"
            (with-module
                syscalls
              (time->string (time) "%Y-%m-%d %H:%M:%S %Z")))
    (if npages
        (begin
          (format port "%%Pages: ~d\n" npages)
          (set-property! dev 'num-pages npages)
          (set-property! dev 'current-page 0)))
    (format port "%%EndComments\n")
    (format port "/mx 6 array currentmatrix def\n")
    (format port "% make sure pdfmark is ignored in regular PostScript\n")
    (format port "/pdfmark where\n")
    (format port "  {pop}\n")
    (format port "  {userdict /pdfmark /cleartomark load put}\n")
    (format port "ifelse\n")
    dev))

(define (open-eps-device (file <string>) bbox)
  (let* ((port (open-output-file file))
	 (dev (make <ps-device>
		    ps-port: port
		    ctm: (make-affine-transform))))
    (set-property! dev 'eps? #t)
    (format port "%!PS-Adobe-3.0 EPSF-3.0\n")
    (let ((ll (lower-left bbox))
	  (ur (upper-right bbox)))
      (format port "%%BoundingBox: ~d ~d ~d ~d\n"
	      (inexact->exact (ceiling (psq (x ll))))
	      (inexact->exact (ceiling (psq (y ll))))
	      (inexact->exact (ceiling (psq (x ur))))
	      (inexact->exact (ceiling (psq (y ur))))))
    (format port "%%Title: ~a\n" file)
    (format port "%%Creator: ~a (dv ~a)\n" 
	    (or (getenv "USER") "unknown user")
	    $dv-version)
    (format port "%%EndComments\n")
    (format port "/mx 6 array currentmatrix def\n")
    dev))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   PostScript
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-method psq ((self <fixnum>)) self)
(define-method psq ((self <number>)) 
  (if (exact? self)
      (exact->inexact self)
      self))

(define-method scale ((self <ps-device>) (sx <real>) (sy <real>))
  (format (ps-port self) "~d ~d scale\n" (psq sx) (psq sy)))

(define-method rotate ((self <ps-device>) (angle <real>))
  (format (ps-port self) "~d rotate\n" (psq angle)))

(define-method moveto ((self <ps-device>) (pt <point>))
  (format (ps-port self) "~a ~a moveto\n" (psq (x pt)) (psq (y pt)))
  (set-current-point! self (transform pt (ctm self)))
  (values))

(define-method lineto ((self <ps-device>) (pt <point>))
  (format (ps-port self) "~a ~a lineto\n" (psq (x pt)) (psq (y pt)))
  (set-current-point! self (transform pt (ctm self)))
  (values))

(define-method curveto ((self <ps-device>) (h1 <point>)
					   (h2 <point>) 
					   (pt <point>))
  (format (ps-port self)
	  "~d ~d  ~d ~d  ~d ~d curveto\n"
	  (psq (x h1)) (psq (y h1))
	  (psq (x h2)) (psq (y h2))
	  (psq (x pt)) (psq (y pt)))
  (set-current-point! self (transform pt (ctm self)))
  (values))

;;; A counter-clockwise arc (increasing angle measure)
(define-method arc ((self <ps-device>) (center <point>)
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

;;; A clockwise arc (decreasing angle measure)
(define-method arcn ((self <ps-device>) (center <point>)
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

(define-method setdash ((self <ps-device>) (segments <vector>)
                                           (offset <real>))
  (format (ps-port self)
          "[~a] ~d setdash\n" 
          (string-join " " (map number->string (vector->list segments)))
          offset))

(define-method closepath ((self <ps-device>))
  (format (ps-port self) "closepath\n")
  (values))

(define-method stroke ((self <ps-device>))
  (format (ps-port self) "gsave\n")
  (format (ps-port self) "  mx setmatrix\n")
  (format (ps-port self) "  ~d setlinewidth\n" (ps-currentlinewidth self))
  (format (ps-port self) "  stroke\n")
  (format (ps-port self) "grestore\n")
  (format (ps-port self) "newpath\n")
  (set-current-point! self #f)
  (values))

(define-method fill ((self <ps-device>))
  (format (ps-port self) "gsave\n")
  (format (ps-port self) "  mx setmatrix\n")
  (format (ps-port self) "  fill\n")
  (format (ps-port self) "grestore\n")
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
  
(define-method show ((self <ps-device>) (str <string>))
  (write-ps-string (ps-port self) str)
  (format (ps-port self) " show\n")
  (values))

(define-method xshow ((self <ps-device>) (str <string>) (x-widths <list>))
  (write-ps-string (ps-port self) str)
  (format (ps-port self) " [~j] xshow\n" x-widths)
  (values))

(define-method setfont ((self <ps-device>) (font <text-font>))
  (let ((fn (get-ps-font-name font)))
    (if (not (member fn (used-fonts self)))
        (set-used-fonts! self (cons fn (used-fonts self))))
    (format (ps-port self) "/~a findfont ~d scalefont setfont\n"
            fn
            (psq (font-size font)))))

(define-method setlinewidth ((self <ps-device>) width)
  (set-ps-currentlinewidth! self width)
  (format (ps-port self) "~d setlinewidth\n" (psq width)))

(define-method setcolor ((self <ps-device>) colspec)
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

(define-method device-color ((dev <ps-device>) colorspec)
  ;; identity xform -- let `setcolor' deal with it
  colorspec)

(define-method translate ((self <ps-device>) (delta <point>))
  (let ((nctm (translate (ctm self) delta)))
    ;(format #t "@@@ translate ~s : ~s => ~s\n" delta (ctm self) nctm)
    (set-ctm! self nctm))
  (format (ps-port self) "~d ~d translate\n" (psq (x delta)) (psq (y delta))))

(define-method concat ((self <ps-device>) tm)
  (let ((nctm (concatenate-transform (ctm self) tm)))
    ;(format #t "@@@ concat ~s : ~s => ~s\n" tm (ctm self) nctm)
    (set-ctm! self nctm))
  ;;
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

(define-method with-gstate-saved ((self <ps-device>) thunk)
  (let ((saved-ctm (ctm self))
        (saved-pt (current-point self)))
    ;(format #t "@@@ save ~s ~s\n" saved-pt saved-ctm)
    (format (ps-port self) "gsave\n")
    (thunk)
    (format (ps-port self) "grestore\n")
    ;(format #t "@@@ restore ~s ~s\n" saved-pt saved-ctm)
    (set-current-point! self saved-pt)
    (set-ctm! self saved-ctm)
    (values)))

(define-method showpage ((self <ps-device>))
  (assert (not (get-property self 'eps? #f)))
  (format (ps-port self) "showpage\n")
  ;(format #t "@@@ showpage\n")
  (set-ctm! self (make-affine-transform)))

;;;
;;;  Document structuring stuff
;;;

(define-method startpage ((self <ps-device>) #optional label)
  (let ((pg (+ 1 (get-property self 'current-page))))
    (format (ps-port self) "%%Page: ~a ~d\n" (or label "?") pg)
    (set-property! self 'current-page pg)))

(define-method add-prolog-font ((self <ps-device>) pfa)
  (format (ps-port self) "%%--BeginFont-- ~s\n" pfa)
  (write-string (ps-port self) (file->string pfa))
  (format (ps-port self) "\n%%--Endfont-- ~s\n" pfa))

(define-method endprolog ((self <ps-device>))
  (format (ps-port self) "%%EndProlog\n"))

(define-method transform-to-page-coord ((self <ps-device>) (item <rect>))
  ;;; XXX note:  this is broken when translate's happen as well as moveto's
  ;;; XXX (i.e., for table cells)
  (let ((nitem (transform item (ctm self))))
    ;(format #t "@@@ CTM ~s : ~s => ~s\n" (ctm self) item nitem)
    nitem))

(define-method pdfmark/ann ((self <ps-device>) #rest info)
  (let ((p (ps-port self)))
    (format p "[ ")
    (let loop ((i info))
      (if (null? i)
          (format p "  /ANN pdfmark\n")
          (begin
            (format p "  /~a ~a\n" 
                    (keyword->symbol (car i))
                    (to-pdfmark (cadr i)))
            (loop (cddr i)))))))

(define-method to-pdfmark ((self <number>))
  (number->string self))

(define-method to-pdfmark ((self <list>))
  (string-append "["
                 (string-join " " (map to-pdfmark self))
                 "]"))

(define-method to-pdfmark ((self <symbol>))
  (format #f "/~a" self))

(define-method to-pdfmark ((self <rect>))
  (format #f "[~a ~a ~a ~a]" 
          (x (lower-left self))
          (y (lower-left self))
          (x (upper-right self))
          (y (upper-right self))))
