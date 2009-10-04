
(define-class <ps-device> (<graphics-device>)
  (current-page-num type: <fixnum> init-value: 0)
  (ps-port type: <output-port>)
  (ctm type: <transform>)
  (current-point init-value: #f)
  (current-font init-value: #f)
  (owner init-value: #f)
  (used-fonts init-value: '())
  (binary-encoding-ok? init-value: #f))

(define-method currentpoint ((self <ps-device>))
  (current-point self))

(define-method currentmatrix ((self <ps-device>))
  (ctm self))

(define-method close-graphics-device ((dev <ps-device>))
  (if (not (get-property dev 'num-pages #f))
      (format (ps-port dev) "%%Pages: ~d\n" (current-page-num dev)))
  (format (ps-port dev) "%%EOF\n")
  (close-output-port (ps-port dev)))

(define (write-ps-file-comments port file title creator creator-app)
  (format port "%%Title: ~a\n" (or title file))
  (cond 
   ((and creator creator-app)
    (format port "%%Creator: ~a (dv ~a)\n" creator creator-app))
   (creator
    (format port "%%Creator: ~a\n" creator))
   (creator-app
    (format port "%%Creator: (~a)\n" creator-app)))
  ;;
  (format port "%%CreationDate: ~a\n"
          (time->string (time) "%Y-%m-%d %H:%M:%S %Z")))
  
(define (open-ps-device (file <string>)
                        #key (num-pages default: #f)
                        (title default: #f)
                        (creator default: #f)
                        (creator-app default: #f)
                        (bbox default: #f))
  (let* ((port (open-output-file file))
	 (dev (make <ps-device>
		    ps-port: port
		    ctm: (make-affine-transform))))
    (format port "%!PS-Adobe-3.0\n")
    (write-ps-file-comments port file title creator creator-app)
    (if num-pages
        (begin
          (format port "%%Pages: ~d\n" num-pages)
          (set-property! dev 'num-pages num-pages))
        (format port "%%Pages: (atend)\n"))
    (if bbox
        (bind ((llx lly urx ury (ps-integer-bbox bbox)))
          (format port "%%BoundingBox: ~d ~d ~d ~d\n" llx lly urx ury)))
    (format port "%%EndComments\n")
    (format port "%%BeginProlog\n")
    (gen-ps-prologue port)
    dev))

;(define *ps-prolog* (file->string "prolog.ps"))

(define (gen-ps-prologue port)
  ;; some definitions that make the interaction with the PDF driver easier
  (format port "/g { setgray } bind def\n")
  (format port "/rg { setrgbcolor } bind def\n")
  (format port "/k { setcmykcolor } bind def\n")
  (format port "/cs { setcolorspace } bind def\n")
  ;; we only use the scn for setting pattern colors; 
  ;; otherwise we use the 'g', 'rg', or 'k' operators
  (format port "/scn { patterndict exch get setcolor } bind def\n")
  ;;
  ;(format port "~a\n" *ps-prolog*)
  ;;
  (format port "% make sure pdfmark is ignored in regular PostScript\n")
  ;; 
  (format port "/pdfmark where\n")
  (format port "  {pop}\n")
  (format port "  {userdict /pdfmark /cleartomark load put}\n")
  (format port "ifelse\n"))

(define (open-eps-device (file <string>) (bbox <rect>)
                         #key 
                         (title default: #f)
                         (creator default: #f)
                         (creator-app default: #f))
  (let* ((port (open-output-file file))
	 (dev (make <ps-device>
		    ps-port: port
		    ctm: (make-affine-transform))))
    (set-property! dev 'eps? #t)
    (format port "%!PS-Adobe-3.0 EPSF-3.0\n")
    (write-ps-file-comments port file title creator creator-app)
    (bind ((llx lly urx ury (ps-integer-bbox bbox)))
      (format port "%%BoundingBox: ~d ~d ~d ~d\n" llx lly urx ury))
    (format port "%%EndComments\n")
    (gen-ps-prologue port)
    dev))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   PostScript
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (psq3 n)
  (let ((milli (inexact->exact (round (* n 1000.0)))))
    (cond
     ((= milli 0) 0)
     ((= milli 1000) 1)
     (else (/ milli 1000.0)))))

(define (ps-integer-bbox (self <rect>))
  (bind ((x y w h (rect->values self)))
    (values (inexact->exact (floor (psq x)))
            (inexact->exact (floor (psq y)))
            (inexact->exact (ceiling (psq (+ x w))))
            (inexact->exact (ceiling (psq (+ y h)))))))
  
(define-method psq ((self <fixnum>)) self)

(define-method psq ((self <number>)) 
  (if (exact? self)
      (exact->inexact self)
      (if (integer? self)
          (inexact->exact self)
          self)))

;(define-method psq ((self <quantity>))
;  (psq (quantity/ self 1pt)))

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

(define-method newpath ((self <ps-device>))
  (format (ps-port self) "newpath\n")
  (values))

(define-method rectclip ((self <ps-device>) (r <rect>))
  (format (ps-port self) "~d ~d ~d ~d rectclip\n"
          (psq (origin-x r))
          (psq (origin-y r))
          (psq (size-width r))
          (psq (size-height r))))

(define-method rectfill ((self <ps-device>) (r <rect>))
  (format (ps-port self) "~d ~d ~d ~d rectfill\n"
          (psq (origin-x r))
          (psq (origin-y r))
          (psq (size-width r))
          (psq (size-height r))))

(define-method rectstroke ((self <ps-device>) (r <rect>))
  (format (ps-port self) "~d ~d ~d ~d rectstroke\n"
          (psq (origin-x r))
          (psq (origin-y r))
          (psq (size-width r))
          (psq (size-height r))))

(define-method stroke ((self <ps-device>))
  (format (ps-port self) "stroke\n")
  (values))

(define-method fill ((self <ps-device>))
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
  
(define-method show ((self <ps-device>) item)
  (cond
   ((string? item)
    (write-ps-string (ps-port self) item)
    (format (ps-port self) " show\n")
    (values))
   ((symbol? item)
    (format (ps-port self) "/~a glyphshow\n" item)
    (values))
   (else
    (error "~s: don't know how to show ~s" self item))))

(define-method xshow ((self <ps-device>) (str <string>) (x-widths <list>))
  (write-ps-string (ps-port self) str)
  (if (and (binary-encoding-ok? self)
           (every? (lambda (w) (< (abs w) 32)) x-widths))
      (let ((p (ps-port self))
            (n (length x-widths)))
        (format p "\n% homogeneous number array: type=16 bit fixed-point, len=~d\n~a"
                n
                (pack-string u8: 149
                             u8: (+ 32 10) ; 16-bit fixed-point NBO w/10-bit scale
                             u16/b: n))
        (for-each
         (lambda (w)
           (write-string p (pack-string 
                            s16/b: (inexact->exact (round (* w 1024))))))
         x-widths)
        (format p " xshow\n"))
      (format (ps-port self) " [~j] xshow\n" x-widths))
  (values))

(define-method currentfont ((self <ps-device>))
  (or (current-font self)
      (error "no current font")))

(define-method setfont ((self <ps-device>) (font <text-font>))
  (let ((fn (postscript-name font)))
    (if (not (member fn (used-fonts self)))
        (set-used-fonts! self (cons fn (used-fonts self))))
    (set-current-font! self font)
    (format (ps-port self) "/~a findfont ~d scalefont setfont\n"
            fn
            (psq (font-size font)))))

(define-method clip ((self <ps-device>))
  (format (ps-port self) "clip\n"))
  
(define-method setlinejoin ((self <ps-device>) type)
  (format (ps-port self) "~d setlinejoin\n"
          (case type
            ((miter) 0)
            ((round) 1)
            ((bevel) 2)
            (else
             (error "setlinejoin: unknown line join type: ~s" type)))))

(define-method setmiterlimit ((self <ps-device>) limit)
  (format (ps-port self) "~d setmiterlimit\n" limit))
  
(define-method setlinecap ((self <ps-device>) type)
  (format (ps-port self) "~d setlinecap\n"
          (case type
            ((butt) 0)
            ((round) 1)
            ((square) 2)
            (else
             (error "setlinecap: unknown line cap type ~s" type)))))

(define-method setlinewidth ((self <ps-device>) width)
  (format (ps-port self) "~d setlinewidth\n" (psq width)))

(define-method setcolor ((self <ps-device>) (colspec <string>) #optional mode)
  (format (ps-port self) "~a" colspec)
  (if mode 
      (format (ps-port self) "  % for ~s" mode))
  (newline (ps-port self)))

(define-method device-color ((dev <ps-device>) c)
  (let ((u (postscript-color c dev)))
    (if (vector? u)
        (vector-ref u 0)
        (~ "~a ~a" (cadr u) (car u)))))

(define-method postscript-color ((self <symbol>) dev)
  (case self
    ((black) '(g "0"))
    ((white) '(g "1"))
    (else
     (error "postscript-color: undefined color name: ~s" self))))

(define-method uncolored-pattern-color ((self <ps-device>) pat use)
  (vector (~ "[/Pattern ~a] cs ~a /~a scn"
             (case (car use)
               ((k) "/DeviceCMYK")
               ((rg) "/DeviceRGB")
               ((g) "/DeviceGray")
               (else (error "~s: Unknown postscript color space: ~s"
                            self
                            (car use))))
             (cadr use)
             pat)))

(define-method use-resource ((self <ps-device>) type name)
  ;; the PS driver includes everything up front
  (values))

(define-method postscript-color ((self <pair>) dev)
  (case (car self)
    ((cmyk) (bind ((c m y k (list->values (cdr self))))
              (list 'k (~ "~d ~d ~d ~d" (psq3 c) (psq3 m) (psq3 y) (psq3 k)))))
    ((rgb) (bind ((r g b (list->values (cdr self))))
             (list 'rg (~ "~d ~d ~d" (psq3 r) (psq3 g) (psq3 b)))))
    ((gray) (list 'g (~ "~d" (psq3 (cadr self)))))
    ((pattern) 
     (use-resource dev 'pattern (cadr self))
     (if (pair? (cddr self))
         (let ((use (postscript-color (caddr self) dev)))
           (uncolored-pattern-color dev (cadr self) use))
         (vector (~ "/Pattern cs /~a scn" (cadr self))
                 (~ "/Pattern CS /~a SCN" (cadr self)))))
    (else
     (error "postscript-color: unknown color specification: ~s" self))))

(define-method postscript-color ((self <color>) dev) ; default method
  (postscript-color (cons 'rgb (values->list (color-rgb self))) dev))

(define-method postscript-color ((self <cmyk-color>) dev)
  (postscript-color (cons 'cmyk (values->list (color-cmyk self))) dev))

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
;;;   Images
;;;

(define-method composite-image ((self <ps-device>) image 
                                #key (image-matrix default: $identity-transform))
  (let ((p (ps-port self))
        (v (vector-map psq (matrix image-matrix))))
    (format p "gsave\n")
    ;;
#|
    (format p "~d ~d true\n" (image-width image) (image-height image))
    (format p "[~d ~d ~d ~d ~d ~d]\n"
            (vector-ref v 0)
            (vector-ref v 1)
            (vector-ref v 2)
            (vector-ref v 3)
            (vector-ref v 4)
            (vector-ref v 5))
    (format p "currentfile /ASCIIHexDecode filter\n")
    (format p "imagemask\n")

    (let ((accum 0)
          (shift #x80)
          (j 32))
      (define (off)
        (set! shift (logical-shift-right shift 1)))
      (define (on)
        (set! accum (+ accum shift))
        (set! shift (logical-shift-right shift 1)))
      (define (flush)
        (format p "~02x " accum)
        (if (<= j 0)
            (begin
              (newline p)
              (set! j 32))
            (set! j (- j 1)))
        (set! accum 0)
        (set! shift #x80))

      (for-each-pixel
       image
       (lambda (x y pix)
         (if (eq? (bitwise-and x #b111) 0)
             (flush))
         (bind ((r g b a (pixel-rgba pix)))
           (if (< a 32768) (off) (on)))))
      (flush)
      (format p "\n>\n"))
    ;;
    |#
    (format p "~d ~d 8\n" (image-width image) (image-height image))
    (format p "[~d ~d ~d ~d ~d ~d]\n"
            (vector-ref v 0)
            (vector-ref v 1)
            (vector-ref v 2)
            (vector-ref v 3)
            (vector-ref v 4)
            (vector-ref v 5))
    (format p "currentfile /ASCIIHexDecode filter\n")
    (format p "false 3 colorimage\n")
    (for-each-pixel
     image
     (lambda (x y pix)
       (bind ((r g b a (pixel-rgba pix)))
         (format p "~02x ~02x ~02x\n" 
                 (logical-shift-right r 8)
                 (logical-shift-right g 8)
                 (logical-shift-right b 8)))))
    (format p ">\n")
    (format p "grestore\n")))
  
  
;;;

(define-method with-gstate-saved ((self <ps-device>) thunk)
  (let ((saved-ctm (ctm self))
        (saved-pt (current-point self))
        (saved-font (current-font self)))
    ;(format #t "@@@ save ~s ~s\n" saved-pt saved-ctm)
    (format (ps-port self) "gsave\n")
    (thunk)
    (format (ps-port self) "grestore\n")
    ;(format #t "@@@ restore ~s ~s\n" saved-pt saved-ctm)
    (set-current-point! self saved-pt)
    (set-ctm! self saved-ctm)
    (set-current-font! self saved-font)
    (values)))

;;;
;;;  Document structuring stuff
;;;

(define-method starttext ((self <ps-device>))
  (format (ps-port self) "% starttext...\n"))

(define-method endtext ((self <ps-device>))
  (format (ps-port self) "% done with text...\n"))

(define-method startpage ((self <ps-device>) 
                          #optional label 
                          #key (bbox default: #f)
                               (orientation default: #f))
  ;;
  (let ((pg (+ 1 (current-page-num self)))
        (port (ps-port self)))
    ;;
    (if (= pg 1)
        (format port "%%EndProlog\n"))
    (format port "%%Page: ~a ~d\n" 
            (or label (number->string pg))
            pg)
    (if bbox
        (bind ((llx lly urx ury (ps-integer-bbox bbox)))
          (format port "%%PageBoundingBox: ~d ~d ~d ~d\n" llx lly urx ury)))
    (if orientation
        (format port "%%PageOrientation: ~a\n"
                (case orientation
                  ((portrait) "Portrait")
                  ((landscape) "Landscape")
                  (else (error "Invalid 'orientation: ~s' in startpage"
                               orientation)))))
    (set-current-page-num! self pg)))

(define-method endpage ((self <ps-device>))
  (assert (not (get-property self 'eps? #f)))
  (format (ps-port self) "showpage\n")
  (format (ps-port self) "%%PageTrailer\n")
  (set-ctm! self (make-affine-transform)))

(define-method include-eps ((self <ps-device>) source)
  (write-eps-to-ps-port source (ps-port self)))

(define-method write-eps-to-ps-port ((self <string>) port)
  (write-eps-to-ps-port (string->file self) port))

(define-method write-eps-to-ps-port ((self <file-name>) port)
  (format port "100 dict begin\n")
  (format port "/showpage {} def\n")
  (format port "/_rseps save def\n")
  (format port "\n%%BeginDocument: ~a\n" self)
  ;;
  (for-each
   (lambda (l)
     (write-string port l)
     (write-string port "\n"))
   (string-split (file->string (pathname->os-path self))
                 (reg-expr->proc '(+ (or #\cr #\lf)))))
  ;;
  (format port "%%EndDocument\n")
  (format port "_rseps restore\n")
  (format port "end\n"))

(define eps-bbox (reg-expr->proc
                  '(seq "%%BoundingBox:"
                        (* space)
                        (save (seq (? #\-) (+ (or digit #\.))))
                        (+ space)
                        (save (seq (? #\-) (+ (or digit #\.))))
                        (+ space)
                        (save (seq (? #\-) (+ (or digit #\.))))
                        (+ space)
                        (save (seq (? #\-) (+ (or digit #\.)))))))


(define-method get-eps-bbox ((self <file-name>))
  (call-with-input-file
      (pathname->os-path self)
    (lambda (port)
      (let loop ()
        (let ((l (read-line port)))
          (bind ((s e llx lly urx ury (eps-bbox l)))
            (if s
                (let ((llx (string->number llx))
                      (lly (string->number lly))
                      (urx (string->number urx))
                      (ury (string->number ury)))
                  (make-rect llx lly (- urx llx) (- ury lly)))
                (loop))))))))

  

(define-method include-font ((self <ps-device>) font)
  (let* ((info (get-font-pdf-data font))
         (c (memq 'content: info)))
    (if c
        (let ((nm (cadr (memq 'base-font: info)))
              (p (ps-port self)))
          (format p "%%BeginResource: ~a\n" nm)
          ;; really want a C api to ensure_memory_mapped()...
          (ensure-memory-mapped (cadr c))
          (write-string (ps-port self) (cadr c))
          (format p "\n%%EndResource\n")))))

(define (ensure-memory-mapped (str <string>))
  (with-module
      primops
    (let* (((n <fixnum>) (string-length str))
           (tmp (if (> n 0) 
                    (string-ref str (sub1 n))
                    #f)))
      ;;
      (let loop (((i <fixnum>) 0))
        (if (fixnum<? i n)
            (begin
              (set! tmp (string-ref str i))
              (loop (+ i 4096)))
            (values))))))

(define-method pdfmark-annotation ((self <ps-device>) #rest info)
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
