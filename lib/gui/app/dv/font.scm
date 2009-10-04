
(define-class <text-font-family> (<object>)
  (font-name type: <string>)
  (properties type: <vector> init-value: '#())
  (shapes type: <vector> init-value: '#()))

(define-class <text-font-shape> (<object>)
  (font-member type: <string>)
  (font-family type: <text-font-family>)
  postscript-name
  x-name)

(define-method to-string ((self <text-font-family>)) (font-name self))
(define-method to-string ((self <text-font-shape>)) (postscript-name self))

;;;

(define-class <text-font-repr> (<object>)
  (font-shape type: <text-font-shape>)
  (font-size type: <real>)
  (x-font-name type: <string>))

(define-method to-string ((self <text-font-repr>)) 
  (string-append (postscript-name (font-shape self))
		 "-"
		 (number->string (font-size self))))

(define-method font-name ((self <text-font-shape>))
  (font-name (font-family self)))

;;; these are persistent handles to font family/shapes

(define-class <text-font> (<object>)
  (font-name type: <string>)
  (font-member type: <string>)
  (font-size type: <real>))

;;;

(define *font-library* (make-string-table))
(define *text-font-map* (make-object-table))

(define-method transient-for ((self <text-font>))
  (or (table-lookup *text-font-map* self)
      (let* ((sh (get-font-shape (get-font-family (font-name self))
				 (font-member self)))
	     (rep (make <text-font-repr>
			font-shape: sh
			x-font-name: (format #f (x-name sh) (font-size self))
			font-size: (font-size self))))
	(table-insert! *text-font-map* self rep)
	rep)))

;;;

(define (get-font-family font-name)
  (or (table-lookup *font-library* font-name)
      (begin
	(wm 610 "Substituting `Times' for `~a'" font-name)
	(get-font-family "Times"))))

(define (get-font-shapes (font <text-font-family>))
  (vector->list (vector-map font-member (shapes font))))

(define-syntax (find-in-vector vec slot eq key miss)
  (let (((v <vector>) vec))
    (let loop ((i 0))
      (if (< i (vector-length v))
	  (if (eq key (slot (vector-ref v i)))
	      (vector-ref v i)
	      (loop (+ i 1)))
	  miss))))
	      
(define (get-font-shape (font <text-font-family>) (shape <string>))
  (find-in-vector (shapes font) 
		  font-member
		  string=?
		  shape
		  (let* ((first (vector-ref (shapes font) 0)))
		    (wm 611 "Substituting `~a' for `~a'" 
			(font-member first) shape)
		    first)))

;;;

(define (get-text-font family style size)
  (let ((f (get-font-shape (get-font-family family) style)))
    (make <text-font>
	  font-name: (font-name f)
	  font-member: (font-member f)
	  font-size: size)))

(define (default-text-font)
  (get-text-font "Times" "Roman" 18))

;;;

(let ((times  (make <text-font-family> font-name: "Times"))
      (symbol (make <text-font-family> font-name: "Symbol"))
      (helv   (make <text-font-family> font-name: "Helvetica"))
      (cour   (make <text-font-family> font-name: "Courier"))
      (school (make <text-font-family> font-name: "NewCenturySchoolbook"))
      (book   (make <text-font-family> font-name: "Bookman"))
      (palatino (make <text-font-family> font-name: "Palatino"))
      (minion (make <text-font-family> font-name: "Minion"))
      (univers (make <text-font-family> font-name: "Univers"))
      (briemmono (make <text-font-family> font-name: "BriemMono"))
      (avantg   (make <text-font-family> font-name: "AvantGarde")))
  ;
  (define (setshapes fam lst xname)
    (set-shapes!
     fam
     (vector-map
      (lambda (s)
	(make <text-font-shape>
	      font-member: (car s)
	      postscript-name: (string-append (font-name fam) (cadr s))
	      x-name: (if (pair? (cddr s))
			  (format #f "-adobe-~a-~a-*-*-~~d-*-*-*-*-*-*-*"
				  xname
				  (caddr s))
			  #f)
	      font-family: fam))
      (list->vector lst))))
  ;
  (for-each (lambda (f)
	      (table-insert! *font-library* (font-name f) f))
	    (list times symbol helv cour school book palatino avantg 
                  minion univers briemmono))
  ;
  ;
  ;     (member-within-family postscript-suffix X11-fontname)
  ;
  (setshapes times
	     '(("Roman" "-Roman" "medium-r")
	       ("Bold" "-Bold" "bold-r")
	       ("Italic" "-Italic" "medium-i")
	       ("Bold Italic" "-BoldItalic" "bold-i"))
	     "times")
  ;
  (setshapes minion
             '(("Condensed" "-Condensed" "medium-r")
               ("Condensed Italic" "-CondensedItalic" "medium-i")
               ("Condensed Bold" "-BoldCondensed" "bold-r")
               ("Condensed Bold Italic" "-BoldCondensedItalic" "bold-i"))
             "minion")
  ;
  (setshapes univers
             '(("Condensed" "-UltraCondensed" "medium-r")
               ("Condensed Italic" "-UltraCondensedItalic" "medium-i")
               ("Condensed Bold" "-BoldUltraCondensed" "bold-r")
               ("Condensed Bold Italic" "-BoldUltraCondensedItalic" "bold-i"))
             "univers")
  ;
  (setshapes briemmono
             '(("Condensed" "-Condensed" "medium-r")
               ("Condensed Italic" "-CondensedItalic" "medium-i")
               ("Condensed Bold" "-CondensedBold" "bold-r")
               ("Condensed Bold Italic" "-CondensedBoldItalic" "bold-i"))
             "briemmono")
  ;
  (setshapes symbol
             '(("Regular" "" "medium-r"))
             "symbol")
  ;
  (setshapes helv
	     '(("Regular" "" "medium-r")
	       ("Bold" "-Bold" "bold-r")
	       ("Italic" "-Oblique" "medium-i")
	       ("Bold Italic" "-BoldOblique" "bold-i"))
	     "helvetica")
  ;
  (setshapes palatino
             '(("Roman" "-Roman" "medium-r")
               ("Bold" "-Bold" "bold-r")
               ("Italic" "-Italic" "medium-i")
               ("Bold Italic" "-BoldItalic" "bold-i"))
             "times")
  ;
  (setshapes avantg
             '(("Book" "-Book" "medium-r")
               ("Book Oblique" "-BookOblique" "medium-i")
               ("Demi" "-Demi" "bold-r")
               ("Demi Oblique" "-DemiOblique" "bold-i"))
             "helvetica")
  ;
  (setshapes cour
	     '(("Regular" "" "medium-r")
	       ("Bold" "-Bold" "bold-r")
	       ("Italic" "-Oblique" "medium-i")
	       ("Bold Italic" "-BoldOblique" "bold-i"))
	     "courier")
  ;
  (setshapes book
	     '(("Regular" "-Demi")
	       ("Italic" "-DemiItalic")
	       ("Light" "-Light")
	       ("Light Italic" "-LightItalic"))
	     #f)
  ;
  (setshapes school
	     '(("Regular" "-Roman" "medium-r")
	       ("Bold" "-Bold" "bold-r")
	       ("Italic" "-Italic" "medium-i")
	       ("Bold Italic" "-BoldItalic" "bold-i"))
	     "new century schoolbook"))

(define-method font-afm ((self <text-font-repr>))
  ; `get-afm' caches, so this isn't (quite) as expensive as it looks
  ; however: we can't store the AFM directly in the <text-font> because
  ; the <text-font> is persistent and we don't want to drag in the
  ; whole AFM structure, so we'd do a hashing trick anyway -- might
  ; as well let get-afm do it.
  (get-afm (postscript-name (font-shape self))))

(define-method font-afm ((self <text-font>))
  (font-afm (transient-for self)))

(define (get-x-font-name (self <text-font>))
  (x-font-name (transient-for self)))

(define (get-ps-font-name (self <text-font>))
  (postscript-name (font-shape (transient-for self))))

;;;

(define-method char-widths ((self <text-font>) str)
  (let ((size (font-size self)))
    (map (lambda (cw)
           (* cw size))
         (char-widths (font-afm self) str))))

(define-method get-font-property ((self <text-font>) what)
  (* (font-size self)
     (/ (get-property (font-afm self) 
                      (case what
                        ((x-height) 'XHeight)
                        ((cap-height) 'CapHeight)
                        ((underline-position) 'UnderlinePosition)
                        ((underline-thickness) 'UnderlineThickness)
                        (else (em "Unknown font property ~s" what)))) 
        1000)))

;;;

(define-method string-width ((self <text-font>) (string <string>))
  (string-width (font-afm self) (font-size self) string))

(define-method string-bbox ((self <text-font>) (string <string>))
  (string-bbox (font-afm self) (font-size self) string))
  
;;;

(define *app-fonts*
  '((status-font "-adobe-helvetica-medium-r-normal--10-*-*-*-*-*-*-*")
    (miniconsole-font "-adobe-helvetica-medium-r-normal--10-*-*-*-*-*-*-*")
    (inspection-font "-adobe-helvetica-medium-r-normal--12-*-*-*-*-*-*-*")
    (menu-font "-adobe-helvetica-medium-r-normal--12-*-*-*-*-*-*-*")
    (menu-title-font "-adobe-helvetica-bold-r-normal--12-*-*-*-*-*-*-*")
    (minibuffer-prompt-font "-adobe-helvetica-bold-r-normal--12-*-*-*-*-*-*-*")
    (default-text-font "-adobe-courier-medium-r-normal--12-*-*-*-*-*-*-*")
    (dialog-big-font "-adobe-helvetica-medium-r-normal--18-*-*-*-*-*-*-*")
    (dialog-label-font "-adobe-helvetica-medium-r-normal--12-*-*-*-*-*-*-*")
    (minibuffer-text-font "-adobe-courier-medium-r-normal--12-*-*-*-*-*-*-*")))
