
(define-class <pdf-context> (<object>)
  pdf
  fontmap
  well-known-objects)

(define-class <pdf-device> (<graphics-device>)
  pdf
  current-page
  fontmap                       ; map <font-entry> objects to symbolic name
  (current-page-num init-value: 0)
  well-known-objects)

(define-class <pdf-page> (<object>)
  (current-point init-value: #f)
  (current-font init-value: #f)
  (text-mode? init-value: #f)
  (text-linebase init-value: #f)
  ;(current-color init-value: #f)
  (resources type: <symbol-table>)
  fontmap
  content
  pdf-object
  (next-id init-value: 0))

;; a lighter weight variant of <pdf-page>...
(define-class <resource-accum-stream> (<object>)
  (text-mode? init-value: #f)
  (current-point init-value: #f)
  (current-font init-value: #f)
  content
  (resources type: <symbol-table>))

(define-method close-graphics-device ((self <pdf-device>))
  (flush-pdf (pdf self)))

(define (page-assign-resource-id (self <pdf-page>))
  (let ((id (next-id self)))
    (set-next-id! self (+ id 1))
    id))

(define (open-pdf-device filename)
  (make <pdf-device>
        pdf: (create-pdf filename)
        fontmap: (make-object-table)
        current-page: #f
        well-known-objects: (make-symbol-table)))

(define (make-simple-pattern (self <pdf-device>) 
                             #key 
                             (paint-type type: <symbol>)
                             (bbox type: <rect>) 
                             (step type: <size>) 
                             (content type: <function>))
  ;;
  (bind ((rsrc body (with-pdf-resource-accum self content))
         (pat (make-pdf-stream (pdf self) body))
         (d (dict pat)))
    ;;
    (table-insert! d 'Type 'Pattern)
    (table-insert! d 'PaintType 
                   (case paint-type
                     ((uncolored) 2)
                     ((colored) 1)
                     (else
                      (error "Unknown paint type: ~s" paint-type))))
    (table-insert! d 'PatternType 1)
    (table-insert! d 'TilingType 1)
    (table-insert! d 'Resources 
                   (if (zero? (table-size rsrc))
                       (get-well-known-object self 'emptydict)
                       (alloc-object (pdf self) rsrc)))
    (table-insert! d 'BBox (rect->bbox bbox))
    (table-insert! d 'XStep (dx step))
    (table-insert! d 'YStep (dy step))
    (alloc-object (pdf self) pat)))

(define (rect->bbox (self <rect>))
  (vector (origin-x self)
          (origin-y self)
          (limit-x self)
          (limit-y self)))

(define (make-well-known-object (self <pdf-device>) name)
  (case name
    ((emptydict) (alloc-dict (pdf self)))
    ;;
    ((Csp.cmyk) (alloc-object (pdf self) '#(Pattern DeviceCMYK)))
    ((Csp.rgb) (alloc-object (pdf self) '#(Pattern DeviceRGB)))
    ((Csp.g) (alloc-object (pdf self) '#(Pattern DeviceGray)))
    ;;
    ((neshading nwshading crosshatch)
     (make-simple-pattern
      self
      paint-type: 'uncolored
      bbox: (make-rect 0 0 10 10)
      step: (make-size 10 10)
      content: (lambda (dev)
                 (setlinewidth dev 0.5)
                 (if (memq name '(neshading crosshatch))
                     (for-each (lambda (x)
                                 (moveto dev (make-point x -5))
                                 (rlineto dev (make-size 25 25)))
                               '(-15 -10 -5 0 5)))
                 (if (memq name '(nwshading crosshatch))
                     (for-each (lambda (x)
                                 (moveto dev (make-point x -5))
                                 (rlineto dev (make-size -25 25)))
                               '(0 5 10 15 20)))
                 (stroke dev))))
    ;;
    ((redpolka)
     (let ((p1 (output-to-pdf-stream
                pdf
                (lambda (port)
                  (format port "5 0 m\n")
                  (format port "5 3 3 5 0 5 c\n")
                  (format port "-3 5 -5 3 -5 0 c\n")
                  (format port "h\n")
                  (format port "1 0 0 rg f\n")))))
       ;;
       (table-insert! (dict p1) 'Type 'Pattern)
       (table-insert! (dict p1) 'PaintType 1)
       (table-insert! (dict p1) 'PatternType 1)
       (table-insert! (dict p1) 'TilingType 1)
       (table-insert! (dict p1) 'Resources 
                      (get-well-known-object self 'emptydict))
       (table-insert! (dict p1) 'BBox '#(-10 -10 10 10))
       (table-insert! (dict p1) 'XStep 20)
       (table-insert! (dict p1) 'YStep 30)
       (alloc-object (pdf self) p1)))
    ;;
    (else
     (error "Undefined well-known PDF object: ~s" name))))

(define (get-well-known-object (self <pdf-device>) name)
  (or (table-lookup (well-known-objects self) name)
      (let ((item (make-well-known-object self name)))
        (table-insert! (well-known-objects self) name item)
        item)))

  #|
(define (patterns pdf p)
  (page-add-pattern-resource p 'redpolka (alloc-object pdf p1))
  (values)))
  (let ((emptydict (alloc-dict pdf)))
    (let ((p1 (output-to-pdf-stream
               pdf
               (lambda (port)
                 (format port "5 0 m\n")
                 (format port "5 3 3 5 0 5 c\n")
                 (format port "-3 5 -5 3 -5 0 c\n")
                 (format port "h\n")
                 (format port "1 0 0 rg f\n")))))
      ;;
      (table-insert! (dict p1) 'Type 'Pattern)
      (table-insert! (dict p1) 'PaintType 1)
      (table-insert! (dict p1) 'PatternType 1)
      (table-insert! (dict p1) 'TilingType 1)
      (table-insert! (dict p1) 'Resources emptydict)
      (table-insert! (dict p1) 'BBox '#(-10 -10 10 10))
      (table-insert! (dict p1) 'XStep 20)
      (table-insert! (dict p1) 'YStep 30)
      ;;
      |#
  
(define (with-pdf-resource-accum (self <pdf-device>) proc)
  (let* ((s (make <resource-accum-stream>
                  content: (open-output-string)
                  resources: (make-symbol-table)))
         (tmp (make <pdf-device>
                    pdf: (pdf self)
                    current-page: s
                    fontmap: (fontmap self)
                    well-known-objects: (well-known-objects self))))
    (proc tmp)
    (values (resources s)
            (close-output-port (content s)))))
    
(define-method startpage ((self <pdf-device>) #optional label)
  (let ((pg (+ 1 (current-page-num self)))
        (po (pdf-insert-page (pdf self))))
    (set-current-page-num! self pg)
    (set-current-page! self (make <pdf-page>
                                  fontmap: (make-object-table)
                                  content: (open-output-string)
                                  resources: (dict-lookup po 'Resources)
                                  pdf-object: po))
    ;(patterns (pdf self) (pdf-object (current-page self)))
    (values)))

(define-method endpage ((self <pdf-device>))
  (page-append-contents (pdf-object (current-page self))
                        (alloc-object
                         (pdf self)
                         (output-to-pdf-stream
                          (pdf self)
                          (lambda (port)
                            (write-string port 
                                          (close-output-port 
                                           (content (current-page self))))))))
  (set-current-page! self #f))

(define-method with-gstate-saved ((self <pdf-device>) thunk)
  (let* (((p <pdf-page>) (current-page self))
         (saved-pt (current-point p)))
    (format (content p) "q\n")
    (thunk)
    (format (content p) "Q\n")
    (set-current-point! p saved-pt)
    (values)))

(define-method moveto ((self <pdf-device>) pt)
  (set-current-point! (current-page self) pt)
  (if (not (text-mode? (current-page self)))
      (format (content (current-page self)) "~d ~d m\n" 
              (psq (x pt))
              (psq (y pt)))))

(define-method rlineto ((self <pdf-device>) (dpt <size>))
  (lineto self (point+ (current-point (current-page self)) dpt)))
 
(define-method lineto ((self <pdf-device>) pt)
  (set-current-point! (current-page self) pt)
  (format (content (current-page self)) "~d ~d l\n" 
          (psq (x pt))
          (psq (y pt))))

(define-method curveto ((self <pdf-device>) (h1 <point>)
					    (h2 <point>) 
                                            (pt <point>))
  (format (content (current-page self))
	  "~d ~d  ~d ~d  ~d ~d c\n"
	  (psq (x h1)) (psq (y h1))
	  (psq (x h2)) (psq (y h2))
	  (psq (x pt)) (psq (y pt)))
  (set-current-point! (current-page self) pt)
  (values))

(define-method stroke ((self <pdf-device>))
  (set-current-point! (current-page self) #f)
  (format (content (current-page self)) "S\n"))

(define-method fill ((self <pdf-device>))
  (set-current-point! (current-page self) #f)
  (format (content (current-page self)) "f\n"))

(define-method setcolor ((self <pdf-device>) (color <vector>) #optional mode)
  (if (or (not mode) (eq? mode 'stroke))
      (format (content (current-page self)) "~a\n" (vector-ref color 0)))
  (if (or (not mode) (eq? mode 'fill))
      (format (content (current-page self)) "~a\n" (vector-ref color 1))))

(define (ensure-resource-entry (dict <symbol-table>)
                               (type <symbol>)
                               (name <symbol>)
                               creator-thunk)
  (let ((tbl (table-lookup dict type)))
    (if (not tbl)
        (begin
          (set! tbl (make-symbol-table))
          (table-insert! dict type tbl)))
    (if (not (table-lookup tbl name))
        (table-insert! tbl name (creator-thunk)))
    (values)))

(define-method use-resource ((self <pdf-device>) type name)
  (case type
    ((pattern)
     (ensure-resource-entry (resources (current-page self))
                            'Pattern
                            name
                            (lambda ()
                              (get-well-known-object self name))))
    ((color-space)
     (ensure-resource-entry (resources (current-page self))
                            'ColorSpace
                            name
                            (lambda ()
                              (get-well-known-object self name))))
    (else
     (error "Unknown resource type: ~s name: ~s" type name))))

(define-method uncolored-pattern-color ((self <pdf-device>) pat use)
  (let* ((name (case (car use)
                 ((k) 'Csp.cmyk)
                 ((rg) 'Csp.rgb)
                 ((g) 'Csp.g)
                 (else
                  (error "~s: Unknown postscript color space: ~s"
                         self
                         (car use))))))
    (use-resource self 'color-space name)
    ;;
    (vector (~ "/~a cs ~a /~a scn" name (cadr use) pat)
            (~ "/~a CS ~a /~a SCN" name (cadr use) pat))))

(define-method device-color ((self <pdf-device>) spec)
  (let ((u (postscript-color spec self)))
    (if (vector? u)
        u
        (vector
         (~ "~a ~a" (cadr u) (car u))
         (~ "~a ~a" (cadr u) (list->string
                              (map char-upcase
                                   (string->list
                                    (symbol->string (car u))))))))))


(define-method rectfill ((self <pdf-device>) (r <rect>))
  (format (content (current-page self)) "~d ~d ~d ~d re f\n"
          (psq (origin-x r))
          (psq (origin-y r))
          (psq (size-width r))
          (psq (size-height r))))

(define-method rectstroke ((self <pdf-device>) (r <rect>))
  (format (content (current-page self)) "~d ~d ~d ~d re S\n"
          (psq (origin-x r))
          (psq (origin-y r))
          (psq (size-width r))
          (psq (size-height r))))

(define-method setlinewidth ((self <pdf-device>) width)
  (format (content (current-page self)) "~d w\n" (psq width)))

(define-method concat ((self <pdf-device>) tm)
  (let ((v (vector-map psq (matrix tm))))
    (format (content (current-page self))
	    "~d ~d ~d ~d ~d ~d cm\n"
	    (vector-ref v 0)
	    (vector-ref v 1)
	    (vector-ref v 2)
	    (vector-ref v 3)
	    (vector-ref v 4)
	    (vector-ref v 5))
    (values)))

(define-method setlinecap ((self <pdf-device>) type)
  (format (content (current-page self)) "~d J\n"
          (case type
            ((butt) 0)
            ((round) 1)
            ((square) 2)
            (else
             (error "setlinecap: unknown line cap type ~s" type)))))

(define-method setlinejoin ((self <pdf-device>) type)
  (format (content (current-page self)) "~d j\n"
          (case type
            ((miter) 0)
            ((round) 1)
            ((bevel) 2)
            (else
             (error "setlinejoin: unknown line join type: ~s" type)))))

(define-method setmiterlimit ((self <pdf-device>) limit)
  (format (content (current-page self)) "~d M\n" (psq limit)))

(define-method scale ((self <pdf-device>) (sx <real>) (sy <real>))
  (concat self (scale $identity-transform (make-point sx sy))))

(define-method rotate ((self <pdf-device>) (angle <real>))
  (concat self (rotate $identity-transform angle)))

(define-method translate ((self <pdf-device>) (delta <point>))
  (concat self (translate $identity-transform delta)))
                          

(define-method starttext ((self <pdf-device>))
  (format (content (current-page self)) "BT ")
  (set-text-linebase! (current-page self) (make-size 0 0))
  (set-text-mode?! (current-page self) #t))

(define-method endtext ((self <pdf-device>))
  (format (content (current-page self)) "ET\n")
  (set-text-mode?! (current-page self) #f))

(define-method currentfont ((self <pdf-device>))
  (current-font (current-page self)))

(define-method setfont ((self <pdf-device>) font)
  (let ((name (get-local-font-name self (font-shape font))))
    (ensure-resource-entry (resources (current-page self))
                           'Font 
                           name
                           (lambda ()
                             (get-font-object self font name)))
    (set-current-font! (current-page self) font)
    (format (content (current-page self)) "  /~a ~d Tf\n"
            name
            (font-size font))))

(define (pdf-flush-Td (self <pdf-device>) pg port)
  (if (current-point pg)
      (let ((r (point+ (current-point pg) (text-linebase pg))))
        (format port " ~d ~d Td" (psq (x r)) (psq (y r)))
        (set-text-linebase! pg (point- $zero-point (current-point pg)))))
  (set-current-point! pg #f))

(define-method show ((self <pdf-device>) str)
  (assert (text-mode? (current-page self)))
  (let* ((pg (current-page self))
         (port (content pg)))
    (pdf-flush-Td self pg port)
    (write-string port " ")
    (write-ps-string port str)
    (write-string port " Tj\n")
    (values)))

(define-method xshow ((self <pdf-device>) (str <string>) (x-widths <list>))
  (assert (text-mode? (current-page self)))
  ;; this is not especially efficient...
  (let ((items (make-dequeue)))
    (for-each (lambda (i)
                (dequeue-push-back! items (string (string-ref str i)))
                (dequeue-push-back! items (list-ref x-widths i))
                (values))
              (range (string-length str)))
    (dxshow self (vector->list (dequeue-state items)))))

;;; (dxshow /graphics-device/ /items/)
;;;
;;; Render a set of items, each of which is either a string
;;; or a number.  A string means to render the text, while
;;; a number means to adjust the in-line position (the x coordinate
;;; for horizontal writing systems) by subtracting the given
;;; value expressed in 1/1000s of a unit of text space.
;;;
;;; A unit in text space is essentially before transformation
;;; due to the text size (T_s) and the stretch (T_h).  Although
;;; note that Acrobat 4.0 and earlier would not perform dxshow
;;; with adjustments properly if T_h was not it's default value 
;;; of 100

(define-method dxshow ((self <pdf-device>) (items <list>))
  (assert (text-mode? (current-page self)))
  (let* ((pg (current-page self))
         (port (content pg)))
    (pdf-flush-Td self pg port)
    ;;
    (write-string port "[")
    (for-each
     (lambda (i)
       (if (string? i)
           (write-ps-string port i)
           (format port " ~d" (psq i))))
     items)
    ;;
    (format port "] TJ\n")))
  

(define (get-local-font-name (self <pdf-device>) shape)
  (or (table-lookup (fontmap self) shape)
      (let ((n (assign-local-font-name self shape)))
        (table-insert! (fontmap self) shape n)
        n)))

(define (assign-local-font-name (self <pdf-device>) shape)
  (let ((id (table-size (fontmap self)))
        (pn (assoc (font-family shape) '(("Times" . "ti")
                                         ("Helvetica" . "he")
                                         ("Minion" . "mo"))))
        (qn (assoc (font-style shape) '(("Roman" . "r")
                                        ("Regular" . "re")
                                        ("Bold" . "b")
                                        ("Italic" . "i")
                                        ("BoldItalic" . "bi")
                                        ("Condensed" . "c")
                                        ("Condensed Italic" . "ci")
                                        ("Condensed Bold" . "cb")
                                        ("Condensed Bold Italic" . "cbi")))))
    ;;
    (if (and pn qn)
        (string->symbol (string-append "Fs." (cdr pn) (cdr qn)))
        (string->symbol (~ "Fu.~d" (+ id 1))))))

(define (get-font-object (self <pdf-device>) font name)
  (or (table-lookup (well-known-objects self) name)
      (let ((f (load-font-object self font)))
        (table-insert! (well-known-objects self) name f)
        f)))

;;;  Create the given font object in the PDF file

(define (load-font-object (self <pdf-device>) font)
  (if (get-property (font-shape font) 'standard-pdf #f)
      (let ((f (alloc-dict (pdf self))))
        (dict-insert! f 'Type 'Font)
        (dict-insert! f 'Subtype 'Type1)
        (dict-insert! f 'BaseFont (string->symbol (postscript-name font)))
        f)
      (pdf-import-font (pdf self) (import-font-for-pdf (font-shape font)))))
