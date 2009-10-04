(define *all-pages* '())

(define-class <rendered-document> (<object>)
  (properties init-value: '#()))

(define-class <rendered-page> (<object>)
  (properties init-value: '#())
  (used-fonts init-value: '())
  in-document
  page-ordinal-in-document              ; 0 1 ...
  page-ordinal-in-group                 ; 0 1 ...
  style
  content
  page-owner
  page-label)

(define-method write-object ((self <rendered-page>) port)
  (format port "#[<rendered-page> ~a ~d ~s]"
          (machine-bits->string self)
          (page-ordinal-in-document self)
          (page-label self)))

(define *the-document* (make <rendered-document>))
                             
(define (make-page (self <simple-page-sequence>) pstyle owner)
  (let* ((peer-pages (select (lambda (p)
                               (eq? (query-style pstyle 'numbering-group)
                                    (query-style (style p) 'numbering-group)))
                             *all-pages*))
         (n (length peer-pages))
         (p (make <rendered-page>
                  properties: (vector 'page-sequence self)
                  page-ordinal-in-document: (length *all-pages*)
                  page-ordinal-in-group: n
                  in-document: *the-document*
                  style: pstyle
                  content: '()
                  page-owner: owner
                  page-label: (format-number (+ n 1)
                                             (query-style 
                                              pstyle 
                                              'numbering-format)))))
    (progress "(~a)" (page-label p))
    (set! *all-pages* (cons p *all-pages*))
    p))

(define (reset-pages)
  (set! *the-document* (make <rendered-document>))
  (set! *all-pages* '()))

(define-method page-number ((self <rendered-page>))
  (page-ordinal-in-document self))

#|
(define-method uses-font ((self <rendered-page>) (font <text-font-shape>))
  (if (not (memq font (used-fonts self)))
      (set-used-fonts! self (cons font (used-fonts self))))
  (values))
|#

(define (add-flow-layout-to-page! (self <rendered-page>) (fl <flow-layout>))
  (set-content! self (append (content self) (list fl))))

#|
(define (write-pdf out)
  (let ((p (open-pdf-device out (length *all-pages*))))
))
|#  
    
    
(define *use-minion* #t)

(define *pre-prp-hook* (lambda ()))

(define (render-watermark (dev <graphics-device>) (watermark <string>))
  (with-gstate-saved
   dev
   (lambda ()
     (moveto dev (make-point 30 18))
     (rotate dev 90)
     ;(setfont dev (get-text-font "Courier" "Regular" 6))
     (setfont dev (get-text-font "Helvetica" "Bold" 20))
     (setcolor dev (device-color dev '(cmyk 0.5 0.333 0 0)))
     (show dev watermark)
     )))

(define *document-watermark* #f)

(define (tprp #optional out)
  (*pre-prp-hook*)
  (let ((p (open-ps-device (or out "/tmp/out.ps") 
                           num-pages: (length *all-pages*))))
    ;;
    (set-binary-encoding-ok?! p #f)
    ;;
    (for-each
     (lambda (f)
       (include-font p (font-shape f)))
     ;; XXX figure out how to do this based on what font styles get used
     (if *use-minion*
         (list (get-text-font "Minion" "Condensed" 12)
               (get-text-font "Minion" "Condensed Italic" 12)
               (get-text-font "Minion" "Condensed Bold" 12)
               (get-text-font "Minion" "Condensed Bold Italic" 12)
               (get-text-font "Univers" "UltraCondensed" 12)
               (get-text-font "BriemMono" "Condensed" 12)
               (get-text-font "BriemMono" "Condensed Bold" 12))
         '()))
    ;;
    (for-each (lambda (self)
                (set-property! p 'document-page self)
                (my-start-page self p)
                ;;
                (cond
                 ((getenv "DOCUMENT_WATERMARK")
                  => (lambda (w)
                       (render-watermark p w)))
                 (*document-watermark*
                  (render-watermark p *document-watermark*)))
                ;;
                (fake-print-page self p)
                (endpage p)
                (remove-property! p 'document-page))
              (reverse *all-pages*))
    (close-graphics-device p)))

(define (current-page dev)
  (get-property dev 'document-page))

(define (current-page-owner dev)
  (page-owner (current-page dev)))

(define (get-document-variable doc var)
  (get-property doc var #f))

(define (current-page-label dev)
  (page-label (get-property dev 'document-page)))

(define-method my-start-page ((self <rendered-page>) dev)
  (let* ((ps (style self))
         (page-group (get-page-style ps)))
    ;;
    (bind ((o w h (get-style-attributes/m ps 
                                          '(orientation
                                            page-width
                                            page-height)))
           (opts '()))
      ;;
      (if (eq? o 'landscape)
          (set! opts (cons* 'orientation: 'landscape opts)))
      ;;
      (apply startpage dev (page-label self) opts)
      ;;
      (if (eq? o 'landscape)
          (begin
            (rotate dev 90)
            (translate dev (make-point 0 (- h))))))))
  
(define-method fake-print-page ((page <rendered-page>) dev)
  (render (get-page-style (style page)) dev)
  ;;
  (for-each
   (lambda ((fl <flow-layout>))
     (render fl dev))
   (content page))
  ;;
  (remove-property! dev 'document-page))

#|
(define (tgsp pg)
  (reset *dev*)
  (fake-print-page (list-ref (reverse *all-pages*) pg) *dev*)
  (flush-output-port *dev*))
|#


;;; reorder pages so they are in group-list order

(define (reorder-pages! group-list)
  (set! *all-pages*
        (reverse (reorder-pages (reverse *all-pages*) group-list))))

(define (reorder-pages plist group-list)
  ;;
  (define (select-numbering-group list g)
    (select (lambda (p)
              (eq? (query-style (style p) 'numbering-group) g))
            list))
  ;;
  (let* ((n (length plist))
         (l (apply 
             append
             (map (lambda (g)
                    (select-numbering-group plist g))
                  group-list)))
         (l2 (insert-blank-pages l)))
    (assert (= (length l) n))
    (for-each (lambda (p i)
                (set-page-ordinal-in-document! p i))
              l2
              (range (length l2)))
    ;;
    (for-each (lambda (g)
                (let ((l (select-numbering-group l2 g)))
                  (for-each (lambda (p i)
                              (set-page-ordinal-in-group! p i)
                              (set-page-label! p (format-number
                                                  (+ i 1)
                                                  (query-style
                                                   (style p)
                                                   'numbering-format))))
                            l
                            (range (length l)))))
              group-list)
    ;;
    l2))

(define (insert-blank-pages lst)
  (let loop ((i 0)
             (l lst)
             (r '()))
    (if (null? l)
        (reverse! r)
        (if (blank-page? (car l))       ; reinsert blanks
            (loop i (cdr l) r)
            (case (query-style (style (car l)) 'page-start)
              ((any) 
               (loop (+ i 1) (cdr l) (cons (car l) r)))
              ((recto 
                right) 
               (if (odd? i)
                   (loop (+ i 1) l (cons (make-blank-page (car r)) r))
                   (loop (+ i 1) (cdr l) (cons (car l) r))))
              ((verso
                left)
               (if (even? i)
                   (loop (+ i 1) l (cons (make-blank-page (car r)) r))
                   (loop (+ i 1) (cdr l) (cons (car l) r))))
              (else
               (error "unknown page-start: ~s" 
                      (query-style (style (car l)) 'page-start))))))))

(define (blank-page? p)
  #f)

(define (make-blank-page p)
  (let ((pseq (get-property p 'page-sequence)))
    (make-page pseq (blank-page-style pseq) #f)))
                                
  
#|  *** THIS IS THE REAL ONE ***
(define (make-page (self <simple-page-sequence>) style)
  (bind ((w h numf (get-style-attributes style
                                         'page-width
                                         'page-height
                                         'numbering-format))
         (doc (current-document))
         (page-rect (make-rect 0 0 w h))
         (pg (make <page>
                   properties: (vector 'style style)
                   in-document: (current-document)
                   name: (page-number->string
                          numf
                          (vector-length (document-pages doc)))
                   page-contents: (make <root-group>
                                        graphic-bounding-box: page-rect
                                        in-document: doc)
                   page-size: (size page-rect)
                   page-margins: page-rect)))
    ;; XXX NOTE there is currently no provision for adding pages
    ;;     anywhere but at the end of the document.  It should be
    ;;     added after the end of a sequence with the same
    ;;     <simple-page-sequence>
    (set-document-pages! doc
                         (vector-append (document-pages doc)
                                        (vector pg)))
    pg))
|#

