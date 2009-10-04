(define *all-pages* '())

(define-class <rendered-page> (<object>)
  page-ordinal-in-document
  page-ordinal-in-group
  in-document
  style
  content
  page-owner
  page-label)

(define (make-page (self <simple-page-sequence>) style owner)
  (let ((p (vector '<page> (+ 1 (length *all-pages*)) self style '() owner)))
    (set! *all-pages* (cons p *all-pages*))
    p))

(define-method page-owner ((p <vector>))
  (vector-ref p 5))

(define (reset-pages)
  (set! *all-pages* '()))

(define-method page-number ((p <vector>))
  (vector-ref p 1))

(define (add-flow-layout-to-page! (pg <vector>) (fl <flow-layout>))
  (vector-set! pg 4 (append (vector-ref pg 4) (list fl))))

(define (tprp #optional out)
  (let ((p (open-ps-device (or out "/tmp/out.ps") (length *all-pages*))))
    ;;
    (for-each
     (lambda (f)
       (add-prolog-font p (get-font-definition (get-afm f))))
     '("Minion-Condensed"
       "Minion-CondensedItalic"))
    ;;
    (for-each (lambda (page)
                (startpage p (page-number page))
                (fake-print-page page p)
                (endpage p))
              (reverse *all-pages*))
    (close-graphics-device p)))

(define (current-page dev)
  (get-property dev 'document-page))

(define-method in-document ((pg <vector>))
  *the-fake-document*)

(define (get-document-variable doc var)
  (cadr (assq var doc)))

(define-method page-label ((pg <vector>))
  (number->string (vector-ref pg 1)))

(define (current-page-label dev)
  (number->string (vector-ref (get-property dev 'document-page) 1)))

(define (fake-print-page page dev)
  (let* ((ps (vector-ref page 3))
         (page-group (get-page-style ps)))
    ;;
    (set-property! dev 'document-page page)
    ;;
    (render page-group dev)
    ;;
    (for-each
     (lambda ((fl <flow-layout>))
       (render fl dev))
     (vector-ref page 4))))

(define (tgsp pg)
  (reset *dev*)
  (fake-print-page (list-ref (reverse *all-pages*) pg) *dev*)
  (flush-output-port *dev*))


;;; reoder pages so they are in group-list order

#|
(define (reorder-pages group-list)
  (set! *all-pages*
        (reverse
         (apply 
          append
          (map (lambda (g)
                 (select (lambda (p)
                           (eq? (query-style (
|#                
  
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

