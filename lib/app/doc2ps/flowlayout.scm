
;;;
;;;  Construct a request stream for placing lines which has the
;;;  effect of creating pages as necessary according to the
;;;  target <simple-page-sequence>

(define (get-page-style name)
  (case name
    ;((chapter-start-page) (chapter-start-page))
    ;((chapter-left-page) (chapter-left-page))
    ;((chapter-right-page) (chapter-right-page))
    ;((test-page-style) (test-page-style))
    ;((title-front-page) (title-front-page))
    (else
     (query-style name 'content))))

(define (placement-vskip placement-stream dy)
  (request placement-stream 'vskip dy))

(define (make-placement-stream (target <simple-page-sequence>) 
                               page-owner
                               #optional (flow-tag default: 'a))
  (let ((next-page-style (iterator target)))
    (make-request-stream
     (lambda (rs)
       (let loop ()
         (let* ((style (next-page-style))
                (page (make-page target style page-owner)))
           (format #t "PAGE STYLE ~s => ~s\n" style page)
           (iterate-over-flow (find-text-frames-for-flow-id
                               (get-page-style style)
                               flow-tag)
                              page
                              rs)
           (format #t "DONE WITH PAGE ~s\n" page)
           (loop)))))))

(define (find-text-frames-for-flow-id (lst <list>) flow-id)
  (select (lambda (item)
            (and (instance? item <text-frame>)
                 (eq? (flow item) flow-id)))
          lst))

(define-class <flow-layout-info> (<object>) :abstract)

(define-class <page-flow-layout-info> (<flow-layout-info>)
  page
  flow-layout)

(define-method add-vl-to-flow-layout! ((self <page-flow-layout-info>) 
                                       (vl <vlist>))
  (let ((pg (page (subframe (placement vl)))))
    (if (not (eq? pg (page self)))
        (new-page! self pg))
    (add-vl-to-flow-layout! (flow-layout self) vl)))

(define (new-page! (self <page-flow-layout-info>) new-page)
  (set-page! self new-page)
  (set-flow-layout! self (make <flow-layout>))
  (add-flow-layout-to-page! new-page (flow-layout self)))

(define-method layout-flow-member ((self <vskip>) pstream (fli <flow-layout-info>))
  (request pstream (height self) 'in-column)
  (values))

(define-method layout-flow-member ((self <vbreak>)
                                   pstream
                                   (fli <flow-layout-info>))
  ;(format #t "REQUESTING VBREAK: ~s\n" (section self))
  (let ((a (request pstream 'break (section self))))
    ;(format #t "SATISFIED WITH: ~s\n" a)
    (values)))
    

(define-method layout-flow-member ((self <flow-vbox>)
                                   pstream
                                   (fli <flow-layout-info>))
  (bind ((sf y (request pstream
                        (height self)
                        'in-column)))          ;; placement?
    (add-vl-to-flow-layout! fli
                            (make <vlist>
                                  placement: (make <placement>
                                                   subframe: sf
                                                   y: y)
                                  content: (list
                                            (make <vbox>
                                                  width: (size-width
                                                          (subframe-rect
                                                           sf))
                                                  x: 0  ; XXX use align?
                                                  height: (height self)
                                                  content: self))))))
      
(define-method layout-flow-member ((self <para>)
                                   pstream
                                   (fli <flow-layout-info>))
  (let* ((paralines (make-request-stream
                     (lambda (rs)
                       (insert-line-breaks2 self rs))))
         (vlists (layout-paragraph self
                                   paralines
                                   pstream)))
    (for-each 
     (lambda ((vl <vlist>))
       (add-vl-to-flow-layout! fli vl))
     vlists)))


(define (layout-flow-in-page-sequence (self <flow>) 
                                      (target <simple-page-sequence>)
                                      page-owner)
  (if (null? (content self))
      (values)          ; do nothing
      ;; otherwise, we'll need at least one page...
      (let ((pstream (make-placement-stream target page-owner (flow-tag self)))
            (current-fli (make <page-flow-layout-info>
                               page: #f
                               flow-layout: #f)))
        (let loop ((lst (content self)))
          (if (pair? lst)
              (begin
                (layout-flow-member (car lst)
                                    pstream
                                    current-fli)
                (loop (cdr lst))))))))

(define (gen-test-para k)
  (make <para>
        style: 'body-para-style
        content: (list (make <text-run>
                             style: 'body-char-style
                             content: (format #f "[~d] Whereupon we shall see the "
                                              k))
                       (make <text-run>
                             style: 'literal-char-style
                             content: "true light_of_the_day")
                       (make <text-run>
                             style: 'body-char-style
                             content: ".  So sayeth the folk, so sayeth my spork."))))

(define (gen-test-flow #key (length default: 10))
  (make <flow>
        content: (map gen-test-para (range length))))


#|
(layout-flow-in-page-sequence 
 (gen-test-flow length: 60) 
 *chapter-page-sequence*)
|#

#|.................

              (let* (((para <para>) (car lst))
                     (paralines (make-request-stream
                                 (lambda (rs)
                                   (insert-line-breaks
                                    para
                                    rs)
                                   #|
                                   (break-para-lines 
                                    para
                                    (open-inline-stream para 0)
                                    rs)
                                   |#)))
                     (vlists (layout-paragraph (car lst)
                                               paralines
                                               pstream)))
                (for-each (lambda ((vl <vlist>))
                            (let ((pg (page (subframe (placement vl)))))
                              (format #t "--- PARAGRAPH ~s PRODUCED ON ~s ---\n"
                                      para
                                      pg)
                              (print vl)
                              (if (not (eq? pg current-page))
                                  (begin
                                    (set! current-page pg)
                                    (set! current-flow-layout
                                          (make <flow-layout>))
                                    (add-flow-layout-to-page! 
                                     pg 
                                     current-flow-layout)))
                              (add-vl-to-flow-layout! current-flow-layout vl)))
                          vlists)
|#
