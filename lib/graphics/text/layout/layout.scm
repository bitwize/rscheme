;;;
;;;  At last, the top-level driver
;;;

(define (layout-text dev 
                     #key 
                     node-list        ; an SXML node list
                     (frame type: <rect>))

  (let* ((p (upper-left frame))
         (style (make-text-style))
         (accum '()))
    ;;
    (define (add-para item)
      (set! accum 
            (break-text-into-vlist accum
                                   (sxml:children item)
                                   style)))
    ;;
    (for-each
     (lambda (item)
       (if (sxml:element? item)
           (case (car item)
             ;;
             ((para)
              ;;
              ;;
              (with-line-width
               (size-width frame)
               (lambda ()
                 (if (string=? (xpath-str item "@align") "right")
                     (with-config-parameter
                      'left-skip
                      (sxml->glue '(skip (@ (stretch "1fill"))))
                      (lambda ()
                        (add-para item)))
                     (add-para item)))))
             ;;
             ((skip)
              (set! accum (append accum
                                   (list (make-vskip (sxml->glue item))))))
             ;;
             (else
              (error "Can't handle: ~s" item)))))
     node-list)
    ;;
    (let ((s (vlist-set accum (size-height frame))))
      (y (render-vlist dev
                       (upper-left frame)
                       accum
                       s)))))
