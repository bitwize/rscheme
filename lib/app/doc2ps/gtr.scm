(define-method generate-text-runs ((self <hlist>) stream)
  (let ((stream (or stream
                    (open-inline-stream (para self) (start-index self))))
        (need-hyph? (hyphenate? self)))
    ;;
    (define (build-run style accum)
      (let* ((fontsize (char-style-size style))
             (afm (char-style-afm style))
             (char-list (select char? accum))
             (str (list->string char-list))
             (kern (if (query-style style 'kerning?)
                       (append (string-x-deltas afm str) '(0))
                       (map (lambda (ch) 0) char-list))))
        (let loop ((k kern)
                   (cw (char-widths afm str))
                   (i accum)
                   (r '()))
          (if (null? i)
              (cons style (reverse! r))
              (if (char? (car i))
                  (loop (cdr k)
                        (cdr cw)
                        (cdr i)
                        (cons (list (car i) 
                                    (* fontsize (car cw))
                                    (* fontsize (car k)))
                              r))
                  (loop k cw (cdr i) (cons (car i) r)))))))
    ;;
    (let loop ((i 0)
               (style #f)
               (accum '())
               (aruns '()))
      (format #t "/// ~s ~s ~s ~s\n" i style accum aruns)
      (if (>= i (index-count self))
          (values (reverse! 
                   (cons (build-run style 
                                    (reverse! 
                                     (if need-hyph?
                                         (cons #\- accum)
                                         accum)))
                         aruns))
                  stream)
          (bind ((type info detail (stream)))
            (case type
              ((-)
               (if (and need-hyph?
                        (= (+ i 1) (index-count self)))
                   (begin
                     (set! need-hyph? #f)
                     (loop (+ i 1) style (cons #\- accum) aruns))
                   (loop (+ i 1) style accum aruns)))
              ((/)
               (loop (+ i 1) style accum aruns))
              ((space)
               ;; go through some trouble to try to use a space
               ;; character to represent the space
               (bind ((new-style info)
                      (strut glue (parse-space info detail))
                      (space-width (style-char-width new-style #\space))
                      (accum aruns (if (or (not style)
                                           (eq? style info))
                                       (values accum aruns)
                                       (if (null? accum)
                                           (values '() aruns)
                                           (values '() (cons 
                                                        (build-run
                                                         style
                                                         (reverse! accum))
                                                        aruns)))))
                      (accum+glue (if (zero? glue)
                                      accum
                                      (cons (list 'stretch glue) accum))))
                 (loop (+ i 1)
                       new-style
                       (if (> strut (* space-width 0.75))
                           (cons* #\space
                                  (list 'strut (- strut space-width))
                                  accum+glue)
                           (cons* (list 'strut strut) accum+glue))
                         aruns)))
              ((char)
               (if (or (not style)
                       (eq? style info))
                   (loop (+ i 1) 
                         (or style info)
                         (if (null? accum)
                             (cons detail '())
                             (let ((lig (and (query-style style 'kerning?)
                                             (assoc (string (car accum) detail)
                                                    *ligatures*))))
                               (if lig
                                   (if (null? (cdr accum))
                                       (cons (cadr lig) '())
                                       (cons (cadr lig) (cdr accum)))
                                   (cons* detail
                                          (if (char=? detail #\space)
                                              $iws
                                              $ics)
                                          accum))))
                         aruns)
                   (loop (+ i 1) 
                         info 
                         (cons detail '()) 
                         (cons (build-run style (reverse! (cons $ics accum)))
                               aruns))))
              (else
               (error "Unexpected ~s in hlist" type))))))))

