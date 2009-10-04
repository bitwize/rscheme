
(define-class <simple-page-sequence> (<object>)
  initial-page-styles
  repeat-page-styles
  blank-page-style)             ;; non-#f if an even number of pages are required

;;;
;;;

(define *part-page-sequence*
  (make <simple-page-sequence>
        initial-page-styles: '(part-first-page)
        repeat-page-styles: '()
        blank-page-style: 'intentionally-blank-page-body)) ; XXX part verso

(define *chapter-page-sequence*
  (make <simple-page-sequence>
        initial-page-styles: '(chapter-start-page)
        repeat-page-styles: '(chapter-left-page chapter-right-page)
        blank-page-style: 'intentionally-blank-page-body))

(define *landscape-page-sequence*
  (make <simple-page-sequence>
        initial-page-styles: '()
        repeat-page-styles: '(landscape-table-page)
        blank-page-style: 'intentionally-blank-page-body))

(define *page-format-definitions* (make-string-ci-table))


(table-insert! *page-format-definitions* "default" *chapter-page-sequence*)
(table-insert! *page-format-definitions* "landscape" *landscape-page-sequence*)


(define-method iterator ((self <simple-page-sequence>))
  (proc->iterator
   (lambda (emit)
     (for-each emit (initial-page-styles self))
     (let ((rpt (repeat-page-styles self)))
       (if (null? rpt)
           (error "~s ran out of pages" self)
           (let loop ()
             (for-each emit rpt)
             (loop)))))))

;;;

(define (format-number num fmt)
  ;;
  (define (alpha str)
    (if (zero? num)
        "-"
        (let ((rep (quotient (- num 1) 26))
              (ch (string-ref str (modulo (- num 1) 26))))
          (make-string (+ 1 rep) ch))))
  ;;
  (case fmt
    ((arabic) (format #f "~d" num))
    ((alpha) (alpha "abcdefghijklmnopqrstuvwxyz"))
    ((Alpha) (alpha "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
    ((roman) (if (zero? num)
                 "0"
                 (string-append
                  (vector-ref '#("" "x" "xx" "xxx")
                              (quotient num 10))
                  (vector-ref '#("" "i" "ii" "iii" "iv" "v" "vi" 
                                "vii" "viii" "ix")
                              (modulo num 10)))))
    ((Roman) (list->string
              (map char-upcase
                   (string->list
                    (format-number num 'roman)))))
    (else (number->string num))))


#|
(define (renumber-pages! (self <document>))
  ;;  this procedure needs to go through and take into account
  ;;  where page numbers get restarted
  )
|#
