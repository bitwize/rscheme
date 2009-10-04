

;;;

(define *test-para*
  (make <para>
        style: 'body-para-style
        content: (list (make <text-run>
                             style: 'body-char-style
                             content: "Aya aya, this is a test of the ")
                       (make <text-run>
                             style: 'emphasis-char-style
                             content: "emergency")
                       (make <text-run>
                             style: 'body-char-style
                             content: " finance broadcast sys\0tem."))))
                       
                             
;;;

(define (open-inline-stream (p <para>) offset)
  (let ((s #|(open-inline-stream* p)|#
         (list->iterator (paragraph->item-list p))))
    (let loop ((i offset))
      (if (= i 0)
          s
          (let ((item (s)))
            (if item
                (loop (- i 1))
                #f))))))

(define (open-inline-stream* (p <para>))
  (proc->iterator
   (lambda (emit)
     (iterate-over-content p emit))))

(define-method iterate-over-content ((self <para>) emit)
  (let ((keep-nl? (and (memq (query-style (style self) 'lines)
                             '(asis
                               asis-wrap
                               asis-truncate))
                       #t)))
    (for-each
     (lambda (content-item)
       (iterate-over-content content-item emit keep-nl?))
     (content self))))

(define-method iterate-over-content ((self <text-run>) emit keep-nl?)
  (let ((n (string-length (content self)))
        (s (style self)))
    (format #t "ITERATE OVER ~s OF STYLE ~s\n" self s)
    (if (get-property self 'revisionflag #f)
        (format #t ">>> IS REVISED ~s\n" self))
    (let loop ((i 0))
      (if (< i n)
          (let ((ch (string-ref (content self) i)))
            (case ch
              ((#\-)
               (emit '/)
               (emit 'char s #\-))
              ((#\tab)
               (emit 'break 'tab))
              ((#\newline)
               ;; actually, it looks like we're handling most line
               ;; breaking options here...
               (if keep-nl?
                   (emit 'break 'line)
                   (begin
                     (emit '/)
                     (emit 'char s #\space))))
              ((#\nul)  ; XXX this should be Unicode's discretionary hyphen
               (emit '-))
              ((#\space)
               (emit '/)
               (emit 'char s #\space))
              (else
               (emit 'char s ch)))
            (loop (+ i 1)))))))

(define *t*
  (make <para>
        style: 'body-para-style
        content: (list (make <text-run>
                             style: 'style1
                             content: "Foo")
                       (make <text-run>
                             style: 'style2
                             content: "\0bar"))))

;;;


(define-class <cursor> (<object>)
  x
  y
  bbox)

(define-class <breakpoint-history> (<object>)
  (x type: <vector>)
  (index type: <vector>))

(define (make-breakpoint-history)
  (make <breakpoint-history>
        x: (vector #f #f #f #f)
        index: (vector #f #f #f #f)))

(define (adjust-breakpoint (self <breakpoint-history>) type x-posn k)
  (let ((new-x (clone (x self)))
        (new-index (clone (index self))))
    ;;
    (for-each (lambda (clr)
                (vector-set! new-x clr #f)
                (vector-set! new-index clr #f))
              (case type
                ((0) '(1 2 3))
                ((1) '(2 3))
                ((2) '(3))
                ((3) '())))
    ;;
    (vector-set! new-x type x-posn)
    (vector-set! new-index type k)
    (make <breakpoint-history>
          x: new-x
          index: new-index)))

(define-class <baseline> (<object>)
  vlist
  y
  width)

