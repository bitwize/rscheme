,(use util.xml
      util.xpath)

(define-class <buffer-text-run> (<object>)
  (x type: <fixnum>)
  (width type: <fixnum>)
  (text type: <string>)
  (style type: <fixnum>)        ; an index into the [compiled-]style-index
  (hyper init-value: #f))

(define-class <editor-line> (<object>)
  (y type: <fixnum>)
  (height type: <fixnum>)
  (depth type: <fixnum>)
  (runs type: <vector>))

(define-syntax (line-first-run self)
  (vector-ref (runs self) 0))

(define-syntax (line-last-run self)
  (let (((runs <vector>) (runs self)))
    (vector-ref runs (sub1 (vector-length runs)))))

(define-syntax (line-previous-run self run)
  (let (((runs <vector>) (runs self)))
    (vector-ref runs (sub1 (vmemq run runs)))))


(define (line-limit-x (self <editor-line>))
  ;; NOTE, we now ensure that each line has at least one run
  (let (((last <buffer-text-run>) (line-last-run self)))
    (+ (x last) (width last))))

(define-class <editor-buffer> (<object>)
  (input-data)
  (lines type: <vector>)
  (height type: <fixnum>)
  (style-index type: <vector>)
  (compiled-style-index type: <vector>)
  (margin type: <fixnum>))

(define $sample-text
  '(div
    (span (@ (class "blur"))
          "<---------|---------|---------|---------|"
          "---------|---------|---------|------->")
    "\n\n"
    "(" (span (@ (class "keyword")) "define")
    " ("
    (span (@ (class "defining")) "foo")
    " x)              "
    (span (@ (class "comment")) "; compose a list")
    "\n"
    "  (list x x))\n"
    "\n"
    (span (@ (class "comment")) "#|\n    Now, how was that?\n|#\n\n")
    "(" (span (@ (class "keyword")) "define")
    " "
    (span (@ (class "defining")) "p0")
    " "
    "'"
    (span (@ (class "editable")) "#,(point 50 120)")
    ")\n"))

(define (class->style class)
  (let ((a (assq (string->symbol class)
                 '((default default-face)
                   (comment comment-face)
                   (defining defining-face)
                   (editable hyperlink-face)
                   (keyword keyword-face)
                   (blur blur-face)))))
    (if a
        (cadr a)
        'default-face)))

(define (delete-run (buf <editor-buffer>)
                    (line <editor-line>) 
                    (run <buffer-text-run>))
  (delete-run* buf line run (vmemq run (runs line))))
  
(define (reformat-run (buf <editor-buffer>)
                      (line <editor-line>) 
                      (run <buffer-text-run>))
  (reformat-run* buf line run (vector-ref (style-index buf) (style run))
                 (vmemq run (runs line))))

(define (delete-run* (buf <editor-buffer>)
                     (line <editor-line>) 
                     (run <buffer-text-run>)
                     k)
  (bind ((dw (- (width run)))
         (lruns (runs line))
         (before (subvector (runs line) 0 k))
         (after (subvector (runs line) (+ k 1))))
    ;;
    (set-runs! line (vector-append before after))
    ;;
    (let loop ((i 0))
      (if (< i (vector-length after))
          (let ((r (vector-ref after i)))
            (set-x! r (+ (x r) dw))
            (loop (+ i 1)))
          (values dw (vector-ref before (- k 1)))))))

(define (reformat-run* (buf <editor-buffer>)
                       (line <editor-line>) 
                       (run <buffer-text-run>)
                       style
                       k)
  (bind ((overall-width overall-ascent overall-descent
                        (text-extents (style-compile 
                                       (get-style-attribute style 'font))
                                      (text run)))
         (dw (- overall-width (width run)))
         (lruns (runs line)))
    ;;
    (set-width! run overall-width)
    ;; XXX need to handle a change to the line's height or depth
    (let loop ((i (+ k 1)))
      (if (< i (vector-length lruns))
          (let ((r (vector-ref lruns i)))
            (set-x! r (+ (x r) dw))
            (loop (+ i 1)))
          dw))))

(define (process-text text margin)
  (let ((accum (open-output-string))
        (style (class->style "default"))
        (all-lines (make-dequeue))
        (all-runs (make-dequeue))
        (style-tab (make-symbol-table))
        (style-list (make-dequeue))
        (min-h (get-style-attribute 'default-face 'line-height))
        (min-d (get-style-attribute 'default-face 'line-depth))
        (h 0)
        (d 0)
        (xptr margin)
        (yptr margin))
    ;;
    (define (make-run style str)
      (let ((k (table-lookup style-tab style)))
        (if (not k)
            (begin
              (dequeue-push-back! style-list style)
              (set! k (table-size style-tab))
              (table-insert! style-tab style k)))
        ;;
        (bind ((overall-width overall-ascent overall-descent
                              (if (= (string-length str) 0)
                                  (values 0 0 0)
                                  (text-extents 
                                   (style-compile 
                                    (get-style-attribute style 'font))
                                   str)))
              (x xptr))
          (set! xptr (+ xptr overall-width))
          (set! h (max h overall-ascent))
          (set! d (max d overall-descent))
          (make <buffer-text-run>
                text: str
                x: x
                width: overall-width
                style: k))))
    ;;
    ;; the `force?' flag is so that we can make sure each buffer-line
    ;; has at least one run.  This greatly simplifies program logic
    ;; in other places, e.g., letting us know what style to apply to
    ;; any new text inserted on that line
    ;;
    (define (flush-run #optional force?)
      (let ((a (get-output-string accum)))
        (if (or force? (> (string-length a) 0))
            (begin
              (dequeue-push-back! all-runs (make-run style a))
              (set! accum (open-output-string))))))
    ;;
    (define (flush-line)
      (flush-run #t)
      (dequeue-push-back! all-lines (make <editor-line>
                                          runs: (dequeue-state all-runs)
                                          height: (max h min-h)
                                          depth: (max d min-d)
                                          y: (+ yptr (max h min-h))))
      (set! all-runs (make-dequeue))
      (set! yptr (+ yptr (max h min-h) (max d min-d)))
      (set! xptr margin)
      (set! d 0)
      (set! h 0))
    ;;
    (define (emit ch s)
      (if (not (eq? s style))
          (begin
            (flush-run)
            (set! style s)))
      (if (char=? ch #\newline)
          (flush-line)
          (write-char ch accum)))
    ;;
    (define (scan style node)
      (if (string? node)
          (for-each (lambda (ch)
                      (emit ch style))
                    (string->list node))
          (case (car node)
            ((span)
             (let ((s (class->style (xpath-str node "@class"))))
               (for-each (lambda (sub)
                           (scan s sub))
                         (sxml:children node))))
            ((div)
             (for-each (lambda (n)
                         (scan style n))
                       (sxml:children node)))
            (else
             (error "bad node in input: ~s" node)))))
    ;;
    (scan 'default-face text)
    ;;
    (flush-line)
    (make <editor-buffer>
          input-data: text
          lines: (dequeue-state all-lines)
          margin: margin
          height: yptr
          style-index: (dequeue-state style-list)
          compiled-style-index: (vector-map style-compile
                                            (dequeue-state style-list)))))

(define (reprocess-buffer! (self <editor-buffer>))
  (let ((b (process-text (input-data self) (margin self))))
    (set-lines! self (lines b))
    (set-height! self (height b))
    (set-style-index! self (style-index b))
    (set-compiled-style-index! self (compiled-style-index b))))


(define (char-index-in-run (owner <editor-buffer>)
                           (run <buffer-text-run>) 
                           dx)
  (let ((str (text run))
        (fnt (style-compile (get-style-attribute (vector-ref 
                                                  (style-index owner)
                                                  (style run))
                                                 'font)))
        (n (string-length (text run))))
    ;;
    (let loop ((x 0)
               (dx dx)
               (i 0))
      (if (or (<= dx 0) (>= i n))
          (values i x)
          (let ((w (char-width fnt (string-ref str i))))
            (if (<= dx (/ w 2))
                (values i x)
                (loop (+ x w) (- dx w) (+ i 1))))))))
