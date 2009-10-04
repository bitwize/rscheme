(define (base64-gzip-encode x)
  (with-module
      rs.net.pem
    (with-module
        rs.sys.compression
      (let ((o (open-output-string))
            (s (pem-encode (compress x))))
        (let loop ((i 0))
          (if (< i (string-length s))
              (let ((j (min (string-length s) (+ i 72))))
                (write-string o (substring s i j))
                (write-string o "\n")
                (loop j))
              (close-output-port o)))))))

(define (pretty-printify-xml sxml)
  (define (indention n)
    (if (< n 8)
        (vector-ref '#("\n"
                       "\n  "
                       "\n    "
                       "\n      "
                       "\n        "
                       "\n          "
                       "\n            "
                       "\n              ")
                    n)
        (string-append "\n" (make-string (* n 2) #\space))))
  ;;
  (define (indent n d)
    (let ((gi (car n)))
      (if (eq? gi 'pre) ;; XXX special hack: never indent <pre>...</pre>
          n
          (bind ((attr body (if (and (pair? (cdr n))
                                     (pair? (cadr n))
                                     (eq? (caadr n) '@))
                                (values (cadr n) (cddr n))
                                (values #f (cdr n))))
                 (q (make-dequeue)))
            ;;
            (dequeue-push-back! q gi)
            (if attr
                (dequeue-push-back! q attr))
            (let ((last-is-elem? (for-each
                                  (lambda (x)
                                    (if (sxml:element? x)
                                        (begin
                                          (dequeue-push-back! q (indention d))
                                          (dequeue-push-back! q (indent 
                                                                 x
                                                                 (+ d 1)))
                                          #t)
                                        (begin
                                          (dequeue-push-back! q x)
                                          #f)))
                                  body)))
              (if last-is-elem?
                  (dequeue-push-back! q (indention (- d 1))))
              (vector->list (dequeue-state q)))))))
  ;;
  (indent sxml 1))

(define (find-first predicate lst)
  (let loop ((i lst))
    (if (null? i)
        #f
        (if (predicate (car i))
            (car i)
            (loop (cdr i))))))

(define (list-slice lst start end)
  (vector->list (subvector (list->vector lst) start end)))

(define (node->hash (self <node>))
  (persistent-hash self))

(define (make-node-table)
  (make-table eq? node->hash))

(set! $app-classes (vector-append $app-classes
                                  (vector node->hash)))


(define-method write-object ((self <comment-request>) port)
  (if (comment self)
      (format port "#[<comment-request> ~a, ~#@*20s]"
              (id (base-request self))
              (comment self))
      (format port "#[<comment-request> ~a]"
              (id (base-request self)))))

(define-method write-object ((self <code-review>) port)
  (format port "#[<code-review> ~a, ~a]"
          (id (base-request self))
          (name (group self))))

(define-method write-object ((self <integration-request>) port)
  (format port "#[<integration-request> ~a, ~a, ~s]"
          (id (base-request self))
          (name (file-system self))
          (map name (snapshots self))))

