
(define-constant $sxml-indention
  (list->vector
   (map (lambda (n)
          (string-append "\n" (make-string (* n 2) #\space)))
        (range 40))))

(define (sxml:strip-whitespace sxml #optional (preserve default: '()))
  ;;
  (define (recseq l)
    (if (null? l)
        '()
        (if (string? (car l))
            (let ((t (trim-whitespace (car l))))
              (if (string=? t "")
                  (recseq (cdr l))
                  (cons t (recseq (cdr l)))))
            (cons (rec (car l)) (recseq (cdr l))))))
  ;;
  (define (rec n)
    (if (sxml:element? n)
        (if (memq (car n) preserve)
            n
            (if (and (pair? (cdr n))
                     (pair? (cadr n))
                     (eq? (caadr n) '@))
                (cons* (car n) (cadr n) (recseq (cddr n)))
                (cons (car n) (recseq (cdr n)))))
        n))
  ;;
  (rec sxml))

      
                                 
(define (pretty-printify-xml sxml)
  ;;
  (define (indention n)
    (if (< n 40)
        (vector-ref $sxml-indention n)
        (string-append "\n" (make-string (* n 2) #\space))))
  ;;
  (define (indent n d)
    (let ((gi (car n)))
      ;; XXX special hack: never indent <pre>...</pre>
      (if (eq? gi 'pre)
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
            (if (pair? body)
                (let ((was-elem? #t))
                  ;;
                  (for-each
                   (lambda (x)
                     (if (sxml:element? x)
                         (begin
                           (if was-elem?
                               (dequeue-push-back! q (indention d)))
                           (dequeue-push-back! q (indent 
                                                  x
                                                  (+ d 1)))
                           (set! was-elem? #t))
                         (begin
                           (dequeue-push-back! q x)
                           (set! was-elem? #f))))
                   body)
                  ;;
                  (if was-elem?
                      (dequeue-push-back! q (indention (- d 1))))))
            (vector->list (dequeue-state q))))))
  ;;
  (if (sxml:element? sxml)
      (indent sxml 1)
      sxml))

