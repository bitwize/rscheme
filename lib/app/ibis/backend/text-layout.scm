,(use regex tables)

(define (flow-para text width #optional (first-indent default: 0))
  (define (close-line l)
    (string-join " " (reverse l)))

  (let loop ((words (string-split text (reg-expr->proc 'space)))
             (lines '())
             (accum '())
             (width-so-far first-indent))
    (if (null? words)
        (if (null? accum)
            (reverse lines)
            (reverse (cons (close-line accum) lines)))
        (if (null? accum)
            (loop (cdr words)
                  lines
                  (cons (car words) '())
                  (+ width-so-far (string-length (car words))))
            (let ((nw (+ (string-length (car words))
                         width-so-far
                         1)))
              (if (< nw width)
                (loop (cdr words)
                      lines
                      (cons (car words) accum)
                      nw)
                (loop words
                      (cons (close-line accum) lines)
                      '()
                      0)))))))

(define space-pattern (reg-expr->proc '(+ space)))

(define markup-special-pattern (reg-expr->proc '(or #\< #\&)))

(define markup-entity-pattern (reg-expr->proc '(seq
                                                #\&
                                                (save (+ (or alpha digit #\.)))
                                                #\;)))

(define markup-tag-pattern (reg-expr->proc '(seq
                                             #\<
                                             (save (seq (? #\/)
                                                        (+ alpha)))
                                             (* (not #\>))
                                             #\>)))
                                      
(define (parse-entity-reference text k)
  (bind ((s e ename (markup-entity-pattern text k)))
    (if ename
        (values (list '*entity (string->symbol ename))
                e)
        (values (list '*entity 'amp)
                (+ k 1)))))

(define (parse-markup-tag text k)
  (bind ((s e tag (markup-tag-pattern text k)))
    (values (list (string->symbol tag) (substring text s e))
            e)))

(define (parse-special-markup text k)
  (case (string-ref text k)
    ((#\<) (parse-markup-tag text k))
    ((#\&) (parse-entity-reference text k))))

(define (parse-string* str)
  (parse-string str split-on: #\space))

(define (parse-string str #key (split-on default: space-pattern))
  (let loop ((w (string-split str split-on))
             (r '()))
    (if (null? w)
        (reverse (cdr r))
        (if (string=? (car w) "")
            (loop (cdr w) (cons* 'space r))
            (loop (cdr w) (cons* 'space (car w) r))))))

(define (parse-markup* text)            ; preserve spaces
  (parse-markup text parse-string*))

;;; by default, collapse spaces

(define (parse-markup text 
                      #optional (parse-string default: parse-string))
  (let ((N (string-length text)))
    (let loop ((i 0)
               (l '()))
      (let ((n (markup-special-pattern text i)))
        (if n
            (bind ((tag next (parse-special-markup text n)))
              (if (= i n)
                  (loop next (cons tag l))
                  (loop next (append! (list tag)
                                      (reverse
                                       (parse-string (substring text i n)) )
                                      l))))
            (if (< i N)
                (loop N (append (reverse (parse-string (substring text i))) l))
                (reverse l)))))))
  
(define (collate-referenced-entities items)
  (let ((t (make-symbol-table)))
    (for-each (lambda (i)
                (if (and (pair? i)
                         (eq? (car i) '*entity))
                    (table-insert! t (cadr i) #t)))
              items)
    (key-sequence t)))

(define *local-entities* (make-symbol-table))

(define (expand-local-entities items)
  (if (any? (lambda (ent)
              (table-lookup *local-entities* ent))
            (collate-referenced-entities items))
      (let ((q (make-dequeue)))
        (for-each
         (lambda (i)
           (if (and (pair? i)
                    (eq? (car i) '*entity))
               (let ((e (table-lookup *local-entities* (cadr i))))
                 (if e
                     (for-each (lambda (expansion)
                                 (dequeue-push-back! q expansion)
                                 (values))
                               e)
                     (dequeue-push-back! q i)))
               (dequeue-push-back! q i))
           (values))
         items)
        (vector->list (dequeue-state q)))
      items))

(define (item-sequence-width items)
  (reduce (lambda (n i)
            (cond
             ((string? i)
              (+ n (string-length i)))
             ((eq? i 'space)
              (+ n 1))
             ((eq? (car i) '*entity)
              (let ((x (table-lookup *local-entities* (cadr i))))
                (if x
                    (+ n (item-sequence-width x))
                    (+ n 1))))                  ; a character entity...? 
             (else                              ; a markup tag
              n)))
          0 items))

(define (flow-para-with-markup items width 
                               #optional (first-indent default: 0)
                                         (width-proc default: string-length))
  (define (close-line l)
    (call-with-output-string
     (lambda (port)
       (for-each (lambda (i)
                   (cond
                    ((eq? i 'space)
                     (write-char #\space port))
                    ((pair? i)
                     (if (eq? (car i) '*entity)
                         (format port "&~a;" (cadr i))
                         (write-string port (cadr i))))
                    (else
                     (write-string port i))))
                 (reverse l)))))
  ;
  (let loop ((words items)
             (lines '())
             (accum '())
             (width-so-far first-indent))
    (if (null? words)
        (if (null? accum)
            (reverse lines)
            (reverse (cons (close-line accum) lines)))
        (if (null? accum)
            (loop (cdr words)
                  lines
                  (cons (car words) '())
                  (if (string? (car words))
                      (+ width-so-far (width-proc (car words)))
                      width-so-far))
            (let ((nw (if (string? (car words))
                          (+ (width-proc (car words)) width-so-far 1)
                          width-so-far)))
              (if (< nw width)
                  (loop (cdr words)
                        lines
                        (cons (car words) accum)
                        nw)
                  ; unpop until we find a space
                  (let unpops ((words words)
                               (accum accum))
                    (cond
                     ((null? accum)
                      (loop words
                            (cons (close-line accum) lines)
                            '()
                            0))
                     ((eq? (car accum) 'space)
                      (loop words
                            (cons (close-line (cdr accum)) lines)
                            '()
                            0))
                     (else
                      (unpops (cons (car accum) words)
                              (cdr accum)))))))))))

(define (render-para text indent label)
  (let* ((first (string-append (make-string indent #\space) label))
         (label-w (item-sequence-width (parse-markup* label)))
         (subseq (make-string (+ indent label-w) #\space)))
    (let loop ((l (flow-para-with-markup 
                   (expand-local-entities (parse-markup text))
                   (- 77 label-w indent)))
               (leader first))
      (if (pair? l)
          (begin
            (format #t "~a~a\n" leader (car l))
            (loop (cdr l) subseq))))))
  
;;

(table-insert! *local-entities* 'ie (parse-markup "<i>i.e.</i>"))
(table-insert! *local-entities* 'eg (parse-markup "<i>e.g.</i>"))
(table-insert! *local-entities* 'roadmap (parse-markup "<i>&lt;RMap&gt;</i>"))

;;
      
(define *samp*
  (string-append "Now is the time for all good men to come to "
                 "the aid of their <a href=\"blah blah blah\">country</a>, "
                 "regardless of whether "
                 "or not, &eg;, their country shall come to the aid of "
                 "them <i>or their friends</i>."))

(define (test-flow)
  (with-output-to-file
      "/tmp/out.html"
    (lambda ()
      (display "<body bgcolor=\"white\">\n")
      (display "<pre>\n")
      (render-para *samp* 2 "Position 1: ")
      (display "</pre>\n")
      (display "</body>\n"))))

  
#|

  Issue: xx
         xx'

    Position 1: xx
                xx'

      Sub-issue: xx
                 xx'

      For....: xx

      Against: xx
 
|#