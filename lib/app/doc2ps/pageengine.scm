;;;


(define-class <page-engine> (<object>)
  page-continuation
  resumer
  (section-continuations init-value: '()))

(define (make-page-engine pattern)
  (let ((pe (make <page-engine>
                  page-continuation: #f
                  resumer: #f)))
    (call-with-current-continuation
     (lambda (return)
       (set-resumer! pe (call-with-current-continuation
                         (lambda (cc)
                           (set-page-continuation! pe cc)
                           (return pe))))
       (generator pattern pe)
       (error "no more pages to generate")))))

(define (section-break! pe section)
  (set-page-continuation! pe (cdr (assq section (section-continuations pe)))))

(define (get-next-page pe)
  (call-with-current-continuation
   (lambda (cc)
     ((page-continuation pe) cc))))

(define (next-page-is pe type)
  (set-resumer! pe (call-with-current-continuation
                    (lambda (cc)
                      (set-page-continuation! pe cc)
                      ((resumer pe) type)))))
     

(define (generator pattern self)
  (let ((group-name (car pattern)))
    (call-with-current-continuation
     (lambda (cc)
       ;; allow this group to be broken out of...
       (set-section-continuations! 
        self
        (cons (cons group-name cc) (section-continuations self)))
       ;; ok, now process this group
       (generator* (cdr pattern) self)))))

(define (generator* list pe)
  (let loop ((l list))
    (if (pair? l)
        (let ((i (car l)))
          (cond
           ((symbol? i)
            (next-page-is pe (car l)))
           ((and (pair? i)
                 (eq? (car i) '*))
            (let iloop ()
              (generator* (cdr i) pe)
              (iloop)))
           ((pair? i)
            (generator i pe))
           (else
            (error "page pattern huh? ~s" i)))
          (loop (cdr l))))))

(define *test-page-pattern*
  '(book
    (* (part
        part-start
        (intro (* part-intro))
        (* (chapter chapter-start
                    (* chapter-left chapter-right)))
        (* (appendix appendix-start
                     (* appendix-left appendix-right)))))
    (* (glossary glossary-start
                 (* glossary-left glossary-right)))
    (* (index index-start
              (* index-left index-right)))
    colophon))


