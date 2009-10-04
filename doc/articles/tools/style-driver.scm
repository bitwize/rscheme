(define (skip-children n . skips)
  (select (lambda (c)
            (or (not (sxml:element? c))
                (not (memq (car c) skips))))
          (sxml:children n)))

(define-thread-var *stylesheet-mode* 'top)

(define-syntax (in-mode k . forms)
  (thread-let ((*stylesheet-mode* k))
    (begin . forms)))

(define *style-bindings* (table *self*))

;;;
;;;  a style processor should ALWAYS produce a list
;;;

(define (style-dispatch n)
  (let ((result (style-dispatch* n)))
    (assert (list? result))
    result))

(define (style-article-node (self <article-node>))
  (thread-let ((*current-article-node* self))
    (format #t " (styling ~s" (name self))
    (flush-output-port (current-output-port))
    (set-content! self (style-node-list (content self))))
  (format #t ")")
  (flush-output-port (current-output-port)))

(define (style-dispatch* n)
  ;;
  (define (call mode key)
    (let ((k (symbol-append mode "-style:" key)))
      (cond
       ((table-lookup *style-bindings* k)
        => (lambda (b)
             ((value b) n)))
       (else
        (let ((k (symbol-append "all-style:" key)))
          (cond
           ((table-lookup *style-bindings* k)
            => (lambda (b)
                 ((value b) n)))
           (else
            (error "No style binding for ~s (in mode ~s)" key mode))))))))
  ;;
  (cond
   ((instance? n <article-node>)
    (style-article-node n)
    ;; nothing goes in here, though..
    '())
   #|
   ((sxml:entityref? n)
    (style-dispatch-entity n))
   |#
   ;;
   ((sxml:element? n)
    (call *stylesheet-mode* (car n)))
   ;;
   ((sxml:text? n)
    (call *stylesheet-mode* "TEXT"))
   ;;
   ((sxml:comment? n)
    '())
   ;;
   (else
    (format #t "ignoring: ~s\n" n)
    '())))


(define (style-children n)
  (style-children-except n))

(define (interesting-children n)
  (select (lambda (s)
            (not (equal? s "")))
          (map (lambda (s)
                 (if (sxml:text? s)
                     (trim-whitespace s)
                     s))
               (sxml:children n))))

(define (style-children-first n)
  (style-node-list (list (car (interesting-children n)))))

(define (style-children-rest n)
  (style-node-list (cdr (interesting-children n))))

(define (style-children-except n . skip)
  (apply append (map style-dispatch (apply skip-children n skip))))

(define (style-node-list nl)
  (apply append (map style-dispatch nl)))

(define (english-join-html lst #optional (logic default: "and"))
  (case (length lst)
    ((1) lst)
    ((2) (list (car lst) " " logic " " (cadr lst)))
    ((0) '())
    (else
     (let loop ((l lst)
                (r '()))
       (if (null? (cdr l))
           (reverse! (cons* (car l) " " logic r))
           (loop (cdr l)
                 (cons* ", " (car l) r)))))))


(load "web-style.scm")
(load "web-refentry.scm")
(load "web-biblio.scm")
