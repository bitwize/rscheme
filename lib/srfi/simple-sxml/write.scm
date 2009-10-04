;(cond-expand (srfi-23))
;(cond-expand (srfi-6))


(define (get-element-attrs elem)
  (if (and (pair? (cdr elem))
           (pair? (cadr elem))
           (eq? (caadr elem) '@))
      (cdadr elem)
      '()))

(define (get-element-post-attrs elem)
  (if (and (pair? (cdr elem))
           (pair? (cadr elem))
           (eq? (caadr elem) '@))
      (cddr elem)
      (cdr elem)))

(define *sxml-attr-escapes*
  '((#\' . "&apos;")
    (#\" . "&quot;")
    (#\< . "&lt;")
    (#\> . "&gt;")
    (#\& . "&amp;")))
    
(define *sxml-text-escapes*
  '((#\< . "&lt;")
    (#\> . "&gt;")
    (#\& . "&amp;")))
    
(define (write-sxml-attr-value value port)
  ;; this is crude; always use the same delimiter and escape everything
  ;; a better implementation would be cleverer
  (write-char #\" port)
  (for-each
   (lambda (ch)
     (let ((a (assq ch *sxml-attr-escapes*)))
       (if a
           (display (cdr a) port)
           (write-char ch port))))
   (string->list value))
  (write-char #\" port))

(define (write-sxml-attr attr port)
  (write-char #\space port)
  (display (car attr) port)
  (write-char #\= port)
  (write-sxml-attr-value (cadr attr) port))

(define (write-sxml-entity-ref item port)
  (write-char #\& port)
  (display (cadr item) port)
  (write-char #\; port))

(define (write-sxml-child-of-element item port)
  (cond
   ((string? item) (write-sxml-text item port))
   ((pair? item) 
    (case (car item)
      ((*COMMENT*) (write-sxml-comment item port))
      ((*ENTITY*) (write-sxml-entity-ref item port))
      (else (write-sxml-element item port))))
   (else (error "Bad stuff in SXML"))))

(define (write-sxml-text text port)
  (let loop ((i 0))
    (if (< i (string-length text))
        (let* ((ch (string-ref text i))
               (alt (assq ch *sxml-text-escapes*)))
          (if alt
              (display (cdr alt) port)
              (write-char ch port))
          (loop (+ i 1))))))

(define (write-sxml-element elem port)
  (if (not (symbol? (car elem)))
      (error "element tag is not a symbol!"))
  (if (not (list? elem))
      (error "element structure is not a list"))
  ;;
  (write-char #\< port)
  (display (car elem) port)
  (let ((postattr (get-element-post-attrs elem))
        (attrs (get-element-attrs elem)))
    ;;
    (if (not (list? attrs))
        (error "element attrs are not a list"))
    ;;
    (for-each 
     (lambda (attr)
       (write-sxml-attr attr port))
     attrs)
    ;;

    (if (null? postattr)
        ;; it's empty
        (display "/>" port)
        ;; otherwise, it has some stuff
        (begin
          (write-char #\> port)
          (for-each
           (lambda (child)
             (write-sxml-child-of-element child port))
           postattr)
          (write-char #\< port)
          (write-char #\/ port)
          (display (car elem) port)
          (write-char #\> port)))))


(define (write-sxml-pi pi port)
  
(define (write-sxml-decl decl port)
  (display port "<!")
  
     ((eq? (car self) '*XML*)
    ...)
   ((eq? (car self) '*DECL*)
    (format port "<! ~a" (cadr self))
    (if (pair? (cddr self))
        (let ((external-id (caddr self)))
          (case (car external-id)
            ((public)
             (format port "PUBLIC ~s ~s" 
                     (c
          ((system)

(define (write-sxml-annotations annots port)
  ;; need to study this some more...
  (for-each
   (lambda (annot)
     (case (car annot)
       ((*XML*)
        (display "<?xml " port)
        (display (cadr annot) port)
        (display "?>" port)
        (newline port))
       ((*DECL*)
        (display "<!DOCTYPE " port)
        (display (cadr annot) port)
        (if (pair? (cddr annot)) 
        (display " PUBLIC " port)
        (write (caddr annot) port)
        (if (pair? (cdddr annot))
            (begin
              (display " " port)
              (write (cadddr annot) port)))
        (display ">" port)
        (newline port))))
   annots))

(define (write-sxml-comment comment port)
  (display "<!--" port)
  ;; no escaping of comment body; better not have a "--" in it
  (display (cadr comment) port)
  (display "-->" port))

(define (write-sxml-doc doc port)
  (letrec ((preamble (lambda (l)
                       (case (caar l)
                         ((*COMMENT*)
                          (write-sxml-comment (car l) port)
                          (preamble (cdr l)))
                         ((@)
                          (write-sxml-annotations (cdar l) port)
                          (preamble (cdr l)))
                         ((*DECL*)
                          (write-sxml-decl (car l) port)
                          (preamble (cdr l)))
                         ((*PI*)
                          (write-sxml-pi (car l) port)
                          (preamble (cdr l)))
                         (else
                          (write-sxml-element (car l) port))))))
    (if xml-decl?
        (format port "<?xml version='1.0' encoding='UTF-8'?>\n"))
    (preamble (cdr doc))))


(define (write-sxml* sxml port)
  ;; If we are handed a bare Element, wrap it into a plain Document
  (if (not (eq? (car sxml) '*TOP*))
      (write-sxml-doc (list '*TOP* sxml) port)
      (write-sxml-doc sxml port)))

(define (write-shtml-as-html sxml . opt)
  ...)

(define (write-sxml sxml . opt)
  (case (length opt)
    ((0) (write-sxml* sxml (current-output-port)))
    ((1) (write-sxml* sxml (car opt)))
    (else (error "wrong # args"))))

(define (sxml->string sxml)
  (let ((p (open-output-string)))
    (write-sxml sxml p)
    (get-output-string p)))

;;;
;;;
