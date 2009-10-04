
(define-macro (debug-refentry . forms)
  '(values))


(define *refentry-language* 'default)

(define-generic-function bridge-scheme-synopsis)
(define-generic-function bridge-c-synopsis)

(define (bridge-synopsis-proc)
  (case *refentry-language*
    ((default scheme) bridge-scheme-synopsis)
    ((c) bridge-c-synopsis)
    (else (error "Unknown language for synopsis: '~s" *refentry-language*))))

(define-method bridge-display-stuff ((self <REFENTRY>) emit)
  (let ((lang (or (get-attribute-value self "role") "scheme")))
    ;;
    ;; note that REFENTRY's do not occur inside REFENTRY's, so
    ;; we're OK here...
    ;;
    (set! *refentry-language* (string->symbol (string-downcase lang)))
    ;;
    ;;(format #t "REFENTRY ~s\n" (xpath self "refnamediv" "refname" 'cdata))
    (bridge-refname-div-display (xpath self "refnamediv") emit)
    (for-each
     (lambda ((div <REFSYNOPSISDIV>))
       (for-each
        (lambda (s)
          (cond
           ((instance? s <FUNCSYNOPSIS>)
            (emit
             (make-paragraph-from-inline s
                                         'synopsis-style 
                                         (bridge-synopsis-proc))))
           ((instance? s <SYNOPSIS>)
            (emit
             (make-paragraph-from-inline s
                                         'synopsis-style
                                         (bridge-synopsis-proc))))
           ((instance? s <REFSECT2>)
            (bridge-refsect s emit))
           (else
            (debug-refentry
             (format #t "skipping: ~s\n" s)))))
        (content div)))
     (xpath* self "refsynopsisdiv"))
    ;;
    (bridge-display-stuff (select (lambda (c)
                                    (and (not (instance? c <REFSYNOPSISDIV>))
                                         (not (instance? c <REFNAMEDIV>))))
                                  (content self))
                          emit)))

(define-method bridge-display-stuff ((self <REFSECT1>) emit)
  (emit (make-paragraph-from-inline
         (list "\t" (xpath self "TITLE"))
         'refnamesect-title-style
         bridge-title-stuff))
  (bridge-display-stuff (content self) emit))

(define-method bridge-display-stuff ((self <REFSECTION>) emit)
  (emit (make-paragraph-from-inline
         (list "\t" (xpath self "TITLE"))
         'refnamesect-title-style
         bridge-title-stuff))
  (bridge-display-stuff (content self) emit))

(define-method bridge-display-stuff ((self <REFSECT2>) emit)
  (emit (make-paragraph-from-inline
         (xpath self "TITLE")
         'refnamesect-title2-style
         bridge-title-stuff))
  (bridge-display-stuff (content self) emit))

(define-method bridge-display-stuff ((self <FUNCSYNOPSIS>) emit)
  (emit
   (make-paragraph-from-inline self 'synopsis-style (bridge-synopsis-proc))))

(define-method bridge-refsect ((self <REFSECT2>) emit)
  (emit (make-paragraph-from-inline (list "\t" (xpath self "title"))
                                    'refnamesect-title-style
                                    bridge-title-stuff))
  (bridge-display-stuff (content self) emit))

(define (make-indented s dx)
  (override-style style
                  left-margin-first: (+ dx (query-style s 'left-margin-first))
                  left-margin: (+ dx (query-style s 'left-margin))))

(define-method bridge-display-stuff ((self <VARIABLELIST>) emit)
  (let ((vlist-indent 1in))
    (for-each
     (lambda ((e <VARLISTENTRY>))
       (let ((plist '())
             (terms (append
                     (cdr
                      (apply
                       append
                       (map (lambda ((s <string>))
                              (list
                               (make <text-run>
                                     style: 'listterm-char-style
                                     content: ", ")
                               (make <text-run>
                                     style: 'listterm-char-style
                                     content: s)))
                            (xpath* e "term" 'cdata))))
                     (list (make <text-run>
                                 style: 'listterm-char-style
                                 content: "\t")))))
         ;;
         (debug-refentry
          (format #t "TERMS:\n")
          (print terms))
         ;;
         (bridge-display-stuff (content (xpath e "listitem"))
                               (lambda (e)
                                 (set! plist (cons e plist))))
         ;;
         (let loop ((i 0)
                    (plist (reverse! plist)))
           (if (pair? plist)
               (let ((p (car plist)))
                 ;(format #t "VARLISTENTRY PARAGRAPH:\n")
                 ;(print p)
                 ;;
                 (if (eq? i 0)
                     (begin
                       (assert (instance? (car plist) <para>)) ; XXX so far
                       (emit
                        (make <para>
                              content: (append terms (content p))
                              style: (override-style
                                      (style p)
                                      left-margin: (+ vlist-indent
                                                      (query-style 
                                                       (style p)
                                                       'left-margin))
                                      tab-stops: (cons
                                                  `(position: (l ,vlist-indent)
                                                              align: left)
                                                  (query-style (style p)
                                                               'tab-stops))))))
                     (begin
                       (assert (instance? (car plist) <para>)) ; XXX so far
                       (emit
                        (make <para>
                              style: (make-indented (style p) 
                                                    vlist-indent)
                              content: (content p)))))
                 (loop (+ i 1) (cdr plist)))))))
     (select (lambda (x)
               (instance? x <VARLISTENTRY>))
             (content self)))))

(define-method bridge-refname-div-display ((self <REFNAMEDIV>) emit)
  (emit
   (make-paragraph-from-inline 
    (if (pair? (xpath* self "refclass" 'cdata))
        (list "\t"
              (xpath self "refname")
              "\t"
              (list :bold (xpath self "refclass" 'cdata)))
        (list "\t"
              (xpath self "refname")))
    'refnamediv-name-style
    bridge-refname-div))
  ;;
  (emit
   (make-paragraph-from-inline (xpath self "refpurpose")
                               'refnamediv-style
                               bridge-refname-div)))

(define-method bridge-refname-div ((self <list>) emit style)
  (bridge-inline-list self emit style bridge-refname-div))

(define-method bridge-refname-div ((self <string>) emit style)
  (emit (make <text-run>
              style: style
              content: self)))

(define-method bridge-refname-div ((self <QUOTE>) emit style)
  (emit (make <text-run>
              style: style
              content: "\252"))
  (bridge-refname-div (content self) emit style)
  (emit (make <text-run>
              style: style
              content: "\272")))

(define-method bridge-refname-div ((self <sgml-text>) emit style)
  (bridge-refname-div (content self) emit style))

(define-method bridge-refname-div ((self <PARAMETER>) emit style)
  (bridge-refname-div (content self) emit 'emphasis-char-style))

(define-method bridge-refname-div ((self <PHRASE>) emit style)
  ;; XXX segues in and doesn't come back to bridge-refname-div
  (bridge-inline-stuff (content self) emit style))

(define-method bridge-refname-div ((self <REFPURPOSE>) emit style)
  (bridge-refname-div (content self) emit style))

(define-method bridge-refname-div ((self <REFNAME>) emit style)
  (if (get-attribute-value self "id")
      (let ((a (make-anchor self
                            <BOOK>
                            (list 'id
                                  (get-attribute-value self "id")
                                  (plain-content self)))))
        (set-property! self 'id-anchor a)
        (emit a)))
  (bridge-refname-div (content self) emit 'prototype-char-style))


(define (emit-tr emit style (content <string>))
  (emit (make <text-run>
              style: style
              content: content)))


(define (get-refentry)
  (force *test-book*)
  (list-ref (xpath* dd '* "refentry") 58))

;;;

(define (trefentry)
  (force *test-book*)
  (let ((paras '()))
    ;;
    (bridge-display-stuff (get-refentry)
                          (lambda (p)
                            (set! paras (cons p paras))))
    ;;
    (set! paras (reverse! paras))
    (print paras)
    ;;
    (layout-flow-in-page-sequence
     (make <flow>
           content: (cons (make <vbreak>
                                section: 'frame)
                          paras))
     *chapter-page-sequence*
     #f)))

(load "refentry-scheme.scm")
(load "refentry-c.scm")
