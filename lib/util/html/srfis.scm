#! /u/donovan/bin/rsf -script

(load "esis.scm")

,(use rs.sys.threads.manager
      util.xpath
      util.xml
      calendar
      syscalls)

;;;==================================================
;;;
;;;  Fixes to erroneous information
;;;

(define (manual-fixup doc)
  doc)

#|
(define (manual-fixup doc)
  (let ((x (xpath () doc "srfi[@name='46']/attr/final")))
    (if (null? x)
        (error "could not find SRFI-46 attr/final to fix it up")
        (set-cdr! (car x) '((final (single-date (@ (date "2005-02-28"))))))))
  ;;
  doc)
        

(define (fixup)
  (manual-fixup
   (call-with-input-file "srfi-index.xml" port->sxml)))
|#

;;;
;;; this information is not available online as far as I can tell...
;;;

(define *supersedes* (make-string-table))
(table-insert! *supersedes* "47" "63")

;;;
;;;===================================================

(define (process->string cmd)
  (let ((p (open-input-process cmd)))
    (let ((d (port->string p)))
      (close-input-port p)
      d)))

(define (load-srfi-html uri)
  (format #t "=== GET ~a ===\n" uri)
  (let ((s (process->string
            (~ "curl http://srfi.schemers.org/~a" uri))))
    (call-with-output-file
        "/tmp/srfi.tmp"
      (lambda (port)
        (write-string port (string-join ">" (string-split s "/>")))))
    (load-html "/tmp/srfi.tmp" uri)))

#|
(load-srfi-html "final-srfis.html")
(load-srfi-html "draft-srfis.html")
|#


(define (parse-srfi-html html)
  (select
   (lambda (n)
     (pair? (xpath () n "a")))
   (xpath () html "//li")))

(define srfi-attribute-pattern (reg-expr->proc
                                '(seq
                                  (save (or "Draft"
                                            "Revised"
                                            "Withdrawn"
                                            "Received"
                                            "Final"))
                                  #\:
                                  (+ space)
                                  (save (* any)))))

(define strip-leading-delim (reg-expr->proc
                             '(prefix (* (or #\: space)))))

(define (normalize-spaces str)
  (string-join " " (string-split str (reg-expr->proc '(+ space)))))

(define (trim-trailing-whitespace str)
  (if (and (> (string-length str) 0)
           (char-whitespace? (string-ref str (- (string-length str) 1))))
      (trim-trailing-whitespace (substring str 0 (- (string-length str) 1)))
      str))

(define (normalize-spaces-rec n)
  (if (sxml:text? n)
      (normalize-spaces n)
      (if (pair? n)
          (map normalize-spaces-rec n)
          n)))

(define (pick-out-title node)
  (define (after-a l)
    (if (and (sxml:element? (car l))
             (eq? (caar l) 'a))
        (after-a (cdr l))
        l))
  ;;
  (define (until-ul l)
    (if (and (sxml:element? (car l))
             (eq? (caar l) 'ul))
        '()
        (cons (car l)
              (until-ul (cdr l)))))
  ;;
  (define (strip-left l)
    (if (sxml:text? (car l))
        (bind ((s e (strip-leading-delim (car l)))
               (stripped (substring (car l) e)))
          (if (string=? stripped "")
              (strip-left (cdr l))
              (cons stripped (cdr l))))
        l))
  ;;
  (define (strip-right-rev l)
    (if (sxml:text? (car l))
        (bind ((stripped (trim-trailing-whitespace (car l))))
          (if (string=? stripped "")
              (strip-right-rev (cdr l))
              (cons stripped (cdr l))))
        l))
  ;;
  (normalize-spaces-rec
   (reverse
    (strip-right-rev
     (reverse
      (strip-left
       (until-ul (after-a (sxml:children node)))))))))

(define srfi-name-pattern (reg-expr->proc '(seq "SRFI" (+ space)
                                                (save (+ digit)))))

(define (parse-srfi-entry attr node)
  (let ((a '()))
    ;;
    ;; pick up the first-level text, but not lower
    (bind ((title (pick-out-title node))
           (href (xpath-str node "a/@href"))
           (label (xpath-str node "a"))
           (s e name (srfi-name-pattern label))
           (name (or name label))
           (superseded (cond
                        ((table-lookup *supersedes* name)
                         => (lambda (s)
                              `((superseded-by (@ (name ,s))))))
                        (else 
                         '()))))
                           
      ;;
      (for-each
       (lambda (li)
         (bind ((s e key data (srfi-attribute-pattern
                               (trim-whitespace (xpath:node->string li)))))
           (if s
               (set! a (cons (list (downcased-symbol key)
                                   (parse-srfi-attr-value 
                                    (trim-whitespace data))) a)))))
       (xpath () node "ul/li"))
      ;;
      `(srfi (@ (name ,name) ,@attr)
        (title ,@title)
        (link (@ (href ,href)))
        ,@superseded
        (attr
         ,@(reverse! a))))))
  
(define date-range-pattern (reg-expr->proc
                            '(entire
                              (seq
                               (save (seq digit digit digit digit))
                               #\/
                               (save (seq (? digit) digit))
                               #\/
                               (save (seq (? digit) digit))
                               (* space)
                               #\-
                               (* space)
                               (save (seq digit digit digit digit))
                               #\/
                               (save (seq (? digit) digit))
                               #\/
                               (save (seq (? digit) digit))))))



(define single-date-pattern (reg-expr->proc
                            '(entire
                              (seq
                               (save (seq digit digit digit digit))
                               #\/
                               (save (seq (? digit) digit))
                               #\/
                               (save (seq (? digit) digit))))))

(define-macro (pattern-case . cases)
  (let ((tmp0 '%result0)
        (tmp+ '%result+))
    (define (rec cases)
      (if (null? cases)
          '(values)
          (let ((expr (caar cases))
                (vars (cadar cases))
                (body (cddar cases)))
            (if (eq? expr 'else)
                (if (null? vars)
                    `(begin ,@body)
                    (error "vars list must be empty on else clause"))
                `(bind ((,tmp0 #rest ,tmp+ ,expr))
                   (if ,tmp0
                       (bind ((,(car vars) ,tmp0)
                              (,@(cdr vars) (list->values ,tmp+)))
                         ,@body)
                       ,(rec (cdr cases))))))))
    ;;
    (rec cases)))

(define (parse-srfi-attr-value str)
  (pattern-case
   ;;
   ((date-range-pattern str)
    (s e y0 m0 d0 y1 m1 d1)
    `(date-range (@ (from ,(to-string (ymd->date 
                                       (string->number y0)
                                       (string->number m0)
                                       (string->number d0))))
                    (to ,(to-string (ymd->date 
                                     (string->number y1)
                                     (string->number m1)
                                     (string->number d1)))))))
   ;;
   ((single-date-pattern str)
    (s e y0 m0 d0)
    `(single-date (@ (date ,(to-string (ymd->date 
                                        (string->number y0)
                                        (string->number m0)
                                        (string->number d0)))))))
   ;;
   (else
    ()
    `(comment ,str))))
                        
                                           
(define (import-srfi-toc-as-xml)
  `(srfi-index
    (@ (loadtime ,(time->string (time) "%Y-%m-%dT%H:%M:%SZ" #f)))
    ,@(map (lambda (n)
             (parse-srfi-entry '((status "final")) n))
           (parse-srfi-html (load-srfi-html "final-srfis.html")))
    ,@(map (lambda (n)
             (parse-srfi-entry '((status "draft")) n))
           (parse-srfi-html (load-srfi-html "draft-srfis.html")))
    ,@(map (lambda (n)
             (parse-srfi-entry '((status "withdrawn")) n))
           (parse-srfi-html (load-srfi-html "withdrawn-srfis.html")))))

(define (export-srfi-index ix)
  (call-with-output-file
      "srfi-index.xml"
    (lambda (p)
      (format p "<?xml version='1.0'?>\n")
      (write-sxml (pretty-printify-xml ix) p)
      (newline p))))

(define (main args)
  (export-srfi-index 
   (manual-fixup
    (import-srfi-toc-as-xml))))


#|
                            - run -
./srfis.scm
                             - or -
rsf -q srfis.scm \
    -e '(export-srfi-index (import-srfi-toc-as-xml))' -exit

                          - and then -

cp srfi-index.xml ~/p/rscheme-web/state/rs-meta/srfi-index.xml
(cd ~/p/rscheme-web ; ./tools/push.sh)
|#
