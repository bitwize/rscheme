;;;
;;;  I'm not smart enough to do this the right way (which would
;;;  be some kind of flexible template-based numbering scheme),
;;;  so hard code for now:
;;;
;;;  Plus, we'll choose to number things globally within an article
;;;  (i.e., within a chapter)

(define (NUM node)
  (let ((k (ID node)))
    (and k (table-lookup (numbering-index *current-article*) k))))

(define (ARABIC-NUM node)
  (let ((l (NUM node)))
    (if l
        (string-join "." (map number->string (reverse l)))
        "?")))

(define (number-article-objects (self <article>))
  ;;
  (let ((nix (numbering-index self)))
    ;;
    (define (number-objects nl)
      (for-each
       (lambda (i node)
         (table-insert! nix (ID node) (list (+ i 1))))
       (range (length nl))
       nl))
    ;;
    (define (number-objects* above below . procs)
      (if (pair? procs)
          (let ((subs ((car procs) below)))
            (for-each
             (lambda (i node)
               (let ((l (cons (+ i 1) above)))
                 (table-insert! nix (ID node) l)
                 (apply number-objects* l node (cdr procs))))
             (range (length subs))
             subs))))
    ;;
    (number-objects*
     '()
     (document self)
     (lambda (n) (xpath () n "//sect1"))
     (lambda (n) (xpath () n "sect2"))
     (lambda (n) (xpath () n "sect3")))
    ;;
    (number-objects (xpath () (document self) "//figure"))
    (number-objects (xpath () (document self) "//table"))
    (number-objects (xpath () (document self) "//example"))))



