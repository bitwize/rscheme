
(define (print-ibis-block-html self depth #optional flag)
  (print-ibis-block-html* self depth flag '(long)))

(define (print-ibis-block-html-short self depth #optional flag)
  (print-ibis-block-html* self depth flag '(short)))

(define (print-ibis-block-html-for-browser self depth flag)
  (print-ibis-block-html* self depth flag '(short edit-links)))

(define (disposition-flag (self <item>))
  (case (get-property self 'disposition #f)
    ((taken) "*")
    ((declined) "/")
    ((future) ".")
    (else "")))

(define (print-ibis-block-html* self depth flag mode)
  (let* ((tag (display-tag self))
         (title (title self))
         (dpy-id (display-id self))
         (content (content self))
         (label (if (memq 'long mode)
                    (format #f "<b>~a <a href=\"~a?edit=1\">~a</a></b>: "
                            (display-type self)
                            (global-id self)
                            (id self))
                    (format #f "<b>~a~a</b> (<a href=\"~a\">~a</a>) " 
                            tag 
                            (disposition-flag self)
                            (global-id self)
                            dpy-id)))
         (label-w (item-sequence-width (parse-markup label))))
    (if (and content
             (not (string=? content title)))
        (begin
          (render-para (string-append "<b>" title "</b>")
                       (* depth 3)
                       label)
          (if flag
              (if (not (eq? flag 'title))
                  (render-para (format #f "See <a href=\"~a\">~a</a>"
                                       (global-id self)
                                       dpy-id)
                               (+ (* depth 3) label-w) ""))
              (render-para content (+ (* depth 3) label-w) "")))
        (begin
          (render-para title (* depth 3) label)
          (if (and flag (not (eq? flag 'title)))
              (render-para (format #f "See <a href=\"~a\">~a</a>"
                                   (global-id self)
                                   dpy-id)
                           (+ (* depth 3) label-w) ""))))
    (if (memq 'edit-links mode)
        (let ((gi (global-id self)))
          (format #t "~a[<a href=\"~a?edit=1\">Edit</a>"
                  (make-string (+ (* depth 3) label-w) #\space)
                  gi)
          (cond
           ((instance? self <issue>)
            (format #t " <a href=\"~a?fup=respond\">#</a>" gi))
           ((instance? self <position>)
            (format #t " <a href=\"~a?fup=support\">+</a>" gi)
            (format #t " <a href=\"~a?fup=object-to\">-</a>" gi)))
          (if (instance? self <issue>)
              (format #t " <a href=\"~a?fup=specialize\">?</a>" gi)
              (format #t " <a href=\"~a?fup=question\">?</a>" gi))
          (format #t "]\n")))
    (newline)))
  


(define (print-ibis-block-text self depth #optional flag)
  (let* ((tag (string-append (display-tag self)
                             (disposition-flag self)))
         (title (title self))
         (dpy-id (display-id self))
         (pre (string-append (make-string (* depth 2)) tag " "))
         (subseq (make-string (+ (* depth 2)
                                 (string-length tag)
                                 1)))
         (para (format #f (if flag
                              "~a  See <~a>\n"
                              "~a <~a>\n")
                       title
                       dpy-id)))
    (let loop ((lines (flow-para para (- 78 (string-length pre))))
               (first? #t))
      (if (pair? lines)
          (begin
            (format #t "~a~a\n" (if first? pre subseq) (car lines))
            (loop (cdr lines) #f))))))

  
(define (id<? a b)
  (< (id a) (id b)))


(define-method display-tag ((self <affirmative>)) "+")
(define-method display-tag ((self <negative>)) "-")
(define-method display-tag ((self <position>)) "#")
(define-method display-tag ((self <issue>)) "?")

;;;  `filter' is a procedure which takes a node and the current issue
;;;  depth, and decides what to do at that point.  The valid return
;;;  values are:
;;;
;;;     #f    => don't include the node or its children
;;;     #t    => include the node and its children
;;;     'link => include the node in summary mode, but not its children
;;;     'if   => include the node only if it has children that are included
;;;
;;;  If filter is #t, then the behavior is as if #t is always returned

(define (flatten-ibis-tree tree)
  (let ((q (make-dequeue)))
    (define (traverse n)
      (dequeue-push-back! q n)
      (for-each traverse (cddr n)))
    (traverse tree)
    (vector->list (dequeue-state q))))

(define (query-ibis-tree (root <item>)
                         #key (depth default: 1)
                         (filter default: #t))
  ;;
  (letrec ((process (lambda (n d)
                      #|
                      (format #t "P [~d] ~s (~s)\n" d n (if (eq? filter #t)
                                                            #t
                                                            (filter n d)))
                      |#
                      (case (if (eq? filter #t)
                                #t
                                (filter n d))
                        ((#f) 
                         #f)
                        ((#t)
                         (cons* n '() (children n d)))
                        ((if)
                         (let ((c (children n d)))
                           (if (null? c)
                               #f
                               (cons* n '() c))))
                        ((link)
                         (list n '(link))))))
           (children (lambda (n d)
                      (if (or (eq? depth #t)
                              (< d depth))
                          (select
                           identity
                           (map (lambda (sub)
                                  (if (instance? sub <issue>)
                                      (process sub (+ d 1))
                                      (process sub d)))
                                (append
                                 (sort (child-items n) id<?)
                                 (sort (sub-issues n) id<?))))
                          '()))))
    (cons* root '() (children root 0))))

;;;     undecided?      ==> keep undecided nodes
;;;     future?         ==> interpret future disposition as taken
;;;                         (else, interpret as undecided)
;;;     moot?           ==> keep declined nodes, but not their children
;;;                         (else, reject declined nodes)

(define (make-prune-filter undecided? future? moot?)
  (let ((if-undecided undecided?)
        (if-declined (if moot? 'link #f))
        (if-taken #t)
        (if-future '#undef))
    (set! if-future (if future? 
                        if-taken
                        if-undecided))
    ;
    (lambda ((self <item>) depth)
      (if (instance? self <position>)
          (case (get-property self 'disposition #f)
            ((taken) if-taken)
            ((future) if-future)
            ((declined) if-declined)
            (else if-undecided))
          #t))))

(define (print-as-text node depth annot)
  (if (memq 'link annot)
      (if (instance? node <issue>)
          (print-ibis-block-text node depth #t)
          (print-ibis-block-text node depth 'title))
      (print-ibis-block-text node depth)))

(define (print-as-html-for-browser node depth annot)
  (if (memq 'link annot)
      (if (instance? node <issue>)
          (print-ibis-block-html-for-browser node depth #t)
          (print-ibis-block-html-for-browser node depth 'title))
      (print-ibis-block-html-for-browser node depth #f)))

(define (print-as-html node depth annot)
  (if (memq 'link annot)
      (if (instance? node <issue>)
          (print-ibis-block-html-short node depth #t)
          (print-ibis-block-html-short node depth 'title))
      (print-ibis-block-html-short node depth)))

(define (render-tree-query-result result print)
  (letrec ((loop (lambda (n d)
                   (print (car n) d (cadr n))
                   (for-each (lambda (ch)
                               (loop ch (+ d 1)))
                             (cddr n)))))
    (loop result 0)))
                 

(define-method print-ibis-tree ((self <item>) 
                                depth 
                                #key
                                (issue-depth default: 1)
                                (print-block default: print-ibis-block-text))
  ;;
  (define (print-sub-issue-link sub)
    (print-block sub (+ depth 1) #t))
  ;;
  (print-block self depth #f)
  (for-each
   (lambda (sub)
     (print-ibis-tree sub (+ depth 1) 
                      issue-depth: issue-depth
                      print-block: print-block))
   (sort (child-items self) id<?))
  ;;
  (for-each
   (lambda (sub)
     (if (= issue-depth 1)
         (print-sub-issue-link sub)
         (print-ibis-tree sub (+ depth 1) 
                          issue-depth: (- issue-depth 1)
                          print-block: print-block)))
   (sort (query-relation-pick 'questions 'issue item: self) id<?))
  ;;
  (for-each
   (lambda (sub)
     (if (= issue-depth 1)
         (print-sub-issue-link sub)
         (print-ibis-tree sub (+ depth 1) 
                          issue-depth: (- issue-depth 1)
                          print-block: print-block)))
   (sort (query-relation-pick 'specializes 'fine coarse: self) id<?)))

(define-method display-type ((self <note>)) "Note")
(define-method display-type ((self <issue>)) "Issue")
(define-method display-type ((self <theme>)) "Theme")
(define-method display-type ((self <position>)) "Position")
(define-method display-type ((self <affirmative>)) "Affirmative")
(define-method display-type ((self <negative>)) "Negative")

(define (print-details (self <item>))
  (format #t "-------------------- ~a ~a --------------------\n" 
          (display-type self)
          (global-id self))
  (render-para (title self) 8 "Title:   ")
  (newline)
  (format #t "      Created:   ~a\n" 
          (transaction-display-time (create-xaction self)))
  (format #t "Last modified:   ~a\n" 
          (transaction-display-time (modify-xaction self)))
  (if (delete-xaction self)
      (format #t "      Deleted:   ~a\n" 
              (transaction-display-time (delete-xaction self))))
  (format #t "   Originator:   ~a (~a)\n" 
          (fullname (originator self))
          (name (originator self)))
  (format #t "        Owner:   ~a (~a)\n" 
          (fullname (owner self))
          (name (owner self)))
  (let ((any #f))
    (define (report-property msg . args)
      (format #t "     Property:   ")
      (apply format #t msg args)
      (newline)
      (set! any #t))
    ;
    (let ((d (get-property self 'disposition #f)))
      (if d (report-property "disposition: ~a" d)))
    )
  ;
  (let ((brk "=========================================================\n"))
    (format #t "Content:\n")
    (display brk)
    (display (content self))
    (let ((s (content self)))
      (if (and (> (string-length s) 1)
               (not (char=? (string-ref s (- (string-length s) 1)) #\newline)))
          (newline)))
    ;
    (for-each print-details-note (notes self))))

(define (print-audit-note-contents (self <audit-note>))
  ;;
  (define (changed a what)
    ;(format #t "Changed ~a\n" what)
    (render-para (to-string (cadr a)) 0 (format #f "Old ~C: " what))
    (render-para (to-string (caddr a)) 0 (format #f "New ~C: " what)))
  ;;
  (let ((sep? #f))
    (for-each 
     (lambda (a)
       (if sep? (newline))
       (set! sep? #t)
       (case (car a)
         ((change-title) (changed a "title"))
         ((change-content) (changed a "description"))
         ((change-owner) (changed a "owner"))
         ((change-originator) (changed a "originator"))
         ((change-disposition) (changed a "disposition"))
         ((change-parent)
          (bind ((old-parents new-parents old-id new-id 
                              (list->values (cdr a))))
            (if (not (string=? old-id new-id))
                (format #t "\nThe id was changed from ~s to ~s\n"
                        old-id new-id))
            (format #t "\nThe parent was changed\n")
            (for-each
             (lambda (p)
               (render-para (string-append "(" (title p) ")")
                            0
                            (format #f "From ~a " (global-id p))))
             old-parents)
            (for-each
             (lambda (p)
               (render-para (string-append "(" (title p) ")")
                         2
                         (format #f "To ~a " (global-id p))))
             new-parents)))
         ((delete) (format #t "Deleted item\n"))
         ((recreate) (format #t "Recreated item\n"))
         (else
          (format #t "Event <~a>\n" (car a))
          (for-each
           (lambda (i arg)
             (format #t "  [~d] ~#@*50s\n" i arg))
           (range (length (cdr a)))
           (cdr a)))))
     (audit-entries self))))

(define-method print-details-note ((self <audit-note>))
  (format #t "\n====== Update /~d performed ~a by <~a> ======\n"
          (id self)
          (transaction-display-time (create-xaction self))
          (name (owner self)))
  (print-audit-note-contents self))

(define-method print-details-note ((self <note>))
  (format #t "\n====== Note /~d added ~a by <~a> ======\n"
          (id self)
          (transaction-display-time (create-xaction self))
          (name (owner self)))
  (if (string=? (title self) "untitled")
      (display (content self))
      (begin
        (render-para (title self) 0 "")
        (render-para (content self) 0 ""))))

(define (transaction-display-time x)
  (time->string (process-time x) "%Y-%m-%d %H:%M:%S"))
