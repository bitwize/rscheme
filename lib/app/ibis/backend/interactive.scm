(define-module rs.util.evalhelp ()
  (&module
   (import usual-inlines
           repl
           compiler
           mlink))
  ;;
  (define (define-in-envt envt name value)
    (bind! envt (make <top-level-var>
                      name: name
                      value: value)))
  ;;
  (define (make-top-level . imports)
    (let ((e (make-top-level-contour)))
      (use-module-in 'r4rs (get-module 'r4rs) e)
      (for-each (lambda (i)
                  (use-module-in (name i) i e))
                imports)
      e))
  ;;
  (define (build-private-module . bindings)
    (bind ((m e (make-module 'app.ibis.backend.private)))
      (set-module-exports! m (table e)) ;; export everything
      (for-each (lambda (b)
                  (bind! e b))
                bindings)
      m))
  ;;
  (&module
   (export eval-in-envt
           define-in-envt
           make-top-level
           build-private-module)))

,(use regex)

(define internal-id-pattern (reg-expr->proc '(seq
                                              (* space)
                                              (or #\<)
                                              (save (seq
                                                     digit
                                                     (* (or digit #\.))))
                                              (or #\>))))

(define tag-pattern (reg-expr->proc '(seq
                                      (* space)
                                      (or #\[ #\<)
                                      (save (seq
                                             (+ uppercase)
                                             #\.
                                             (+ (or alpha digit #\.))))
                                      (or #\] #\>))))

(define title-pattern (reg-expr->proc '(prefix 
                                        (seq
                                         (* space)
                                         #\(
                                         (* space)
                                         (save (+ (not #\))))
                                         #\)
                                         (* space)))))
                                               

(define (process-internal-id text)
  (bind ((s e tag (internal-id-pattern text)))
    (if s
        tag
        #f)))

(define (process-tag text)
  (bind ((s e tag (tag-pattern text)))
    (if s
        (values (substring text 0 s) tag)
        (values text #f))))

(define (process-title-and-content info)
  (if (and (pair? (cdr info))
           (string? (cadr info)))
      (values (car info) (cadr info))
      (bind ((text (car info))
             (s e title-part (title-pattern text)))
        (if s
            (values title-part (substring text e))
            (values text text)))))

(define (info->new-item class parent info)
  (bind ((t0 c0 (process-title-and-content info))
         (klist (if (pair? (cdr info))
                    (if (string? (cadr info))
                        (cddr info)
                        (cdr info))
                    '()))
         (t (process-tag t0))
         (c tag (process-tag c0))
         (item (if (subclass? class <issue>)
                   (make class       ; don't supply parent for them
                         title: t
                         content: c)
                   (make class
                         title: t
                         content: c
                         parent: parent))))
    (if tag
        (set-property! item 'tag tag))
    (if (memq 'disposition: klist)
        (set-property! item 'disposition (cadr (memq 'disposition: klist))))
    item))

(define (specialize item . info)
  (let* ((p (lookup-item item))
         (i (info->new-item <issue> p info)))
    (link-sub-issue p i)
    i))

(define (respond item . info)
  (let* (((p <issue>) (lookup-item item))
         ((i <position>) (info->new-item <position> p info)))
    (link-child p i)
    i))

(define (notate item . info)
  (let* ((p (lookup-item item))
         (p (if (instance? p <note>)
                (service-error 
                 531 
                 "Creating a note on a note is currently verboten")
                p))
         (i (info->new-item <note> p info)))
    (notify (owner p))
    (notify (originator p))
    (notify (component p) 'add-note)
    (notify-subject "Note Added to ~a ~a" (display-type p) (global-id p))
    (notify-print
     (lambda ()
       (format #t "A note has been added to ~a ~a by ~a.\n\n"
               (display-type p)
               (global-id p)
               (name (user (current-transaction))))
       (short-notify-info p)
       (newline)
       (format #t "The text of the note is:\n\n")
       (format #t "* ~a\n" (title i))
       (format #t "~a\n" (content i))))
    (establish-relation 'discusses i p)
    i))

(define (support item . info)
  (let* (((p <position>) (lookup-item item))
         (i (info->new-item <affirmative> p info)))
    (link-child p i)
    i))

(define (object-to item . info)
  (let* (((p <position>) (lookup-item item))
         (i (info->new-item <negative> p info)))
    (link-child p i)
    i))

(define (question item . info)
  (let* (((p <item>) (lookup-item item))
         (i (info->new-item <issue> p info)))
    (if (instance? p <issue>)
        (service-error 431 "Cannot question a question; specialize instead"))
    (link-sub-issue p i)
    i))
  
(define (edit item #key 
              (title default: #f) 
              (content default: #f)
              (owner default: #f)
              (originator default: #f)
              (domain default: #f))
  (let ((p (lookup-item item)))
    (edit* p 
           title 
           content
           (if owner (string->user* owner) #f)
           (if originator (string->user* originator) #f)
           (if domain (string->domain* domain) #f))))

(define (with-audit item proc)
  (let ((n (make <audit-note>
                 title: "audit"
                 content: ""
                 parent: item
                 audit-entries: '())))
    (establish-relation 'discusses n item)
    (proc (lambda ((action <symbol>) . info)
            (if (eq? action '?)
                n
                (set-audit-entries! n (cons (cons action info) 
                                            (audit-entries n))))))))

(define (edit* (item <item>) 
               new-title 
               new-content
               new-owner
               new-originator
               new-domain)
  ;;
  (if (and new-title (string=? (title item) new-title))
      (set! new-title #f))
  (if (and new-content (string=? (content item) new-content))
      (set! new-content #f))
  (if (and new-owner (eq? (owner item) new-owner))
      (set! new-owner #f))
  (if (and new-originator (eq? (originator item) new-originator))
      (set! new-owner #f))
  (if (and new-domain (eq? (component item) new-domain))
      (set! new-domain #f))
  ;;
  (if new-domain
      (service-error 532 "Setting domain is not yet supported"))
  ;;
  (if (or new-title
          new-content
          new-owner
          new-originator
          new-domain)
      (with-audit
       item
       (lambda (audit)
         (notify (owner item))
         (notify (originator item))
         (notify (component item) 'modify-item)
         (notify-subject "~a ~a Modified" (display-type item) (global-id item))
         ;;
         (if new-title
             (begin
              (audit 'change-title (title item) new-title)
              (set-title! item new-title)))
         (if new-content
             (begin
               (audit 'change-content (content item) new-content)
               (set-content! item new-content)))
         (if new-originator
             (begin
               (audit 'change-originator 
                      (name (originator item))
                      (name new-originator))
               (set-originator! item new-originator)
               (notify (originator item))))
         (if new-owner
             (begin
               (audit 'change-owner 
                      (name (owner item))
                      (name new-owner))
               (set-owner! item new-owner)
               (notify (owner item))))
         (notify-print
          (lambda ()
            (format #t "~a ~a has been modified by ~a.\n\n"
                    (display-type item)
                    (global-id item)
                    (name (user (current-transaction))))
            (short-notify-info item)
            (format #t "\nThese changes were made:\n\n")
            (print-audit-note-contents (audit '?))))))))

(define-method short-notify-info ((item <item>))
  (render-para (title item) 0 "Title:         ")
  (format #t "Component:     ~a\n" "universe")
  (format #t "Owner:         ~a\n" (name (owner item)))
  (format #t "Originator:    ~a\n" (name (originator item)))
  (format #t "Created:       ~a\n" 
          (transaction-display-time (create-xaction item)))
  (format #t "\n")
  (if *web-app-root*
      (format #t "<~a~a>\n" *web-app-root* (global-id item))))

(define *web-app-root* #f)
(define (set-web-app-root! war)
  (set! *web-app-root* war))

(define (item-recreate (item <item>))
  (if (not (delete-xaction item))
      (service-error 434 "Cannot recreate item `~a' b;not currently deleted" 
                     (global-id item))
      (with-audit
       item
       (lambda (audit)
         (notify (owner item))
         (notify (originator item))
         (notify (root-domain (current-information-base)) 'item-recreate)
         (notify-subject "~a ~a recreated" 
                         (display-type item) 
                         (global-id item))
         (notify-print
          (lambda ()
            (format #t "~a item ~a has been recreated.\n\n" 
                    (display-type item) 
                    (global-id item))
            (short-notify-info item)))
         (audit 'recreate (delete-xaction item))
         (set-delete-xaction! item #f)))))

(define (item-delete (item <item>))
  (if (delete-xaction item)
      (service-error 433 "Item `~a' already deleted" (global-id item))
      (with-audit
       item
       (lambda (audit)
         (notify (owner item))
         (notify (originator item))
         (notify (root-domain (current-information-base)) 'item-delete)
         (notify-subject "~a ~a deleted" 
                         (display-type item) 
                         (global-id item))
         (notify-print
          (lambda ()
            (format #t "~a item ~a has been deleted.\n\n"
                    (display-type item)
                    (global-id item))
            (short-notify-info item)))
         (audit 'delete)
         (set-delete-xaction! item (current-transaction))))))

(define (change-disposition item to)
  (with-audit
   item
   (lambda (audit)
     (notify (owner item))
     (notify (originator item))
     (notify (component item) 'change-disposition)
     (notify-subject "~a ~a Disposition Changed to ~C"
                     (display-type item)
                     (global-id item)
                     to)
     ;;
     (audit 'change-disposition (get-property item 'disposition 'undecided) to)
     (set-property! item 'disposition to)
     ;;
     (notify-print
      (lambda ()
        (format #t "The disposition of ~a ~a has been changed by ~a.\n\n"
                (display-type item)
                (global-id item)
                (name (user (current-transaction))))
        (short-notify-info item)
        (format #t "\nThese changes were made:\n\n")
        (print-audit-note-contents (audit '?)))))))

(define (reparent item newp)
  (let* ((item (lookup-item item))
         (new-parent (lookup-item newp))
         (old-parents (parent-items item))
         (old-id (global-id item)))
    ;;
    (for-each
     (lambda (p)
       (if (issue? item)
           (unlink-sub-issue p item)
           (unlink-child p item)))
     old-parents)
    ;;
    (if (issue? item)
        (link-sub-issue new-parent item)
        (link-child new-parent item))
    ;;
    (with-audit
     item
     (lambda (audit)
       (notify (owner item))
       (notify (originator item))
       (notify (component item) 'change-parent)
       (notify-subject "~a ~a Reparented (now ~a)" 
                       (display-type item) 
                       old-id
                       (global-id item))
       (audit 'change-parent 
              old-parents 
              (list new-parent)
              old-id
              (global-id item))
       (notify-print
        (lambda ()
          (format #t "The parent of ~a ~a has been changed by ~a.\n"
                  (display-type item)
                  old-id
                  (name (user (current-transaction))))
          (format #t "That ~a is now ~a.\n" 
                  (display-type item) 
                  (global-id item))
          (short-notify-info item)
          (format #t "\nThese changes were made:\n\n")
          (print-audit-note-contents (audit '?))))))))
    
(define (mark item . opts)
  (let ((item (lookup-item item)))
    (let loop ((o opts))
      (if (null? o)
          item
          (let ((k (keyword->symbol (car o)))
                (v (cadr o)))
            (case k
              ((disposition)
               (if (member v '("undecided"
                               "taken"
                               "future"
                               "declined"))
                   (change-disposition item (string->symbol v))
                   (service-error 451 "Invalid disposition `~a'" v)))
              (else
               (service-error 452 "Invalid option to mark, `--~a'" k)))
            (loop (cddr o)))))))
  
(define (view item)
  (print-details (lookup-item item)))

(define (audit-user (self <user>) event . args)
  (set-history! self (cons (cons (if (null? args)
                                     event
                                     (cons event args))
                                 (current-transaction))
                           (history self))))
;;;
;;;
;;;    interests  -->  ((observer #[<domain> universe])
;;;                     (manager #[<domain> foo] #[<domain> bar]))

(define (notify-create (the-owner <user>) 
                       (the-interest <symbol>) 
                       (the-domain <domain>))
  (let* ((il (get-property the-owner 'interest '()))
         (for-i (assq the-interest il)))
    (audit-user the-owner 'notify-create the-interest (name the-domain))
    (if (not for-i)
        (set-property! the-owner 'interest (cons (list the-interest
                                                       the-domain)
                                                 il))
        (if (memq the-domain (cdr for-i))
            (service-error 449 "Interest `~a' already present on `~a'"
                           the-interest
                           (name the-domain))
            (set-cdr! for-i (cons the-domain (cdr for-i)))))
    (values)))

(define (notify-delete (the-owner <user>) 
                       (the-interest <symbol>) 
                       (the-domain <domain>))
  (let* ((il (get-property the-owner 'interest '()))
         (for-i (assq the-interest il)))
    (audit-user the-owner 'notify-delete the-interest (name the-domain))
    (if (or (not for-i)
            (not (memq the-domain (cdr for-i))))
        (service-error 448 "Interest `~a' not defined for `~a'"
                       the-interest
                       (name the-domain))
        (if (equal? (cdr for-i) (list the-domain))
            (set-property! the-owner 'interest (delq! for-i il))
            (set-cdr! for-i (delq! the-domain for-i))))
    (values)))


;;;

(define (coalesce-continuations port)
  (let loop ((accum '()))
    (let ((x (read port)))
      (if (eof-object? x)
          (reverse accum)
          (if (eq? (car x) '...)
              (loop (cons (append (car accum) (list (caddr x)))
                          (cdr accum)))
              (loop (cons x accum)))))))

(define (file-nodes->script input)
  (let loop ((stack '())
             (output '())
             (input input))
    (if (null? input)
        (reverse output)
        (let* ((n (car input))
               (head (car n))
               (xposn (cadr n))
               (text (string-join #\newline (cddr n))))
          (format #t "----- ~s -----\n" n)
          (print stack)
          (if (and (pair? stack)
                   (<= xposn (caar stack)))
              (loop (cdr stack)
                    output
                    input)
              (case head
                ((issue)
                 (if (null? stack)
                     (bind ((tag0 (process-internal-id text)))
                       (if tag0
                           (let ((o (cons xposn `(lookup-item ,tag0))))
                             (loop (cons o stack)
                                   (cons o output)
                                   (cdr input)))
                           (let ((o (cons xposn `(specialize root ,text))))
                             (loop (cons o stack)
                                   (cons o output)
                                   (cdr input)))))
                     (let ((parent (list 'saved (caar stack))))
                       (case (cadar stack)
                         ((specialize question)
                          (let ((o (cons xposn `(specialize ,parent ,text))))
                            (loop (cons o stack)
                                  (cons o output)
                                  (cdr input))))
                         (else
                          (let ((o  (cons xposn `(question ,parent ,text))))
                            (loop (cons o stack)
                                  (cons o output)
                                  (cdr input))))))))
                ((position taken-position declined-position future-position)
                 (if (null? stack)
                     (error "out of sync at ~s\n" n)
                     (let ((parent (list 'saved (caar stack))))
                       (case (cadar stack)
                         ((specialize question lookup-item)
                          (let ((o (cons xposn 
                                         `(respond 
                                           ,parent 
                                           ,text
                                           ,@(case head
                                               ((taken-position) 
                                                '(disposition: 'taken))
                                               ((declined-position)
                                                '(disposition: 'declined))
                                               ((future-position)
                                                '(disposition: 'future))
                                               (else '()))))))
                            (loop (cons o stack)
                                  (cons o output)
                                  (cdr input))))
                         (else
                          (error "cannot respond at ~s\n" n))))))
                ((support object-to)
                 (if (null? stack)
                     (error "out of sync at ~s\n" n)
                     (let ((parent (list 'saved (caar stack))))
                       (case (cadar stack)
                         ((respond lookup-item)
                          (let ((o (cons xposn `(,head ,parent ,text))))
                            (loop (cons o stack)
                                  (cons o output)
                                  (cdr input))))
                         (else
                          (error "cannot argue at ~s\n" n))))))
                (else
                 (error "what? at ~s\n" head))))))))

,(use rs.util.evalhelp)

(define *script-basis* (build-private-module (& specialize)
                                             (& respond)
                                             (& note)
                                             (& support)
                                             (& object-to)
                                             (& question)
                                             (& mark)
                                             (& edit)))

(define (exec-script lst root)
  (let ((script-envt (make-top-level *script-basis*))
        (state (make-vector 99)))
    (define-in-envt script-envt 'lookup-item lookup-item*)
    (define-in-envt script-envt '*state* state)
    (define-in-envt script-envt 'saved (lambda (n)
                                         (vector-ref state n)))
    (define-in-envt script-envt 'root 
      (or root
          (root-issue (current-information-base))))
    ;;
    (pp lst)
    ;;
    (map (lambda (l)
           (if (number? (car l))
               (let ((x (eval-in-envt (cdr l) script-envt)))
                 (vector-set! state (car l) x)
                 x)
               (eval-in-envt l script-envt)))
         lst)))
