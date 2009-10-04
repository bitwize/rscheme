
(define (string->user* name)
  (or (string->user name)
      (service-error 402 "No such user `~a'" name)))

(define (string->domain* name)
  (or (string->domain name)
      (service-error 403 "No such domain `~a'" name)))

(define (string->item* item)
  (or (lookup-item item)
      (service-error 401 "No such item `~a'" item)))
  
(define (lookup-item* item)
  (let ((i (if (instance? item <item>)
               item
               (if item
                   (string->item* item)
                   #f))))
    (if (and i (delete-xaction i))
        (service-error 409 "Item `~a' was deleted" (global-id i))
        i)))

;;;

;;;  An `issue block' is an issue plus the transitive closure of
;;;  related objects according to:
;;;
;;;       - if the related object is a position, include it
;;;       - if the related object is an argument, include it
;;;       - if the related object is an issue, then:
;;;         - if the issue is marked as `keep-with-parent', include it
;;;         - otherwise, include a link to it
;;; 

(define *xml-special-pattern* (reg-expr->proc '(or #\< #\> #\&
                                                   (range #\x7f #\xff))))

(define (xml-esc1 str i)
  (case (string-ref str i)
    ((#\<) (values "&lt;" 1))
    ((#\>) (values "&gt;" 1))
    ((#\&) (values "&amp;" 1))
    (else
     (values (format #f "&#x~x;" (char->integer (string-ref str i))) 1))))

(define (xml-escape str)
  (apply-escapements str *xml-special-pattern* xml-esc1))

(define (print-issue-tree-as-xml tree)
  ;
  (define (depth-pre n)
    (if (< n 10)
        (vector-ref '#(""                               ; 0
                        "  "                            ; 1
                        "    "                          ; 2
                        "      "                        ; 3
                        "        "                      ; 4
                        "          "                    ; 5
                        "            "                  ; 6
                        "              "                ; 7
                        "                "              ; 8
                        "                  ")           ; 9
                    n)
        (make-string (* n 2) #\space)))
  ;
  (define (node-xml-type node)
    (cond
     ((instance? node <issue>) "issue")
     ((instance? node <position>) "position")
     ((instance? node <affirmative>) 
      (values "argument" " type=\"support\""))
     ((instance? node <negative>) 
      (values "argument" " type=\"object\""))
     (else "other")))
  ;
  (define (render-link node pre)
    (bind ((gi opts (node-xml-type node)))
      (format #t "~a<xref target=\"~a\" linkend=\"~a\"~a>\n" 
              pre 
              gi
              (global-id node)
              (or opts "")))
    (format #t "~a  <title>~a</title>\n" pre (xml-escape (title node)))
    (format #t "~a</xref>\n" pre))
  ;
  (define (render-user pre u)
    (format #t "~a<user>\n" pre)
    (format #t "~a  <login>~a</login>\n" pre (name u))
    (format #t "~a  <fullname>~a</fullname>\n" pre (fullname u))
    (if (email u)
        (format #t "~a  <email>~a</email>\n" pre (email u)))
    (if (user-area u)
        (format #t "~a  <area>~a</area>\n" pre (user-area u)))
    (format #t "~a</user>\n" pre))
  ;
  (define (render-xaction pre gi x)
    (format #t "~a<~a>\n" pre gi)
    (format #t "~a  <time>~a</time>\n" pre (transaction-display-time x))
    (render-user (string-append pre "  ") (user x))
    (format #t "~a</~a>\n" pre gi))
  ;
  (define (render-audit-arg pre arg)
    (if (instance? arg <ibase-transaction>)
        (render-xaction pre "arg" arg)
        (format #t "~a<arg>~a</arg>\n" pre (to-string arg))))
  ;
  (define (render-notes node pre)
    (let ((notelist (notes node)))
      (if (pair? notelist)
          (begin
            (format #t "~a<notes>\n" pre)
            (for-each
             (lambda (n)
               (if (instance? n <audit-note>)
                   (begin
                    (format #t "~a  <audit id=\"~a\">\n" pre (global-id n))
                    (render-xaction (string-append pre "    ") 
                                    "created"
                                    (create-xaction n))
                    (for-each
                     (lambda (ae)               ; audit entry
                       (format #t "~a    <auditentry action=\"~a\">\n"
                               pre (car ae))
                       (for-each (lambda (aa)   ; audit arg
                                   (render-audit-arg
                                    (string-append pre "      ")
                                    aa))
                                 (cdr ae))
                       (format #t "~a    </auditentry>\n" pre))
                     (audit-entries n))
                    (format #t "~a  </audit>\n" pre))
                   (begin
                     (format #t "~a  <note id=\"~a\">\n" pre (global-id n))
                     (render-xaction (string-append pre "    ") 
                                     "created"
                                     (create-xaction n))
                     (format #t "~a  <title>~a</title>\n" 
                             pre (xml-escape (title n)))
                     (format #t "~a  <description>~a</description>\n" 
                             pre (xml-escape (content n)))
                     (format #t "~a  </note>\n" pre))))
             notelist)
            (format #t "~a</notes>\n" pre)))))
  ;
  (define (render-open node depth pre)
    (bind ((gi opts (node-xml-type node)))
      (format #t "~a<~a id=\"~a\" depth=\"~d\"~a>\n" 
              pre gi 
              (global-id node)
              depth
              (or opts ""))
      (format #t "~a  <head>\n" pre)
      (format #t "~a    <title>~a</title>\n" pre (xml-escape (title node)))
      ;;
      (format #t "~a    <owner>\n" pre)
      (render-user (string-append pre "      ") (owner node))
      (format #t "~a    </owner>\n" pre)
      ;;
      (format #t "~a    <originator>\n" pre)
      (render-user (string-append pre "      ") (originator node))
      (format #t "~a    </originator>\n" pre)
      ;;
      (let ((xpre (string-append pre "    ")))
        (render-xaction xpre "created" (create-xaction node))
        (render-xaction xpre "modified" (modify-xaction node))
        (if (delete-xaction node)
            (render-xaction xpre "deleted" (delete-xaction node))))
      (format #t "~a    <disposition>~a</disposition>\n" 
              pre (get-property node 'disposition "undecided"))
      (format #t "~a  </head>\n" pre)
      (if (= depth 0)
          (begin
           (format #t "~a  <ancestors>\n" pre)
           (for-each (lambda (p)
                       (render-link p (string-append pre "    ")))
                     (reverse (cdr (item-heritage node))))
           (format #t "~a  </ancestors>\n" pre)))
      (format #t "~a  <description>~a</description>\n" 
              pre (xml-escape (content node)))
      (render-notes node (string-append pre "  "))
      gi))
  ;
  (define (render-close node info pre)
    (format #t "~a</~a>\n" pre info))
  ;
  (letrec ((visit (lambda (node depth)
                    (let (((item <item>) (car node))
                          (annot (cadr node))
                          (pre (depth-pre depth)))
                      (if (memq 'link annot)
                          (render-link node pre)
                          (begin
                            (let ((x (render-open item depth pre)))
                              (for-each (lambda (ch)
                                          (visit ch (+ depth 1)))
                                        (cddr node))
                              (render-close item x pre))))))))
    (visit tree 0)))

;;;  Tables
;;;  ------
;;;  <user>
;;;  <component>
;;;  <priviledge-group>
;;;  <authority>
;;;  <item>
;;;  <access-host>

(define *command-list*
  (list (cons 'print (lambda () process-print-command))
        (cons '(user view) (lambda () process-user-view-command))
        (cons '(user create) (lambda () process-user-create-command))
        (cons '(user edit) (lambda () process-user-edit-command))
        (cons '(notify create) (lambda () process-notify-create-command))
        (cons '(notify delete) (lambda () process-notify-delete-command))
        (cons '(item edit) (lambda () process-item-edit-command))
        (cons '(item delete) (lambda () process-item-delete-command))
        (cons '(item recreate) (lambda () process-item-recreate-command))
        (cons '(item reparent) (lambda () process-item-reparent-command))
        (cons 'view (lambda () process-view-command))
        (cons 'commit (lambda () process-commit-command))
        ;
        (cons 'specialize (lambda () process-specialize-command))
        (cons 'respond (lambda () process-respond-command))
        (cons 'support (lambda () process-support-command))
        (cons 'object-to (lambda () process-object-to-command))
        (cons 'question (lambda () process-question-command))
        (cons 'note (lambda () process-note-command))
        ;
        (cons 'mark (lambda () process-mark-command))
        ;
        (cons 'import (lambda () process-import-command))
        ;
        (cons 'report (lambda () process-report-command))
        (cons '(host create) (lambda () process-host-create-command))
        (cons '(host delete) (lambda () process-host-delete-command))
        (cons 'authority (lambda () process-authority-command))
        (cons 'domain (lambda () process-domain-command))
        (cons 'export (lambda () process-export-command))
        (cons 'kill (lambda () process-kill-command))
        ))


(define (process-kill-command (self <request>))
  (process-exit 3))

;;;
;;;   --notify --create --owner USER ... 
;;;                     --interest INTEREST ... 
;;;                     --domain DOMAIN ...

(define (process-notify-create-command (self <request>))
  (for-each-owner-interest-domain self notify-create))

(define (process-notify-delete-command (self <request>))
  (for-each-owner-interest-domain self notify-delete))
     
(define (for-each-owner-interest-domain (self <request>) proc)
  (let ((o-l (map string->user* (get-required-arg-list self 'owner)))
        (d-l (map string->domain* (get-required-arg-list self 'domain)))
        (i-l (map string->symbol (get-required-arg-list self 'interest))))
    (for-each
     (lambda (o)
       (for-each
        (lambda (i)
          (for-each
           (lambda (d)
             (proc o i d))
           d-l))
        i-l))
     o-l)))

;;;
;;;   --user --edit user ... [--login LOGIN] [--fullname FNAME] [--super Y/N]
;;;                          [--email EMAIL] [--area AREA]
;;;

(define (process-user-edit-command (self <request>))
  (let ((users (map string->user* (get-required-arg-list self 'edit)))
        (new-login (get-optional-value self 'login))
        (new-fullname (get-optional-value self 'fullname))
        (new-super (get-optional-value self 'super))
        (new-email (get-optional-value self 'email))
        (new-area (get-optional-value self 'area)))
    ;
    (let ((a (assoc new-super '((#f #f) ("Y" 'y) ("N" 'n) ("y" 'y) ("n" 'n)))))
      (if (not a)
          (service-error 403 "Invalid value for `--super' attribute: ~s" 
                         new-super))
      (set! new-super (cadr a)))
    ;
    (for-each
     (lambda (u)
       (if (and new-login (string=? new-login (name u)))
           (set! new-login #f))
       (if (and new-fullname (string=? new-fullname (fullname u)))
           (set! new-fullname #f))
       (if (and new-email (email u) (string=? new-email (email u)))
           (set! new-email #f))
       (if (and new-area (user-area u) (string=? new-area (user-area u)))
           (set! new-area #f))
       (let ((a '()))
         (define (touch n e)
           (set! a (cons n a))
           e)
         (if new-login
             (or (touch 'login
                        (set-with-index! 
                         u name set-name! new-login 
                         (user-index (current-information-base))))
                 (service-error 451 "Login `~a' already in use" new-login)))
         (if new-fullname
             (set-fullname! u (touch 'fullname new-fullname)))
         (if new-email
             (set-email! u (touch 'email new-email)))
         (if new-area
             (set-user-area! u (touch 'area new-area)))
         (case new-super
           ((y) (set-super-user?! u (touch 'super #t)))
           ((n) (set-super-user?! u (touch 'super #f))))
         (apply audit-user u 'edit (reverse a))))
     users)))


;;;
;;;   --user --create login --fullname FNAME [--super Y/N]
;;;                         [--email EMAIL] [--area AREA]
;;;

(define (process-user-create-command (self <request>))
  (let ((new-login (get-required-arg-value self 'create))
        (new-fullname (get-required-arg-value self 'fullname))
        (new-super (get-optional-value self 'super))
        (new-email (get-optional-value self 'email))
        (new-area (get-optional-value self 'area)))
    (let ((u (make <user>
                   name: new-login
                   fullname: new-fullname
                   email: new-email
                   super-user?: (and new-super (equal? new-super "Y"))
                   history: (list (cons 'create (current-transaction))))))
      (if new-area
          (set-user-area! u new-area))
      (or (add-to-index! u name (user-index (current-information-base)))
          (service-error 451 "Login `~a' already in use" new-login))
      u)))

;;;

(define (add-to-index! object getter index)
  (if (table-lookup index (getter object))
      #f
      (begin
        (table-insert! index (getter object) object)
        (getter object))))
        

(define (set-with-index! object getter setter value index)
  (if (table-lookup index value)
      #f
      (begin
        (table-remove! index (getter object))
        (setter object value)
        (table-insert! index value object)
        value)))

;;;
;;;   --user --view user ... [--long]
;;;

(define (process-user-view-command (self <request>))
  (let ((user-names (get-required-arg-list self 'view))
        (long? (get-optional-flag self 'long)))
    (for-each
     (lambda (uname)
       (let ((u (string->user* uname)))
         (with-response-output 
          self
          (lambda ()
            (format #t "      User: ~a\n" (name u))
            (format #t " Full name: ~a\n" (fullname u))
            (if (email u)
                (format #t "    e-mail: ~a\n" (email u)))
            (if (user-area u)
                (format #t "      Area: ~a\n" (user-area u)))
            (format #t "Super-user: ~a\n" (if (super-user? u)
                                              "Yes"
                                              "No"))
            (newline)
            (format #t "Authority entries: [~d]\n"
                    (vector-length (authority-vector u)))
            (format-table #t
                          '("Domain" "Priviledge Group" "Permission")
                          (map (lambda (a)
                                 (list
                                  (name (with-respect-to a))
                                  (name (priviledge-group a))
                                  (case (access-type a)
                                    ((0) "Deny")
                                    ((1) "Grant"))))
                               (vector->list (authority-vector u))))
            (newline)
            (format #t "Access host entries: [~d]\n" 
                    (length (access-host-list u)))
            (format-table #t
                          '("Local User" "Hostname" "ident?")
                          (map (lambda (a)
                                 (list (user a)
                                       (host a)
                                       (if (run-ident-check? a)
                                           "Yes"
                                           "No")))
                               (access-host-list u)))
            (if long?
                (begin
                  (newline)
                  (let ((ints (apply append
                                     (map (lambda (n)
                                            (map (lambda (e)
                                                   (list 
                                                    (symbol->string (car n) )
                                                    (name e)))
                                                 (cdr n)))
                                          (get-property u 'interest '())))))
                    (format #t "Interest table: [~d]\n" (length ints))
                    (format-table #t '("Interest" "Domain") ints))
                  ;
                  (newline)
                  (format #t "User change history: [~d]\n" 
                          (length (history u)))
                  (format-table #t
                                '("Date/Time" "User" "Event" "Details")
                                (map (lambda (action (x <transaction>))
                                       (append
                                        (list (time-rendition (process-time x))
                                              (name (user x)))
                                        (if (symbol? action)
                                            (list (symbol->string action) "")
                                            (list 
                                             (symbol->string (car action))
                                             (format #f "~s" (cdr action))))))
                                     (map car (history u))
                                     (map cdr (history u))))))))))
     user-names)))

(define (format-table port headers data)
  (define (col-w k)
    (let loop ((w (string-length (list-ref headers k)))
               (r data))
      (if (null? r)
          w
          (loop (max w (string-length (list-ref (car r) k)))
                (cdr r)))))
  ;
  (let* ((num-cols (length headers))
         (col-widths (map col-w (range num-cols))))
    (define (show-row row)
      (let loop ((cw col-widths)
                 (entry row)
                 (subseq? #f))
        (if (pair? entry)
            (let ((x (car entry)))
              (if subseq? 
                  (format port "  "))
              (format port "~a~a" x (if (null? (cdr entry))
                                        ""
                                        (make-string (- (car cw)
                                                        (string-length x))
                                                     #\space)))
              (loop (cdr cw)
                    (cdr entry)
                    #t))
            (format port "\n"))))
    ;
    (show-row headers)
    (show-row (map (lambda (i)
                     (make-string i #\-))
                   col-widths))
    (for-each show-row data)))
                   

(define (get-required-arg-value (self <request>) key)
  (let ((k (assq key (keylist self))))
    (if k
        (case (length (cdr k))
          ((1) (cadr k))
          ((0) (service-error 411 "No values supplied for `--~a'" key))
          (else (service-error 413 "Multiple values supplied for `--~a'" key)))
        (service-error 412 "No `--~a' supplied" key))))

(define (get-optional-value (self <request>) key)
  (let ((k (assq key (keylist self))))
    (if k
        (case (length (cdr k))
          ((1) (cadr k))
          ((0) (service-error 414 "No value supplied for `--~a'" key))
          (else (service-error 415 "Multiple values supplied for `--~a'" key)))
        #f)))

(define (get-optional-flag (self <request>) key)
  (let ((k (assq key (keylist self))))
    (if k
        (if (null? (cdr k))
            #t
            (service-error 414 "Unexpected value for flag `--~a'" key))
        #f)))

(define (get-required-arg-list (self <request>) key)
  (let ((k (assq key (keylist self))))
    (if k
        (if (null? (cdr k))
            (service-error 411 "No values supplied for `--~a'" key)
            (cdr k))
        (service-error 412 "No `--~a' supplied" key))))

;;;
;;;   --host [--owner login]
;;;     --create [user@]host[:option,....] ...
;;;     --delete [user@]host ...
;;;
;;;  the only option so far is "ident"

(define (process-host-delete-command (self <request>))
  (let* ((removes (get-required-arg-list self 'delete))
         (this-user (if (get-optional-value self 'owner)
                        (string->user* (get-optional-value self 'owner))
                        (get-user-object self)))
         (this-name (name this-user))
         (alist (access-host-list this-user)))
    (for-each
     (lambda (str)
       (bind ((u h opts (crack-user-at-host str this-name))
              (a (find-user-host u h alist)))
         (if (not (null? opts))
             (service-error 408 "Cannot specify options on host record remove: `~a'" str))
         (if a
             (set! alist (delq! a alist))
             (service-error 407 "No such host record `~a@~a'" u h))))
     removes)
    ;;
    (if (not (eq? alist (access-host-list this-user)))
        (set-access-host-list! this-user alist))))

(define (process-host-create-command (self <request>))
  (let* ((adds (get-required-arg-list self 'create))
         (this-user (if (get-optional-value self 'owner)
                        (string->user* (get-optional-value self 'owner))
                        (get-user-object self)))
         (this-name (name this-user))
         (alist (access-host-list this-user)))
    ;;
    (for-each
     (lambda (str)
       (bind ((u h opt (crack-user-at-host str this-name))
              (a (find-user-host u h alist)))
         (if a
             (set-options! a (list->vector opt))
             (add-access-record (name this-user) u h opt))))
     adds)))

(define (find-user-host u h alist)
  (let loop ((a alist))
    (if (null? a)
        #f
        (if (and (string=? (user (car a)) u)
                 (string=? (host (car a)) h))
            (car a)
            (loop (cdr a))))))

(define *uh-pattern* (reg-expr->proc '(seq
                                       (? (seq (save (+ (not (or #\@ #\:))))
                                               #\@))
                                       (save (+ (not (or #\@ #\:))))
                                       (? (seq #\: (save (not #\:)))))))

(define *uh-options* '(("ident" ident)))

(define (crack-user-at-host str default-user-name)
  (bind ((s e u h options (*uh-pattern* str)))
    (format #t "cracked: ~s ~s ~s\n" u h options)
    (values (or u default-user-name)
            h
            (if options
                (map (lambda (opt)
                       (if (assoc opt *uh-options*)
                           (cadr (assoc opt *uh-options*))
                           (service-error 405
                                          "Unrecognized host entry option `~a'"
                                          opt)))
                     (string-split options #\,))
                '()))))

(define (with-response-output (self <request>) thunk)
  (client-print-message 
   (output-port (in-session self))
   (with-output-to-string thunk)))

;;;

(define-method print-raw-details ((self <item>))
  (format #t "~a|~a|~a|~a|~a\n"
          (global-id self)
          (name (owner self))
          (get-property self 'disposition "")
          (escape-raw (title self))
          (escape-raw (content self))))

(define non-raw-char-pattern (reg-expr->proc '(not (or (range #\space #\{)
                                                       #\}
                                                       #\~))))

(define *char-esc-table* (make-fixnum-table))

(for-each (lambda (i)
            (table-insert! 
             *char-esc-table*
             i
             (if (non-raw-char-pattern (string (integer->char i)))
                 (format #f "\\~03o" i)
                 (string (integer->char i)))))
          (range 256))

(table-insert! *char-esc-table* (char->integer #\newline) "\\n")
(table-insert! *char-esc-table* (char->integer #\tab) "\\t")


(define (apply-escapements str search-next escape1)
  (bind ((s e (search-next str)))
    (if s
        (call-with-output-string
         (lambda (port)
           (let loop ((prev 0)
                      (i s))
             (write-string port (substring str prev i))
             (bind ((subst len (escape1 str i)))
               (write-string port subst)
               (if (< (+ i len) (string-length str))
                   (bind ((s e (search-next str (+ i len))))
                     (if s
                         (loop (+ i len) s)
                         (write-string port (substring str (+ i len))))))))))
        str)))

(define (escape-raw str)
  (apply-escapements str
                     non-raw-char-pattern
                     (lambda (str i)
                       (values (table-lookup *char-esc-table* 
                                             (char->integer
                                              (string-ref str i)))
                               1))))
                     
  #|
  (bind ((s e (non-raw-char-pattern str)))
    (if s
        (call-with-output-string
         (lambda (port)
           (write-string port (substring str 0 s))
           (let loop ((i s))
             (if (< i (string-length str))
                 (let ((k (char->integer (string-ref str i))))
                   (write-string port (table-lookup *char-esc-table* k))
                   (loop (+ i 1)))))))
        str))
  |#
      
;;;

(define (process-print-command (self <request>))
  (let* ((n (assq 'print (keylist self)))
         (items (map string->item* (cdr n)))
         (raw? (get-optional-flag self 'raw))
         (xml? (get-optional-flag self 'xml))
         (result (with-output-to-string
                   (lambda ()
                     (for-each
                      (cond
                       (xml?
                        (lambda (i)
                          (print-issue-tree-as-xml (list i '()))))
                       (raw?
                        print-raw-details)
                       (else
                        print-details))
                      items)))))
    (client-print-message (output-port (in-session self)) result)))

(define (process-view-command (self <request>))
  (let* ((n (assq 'view (keylist self)))
         (items (map lookup-item* (cdr n)))
         (html? (get-optional-flag self 'html))
         (xml? (get-optional-flag self 'xml))
         (browser? (get-optional-flag self 'html-browser))
         (short? (get-optional-flag self 'short))
         (pfilter (if (get-optional-value self 'where)
                      (let ((w (get-optional-value self 'where)))
                        (make-prune-filter
                         (and (string-search w "undecided") #t)
                         (and (string-search w "future") #t)
                         (and (string-search w "moot") #t)))
                      #t))
         (issue-depth (if (get-optional-flag self 'deep) #t 1))
         (data (map (lambda (i)
                      (query-ibis-tree i
                                       depth: issue-depth
                                       filter: pfilter))
                    items))
         (result (with-output-to-string
                   (lambda ()
                     (for-each
                      (lambda (r)
                        (if xml?
                            (print-issue-tree-as-xml r)
                            (render-tree-query-result 
                             r 
                             (if browser?
                                 print-as-html-for-browser
                                 (if html?
                                     print-as-html
                                     print-as-text)))))
                      data)
                     (newline)))))
    (client-print-message (output-port (in-session self)) result)))

(define (process-import-command (self <request>))
  (let ((files (cdr (assq 'import (keylist self)))))
    (for-each
     (lambda (f)
       (client-print-message (output-port (in-session self))
                             (format #f "Importing ~s...\n" f))
       (let ((x (client-upload (input-port (in-session self))
                               (output-port (in-session self))
                               f))
             (tmpf (format #f "/tmp/stage-%d-%05d.ibis" 
                           (with-module unixm getpid)
                           (modulo (random) 10000))))
         (handler-case (unlink tmpf) ((<condition>)))
         (call-with-output-file
             tmpf
           (lambda (port)
             (write-string port x)))
         ;;
         (format #t "*** ~d bytes from ~s, using ~s\n" 
                 (string-length x) 
                 f
                 tmpf)
         (let* ((p (open-input-process (string-append *ibis->script*
                                                      " "
                                                      tmpf)))
                (v (coalesce-continuations p)))
           (close-input-port p)
           (client-print-message 
            (output-port (in-session self))
            (format #f "   read ~d actions...\n" (length v)))
           (let* ((s (file-nodes->script v))
                  (r (exec-script s (lookup-item* 
                                     (get-optional-value self 'root)))))
             (response-table-from-proc
              self
              (lambda ()
                (build-item-report r)))))))
     files)))

(define *ibis->script* 
  (with-module paths
    (pathname->string
     (append-path (current-absolute-directory)
                  (string->file "ibis2script.pl")))))


;;;

(define (call-interactive (self <request>) cmd proc msg)
  (let ((o (apply proc
                  (get-required-arg-value self cmd)
                  (get-required-arg-value self 'title)
                  (let ((c (get-optional-value self 'comment)))
                    (if c
                        (if (string=? c "-")
                            (list (get-stdin self))
                            (list c))
                        '())))))
    (with-response-output
     self
     (lambda ()
       (if (get-optional-flag self 'raw)
           (format #t "~a\n" (global-id o))
           (format #t msg (global-id o)))))))

(define (process-specialize-command (self <request>))
  (call-interactive self 'specialize specialize 
                    "Opened issue `~a'\n"))

(define (process-respond-command (self <request>))
  (call-interactive self 'respond respond 
                    "Added response `~a'\n"))

(define (process-support-command (self <request>))
  (call-interactive self 'support support
                    "Added supporting argument `~a'\n"))

(define (process-object-to-command (self <request>))
  (call-interactive self 'object-to object-to
                    "Added objecting argument `~a'\n"))

(define (process-question-command (self <request>))
  (call-interactive self 'question question
                    "Opened questioning issue `~a'\n"))

;;;  --note ITEM ... --title TITLE --comment COMMENT

(define (process-note-command (self <request>))
  (call-interactive self 'note notate
                    "Added note `~a'\n"))

(define (process-mark-command (self <request>))
  (let ((i (get-required-arg-value self 'mark))
        (d (get-optional-value self 'disposition))
        (any? #f))
    (if d
        (begin
          (mark i disposition: d)
          (set! any? #t)))
    (if (not any?)
        (service-error 330 "No `--mark' subcommand"))))

;;;

(define (process-commit-command (self <request>))
  (let* ((ps (open-store-pstore *open-store*))
         (n (num-dirty-pages ps))
         (k (commit ps)))
    (client-print-message (output-port (in-session self))
                          (format #f "Commit record ~s (~d pages dirty)\n"
                                  k n))))


;;;
;;;   --authority [--grant|--deny] priviledge ... --owner user --on domain
;;;

(define (process-authority-command (self <request>))
  (let ((user-name (get-required-arg-value self 'owner))
        (domain-name (get-required-arg-value self 'on))
        (grant (assq 'grant (keylist self)))
        (deny (assq 'deny (keylist self))))
    (cond
     ((and grant deny)
      (service-error 417 "Mutually exclusive `--grant' and `--deny' supplied"))
     ((not (or grant deny))
      (service-error 418 "Neither `--grant' nor `--deny' supplied"))
     ((null? (cdr (or grant deny)))
      (service-error 417 "No priviledges specified on `--~a'" 
                     (if grant "grant" "deny")))
     (else
      (let ((user (string->user user-name))
            (domain (string->domain domain-name))
            (privs (map string->priviledge-group (cdr (or grant deny)))))
        (if (not user)
            (service-error 421 "No such user `~a'" user-name))
        (if (not domain)
            (service-error 422 "No such domain `~a'" domain-name))
        ;;
        (for-each 
         (lambda (name priv)
           (if (not priv)
               (service-error 423 "No such priviledge group `~a'" name)))
         (cdr (or grant deny))
         privs)
        ;; TODO: Delete duplicate entries
        (set-authority-vector!
         user
         (vector-append
          (list->vector
           (map
            (lambda (priv)
              (make <authority>
                    owner: user
                    with-respect-to: domain
                    priviledge-group: priv
                    access-type: (if grant 1 0)))
            privs))
          (authority-vector user))))))))
            

;;;
;;;   --domain domain
;;;

(define (process-domain-command (self <request>))
  (service-error 990 "Not implemented"))

;;;
;;;   --report type [--where expr]
;;;

(define (process-report-command (self <request>))
  (bind ((t (string->symbol (get-required-arg-value self 'report))))
    (response-table-from-proc self 
                              (lambda () 
                                (compute-report-result self t)))))
                                

(define (response-table-from-proc (self <request>) thunk)
  (bind ((hdr data (thunk)))
    (with-response-output
     self
     (lambda ()
       (if (get-optional-flag self 'raw)
           (for-each (lambda (row)
                       (display (string-join #\| (map escape-raw row)))
                       (newline))
                     data)
           (format-table #t hdr data))))))

(define (build-user-report)
  (values
   '("Login" "Full Name" "E-Mail" "Area" "SU?")
   (map
    (lambda ((u <user>))
      (list (name u)
            (fullname u)
            (or (email u) "")
            (or (user-area u) "")
            (if (super-user? u) "Y" "")))
    (value-sequence (user-index (current-application-root-object))))))

(define (build-item-report items)
  (values 
   '("Id" "Type" "P" "Owner" "Age" "Modified" "Title")
   (map 
    (lambda ((self <item>))
      (list (global-id self)
            (cond
             ((instance? self <issue>) "Issue")
             ((instance? self <theme>) "Theme")
             ((instance? self <note>) "Note")
             ((instance? self <position>) "Position")
             ((instance? self <affirmative>) "For")
             ((instance? self <negative>) "Against")
             (else "Other"))
            (if (eq? (check-pending self) #t) "P" "")
            (name (owner self))
            (interval->string
             (time-time 
              (time) 
              (process-time (create-xaction self))))
            (time-rendition (process-time (modify-xaction self)))
            (format #f "~@*#60a" (normalize-nl (title self)))))
    items)))

(define (sort-items item-list)
  (sort item-list 
        (lambda (a b)
          (string<? (car a) (car b)))))

(define (item-heritage (self <item>))
  (if (parent self)
      (cons self (item-heritage (parent self)))
      ; if we don't have a parent, then we should be an issue
      (let ((q (query-relation-pick 'specializes 'coarse fine: self)))
        (if (pair? q)
            (cons self (item-heritage (car q)))
            (let ((q (query-relation-pick 'questions 'item issue: self)))
              (if (pair? q)
                  (cons self (item-heritage (car q)))
                  (list self)))))))

(define-method check-pending ((self <item>))
  'if)

(define-method check-pending ((self <issue>))
  (if (and (null? (child-items self))
           (null? (sub-issues self)))
      #t
      (next-method)))

(define-method check-pending ((self <position>))
  (case (get-property self 'disposition #f)
    ((#f) #t)
    ((taken) 'if)
    ((declined future) #f)))

(define (compute-report-result (self <request>) type)
  (case type
    ((item)
     (let ((start (or (lookup-item* (get-optional-value self 'root))
                      (root-issue (current-information-base)))))
       (build-item-report 
        (map car
             (flatten-ibis-tree
              (query-ibis-tree start depth: #t))))))
    ((user)
     (build-user-report))
    ((heritage)
     (let ((start (lookup-item* (get-optional-value self 'start))))
       (build-item-report
        (reverse (item-heritage start)))))
    ((pending)
     (begin
       (define (pending-filter n d)
         (check-pending n))
       (let ((start (or (lookup-item* (get-optional-value self 'root))
                        (root-issue (current-information-base)))))
         (build-item-report
          (map (lambda (tree-node)
                 (car tree-node))
               (flatten-ibis-tree
                (query-ibis-tree start 
                                 filter: pending-filter
                                 depth: #t)))))))
    (else
     (service-error 431 "No such report `~a'" type))))

;;;
;;;   --item --delete item ...
;;;

(define (process-item-delete-command (self <request>))
  (let ((verbose? (get-optional-flag self 'verbose)))
    (for-each
     (lambda (item)
       (item-delete item)
       (if verbose?
           (with-response-output 
            self
            (lambda ()
              (format #t "Deleted item ~a\n" (global-id item))))))
     (map lookup-item* (get-required-arg-list self 'delete)))))

;;;
;;;   --item --recreate item ...
;;;

(define (process-item-recreate-command (self <request>))
  (let ((verbose? (get-optional-flag self 'verbose))
        (x (current-transaction)))
    (for-each
     (lambda (item)
       (let ((msg (if (delete-xaction item)
                      (begin
                        (item-recreate item)
                        "Recreated item `~a'\n")
                      "Item `~a' is not already deleted\n")))
         (if verbose?
             (with-response-output 
              self
              (lambda ()
                (format #t msg (global-id item)))))))
     (map string->item* (get-required-arg-list self 'recreate)))))

;;;
;;;   --item --reparent item ... --parent item
;;;

(define (process-item-reparent-command (self <request>))
  (let ((new-parent (string->item* (get-required-arg-value self 'parent))))
    (for-each
     (lambda (target)
       (reparent target new-parent))
     (map string->item* (get-required-arg-list self 'reparent)))))

;;;
;;;   --item --edit item [--comment COMMENT] 
;;;                      [--title TITLE]
;;;                      [--owner USER]
;;;                      [--originator USER]
;;;                      [--domain DOMAIN]
;;;

(define (process-item-edit-command (self <request>))
  (let ((alst (map (lambda (x)
                       (let* ((argkey (cadr x))
                              (kwd (car x))
                              (s (get-optional-value self argkey)))
                         (if s
                             (list kwd s)
                             '())))
                     '((content: comment)
                       (title: title)
                       (owner: owner)
                       (originator: originator)
                       (domain: domain)))))
    (print alst)
    (apply edit
           (get-required-arg-value self 'edit)
           (apply append alst))))


;;;
;;;   --export [root]
;;;

(define (process-export-command (self <request>))
  (let* ((c (cdr (assq 'export (keylist self))))
         (root (case (length c)
                 ((0) (root-issue (current-information-base)))
                 ((1) (lookup-item* (car c)))
                 (else (service-error 433 "Multiple roots specified")))))
    (with-response-output
     self
     (lambda ()
       (export-issue-base root)))))

(define (with-xml gi attrs thunk)
  (format #t "<~a" gi)
  (for-each (lambda (a)
              (format #t " ~a=~s" (car a) (cadr a)))
            attrs)
  (if thunk
      (begin
        (format #t ">\n")
        (thunk)
        (format #t "</~a>\n" gi))
      (format #t "/>\n")))

(define-method export-issue-base ((self <item>))
  (with-xml 'item '() #f))

(define-method export-issue-base ((self <issue>))
  (with-xml 'issue
            '()
            (lambda ()
              (for-each export-issue-base (child-items self))
              (for-each export-issue-base (sub-issues self)))))


(define (time-rendition t)
  (time->string t "%Y-%m-%d %H:%M:%S"))

(define (normalize-nl s)
  (if (string-search s #\newline)
      (string-join #\space (string-split s #\newline) )
      s))
      

