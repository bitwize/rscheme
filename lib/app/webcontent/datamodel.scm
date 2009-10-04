,(use tables
      syscalls
      util.xml
      util.xpath
      rs.db.rstore
      sort)

(define-class <uuid> (<object>) :bvec)

(define-class <audit-record> (<object>)
  (commit-id init-value: 0)
  principal
  timestamp)

(define-class <pobject> (<object>) :abstract
  (properties type: <vector> init-value: '#())
  (oid type: <fixnum>)
  (creation-audit type: <audit-record>)
  (modification-audit type: <audit-record>)
  (deletion-audit init-value: #f))

;;;

(define-class <repository> (<object>)
  (properties type: <vector> init-value: '#())
  (uuid type: <uuid>)
  (next-oid type: <fixnum> init-value: 100)
  (oid-index type: <hash-integer-table>)
  (worlds type: <string-table>)
  (intern-sxml-table type: <generic-table>))

(define-class <world> (<pobject>)
  (name type: <string>)
  (indices type: <symbol-table>))

;;;

(define-class <content-node> (<pobject>)
  (type type: <symbol>)
  (name type: <string>)
  content)

(define-class <user> (<content-node>))

;;;

(define $pivots (vector <repository>
                        hash-code
                        equal?
                        <uuid>
                        <audit-record>
                        <world>
                        <content-node>
                        <user>))

(define (content-node-class type)
  (case type
    ((user) <user>)
    (else <content-node>)))

;;;

(define (make-uuid)
  (let ((u (bvec-alloc <uuid> 16)))
    (for-each (lambda (i)
                (bvec-set! u i (random 256))
                (values))
              (range 16))
    u))

(define (init-repository file)
  (let ((ps (create-persistent-store file)))
    (register-indirect-page ps 10 $pivots)
    (let ((r (make <repository>
                   uuid: (make-uuid)
                   oid-index: (make-fixnum-table)
                   worlds: (make-string-table)
                   intern-sxml-table: (make-table))))
      ;;
      (add-home-world-and-root-user! r)
      ;;
      (commit ps r)
      (close-persistent-store ps))))

(define (intern-sxml (self <repository>) node)
  (let ((t (intern-sxml-table self)))
    ;;
    (define (new n)
      (if (pair? n)
          (cons (car n) (map intern (cdr n)))
          n))
    ;;
    (define (intern n)
      (or (table-lookup t n)
          (let ((n (new n)))
            (table-insert! t n n)
            n)))
    ;;
    (intern node)))

(define (add-home-world-and-root-user! r)
  (let* ((a (make <audit-record>
                  principal: '#missing
                  timestamp: (time)))
         (u (make <user>
                  name: "root"
                  oid: 2
                  creation-audit: a
                  modification-audit: a
                  type: 'user
                  content: (intern-sxml r '(user (@ (name "root"))
                                                 (authority (super))))))
         (h (make <world>
                  name: "home"
                  creation-audit: a
                  modification-audit: a
                  oid: 1
                  indices: (make-symbol-table))))
    ;;
    (set-principal! a u)
    (table-insert! (oid-index r) (oid h) h)
    (table-insert! (oid-index r) (oid u) u)
    ;;
    (table-insert! (worlds r) (name h) h)
    ;;
    (let ((t (make-string-table)))
      (table-insert! (indices h) 'user t)
      (table-insert! t (name u) u))))

;;;

(define-thread-var *current-pstore* #f)
(define-thread-var *current-world* #f)
(define-thread-var *current-audit* #f)

(define (access file)
  (let ((ps (open-persistent-store file)))
    (register-indirect-page ps 10 $pivots)
    (set! *current-pstore* ps)
    (values)))
    
(define (alloc-next-oid)
  (let* ((rep (current-repository))
         (i (next-oid rep)))
    (set-next-oid! rep (+ i 1))
    i))

(define (create-world world)
  (let ((rep (current-repository)))
    (or (table-lookup (worlds rep) world)
        (let ((w (make <world>
                       name: world
                       creation-audit: *current-audit*
                       modification-audit: *current-audit*
                       oid: (alloc-next-oid)
                       indices: (make-symbol-table))))
          (table-insert! (oid-index rep) (oid w) w)
          (table-insert! (worlds rep) world w)
          w))))
        
(define (land-on world)
  (set! *current-world* 
        (or (table-lookup (worlds (current-repository)) world)
            (error "No such world: ~s" world))))

(define (sync)
  (commit *current-pstore*))

(define (current-world)
  *current-world*)

(define (current-repository)
  (root-object *current-pstore*))

;;;

(define (home-world)
  (table-lookup (oid-index (current-repository)) 1))

(define (root-user)
  (table-lookup (oid-index (current-repository)) 2))


(define (xbegin u)
  (set! *current-audit*
        (make <audit-record>
              principal: u
              timestamp: (time))))

(define (xend)
  (let ((ps *current-pstore*))
    (set-commit-id! *current-audit* (pstore-next-commit-id ps))
    (commit ps)
    (set! *current-audit* #f)))

(define (store sxml)
  (let* ((rep (current-repository))
         (type (car sxml))
         (name (intern-sxml rep (xpath-str sxml "@id")))
         (mix (indices (current-world)))
         (ix (or (table-lookup mix type)
                 (let ((ix (make-string-table)))
                   (table-insert! mix type ix)
                   ix)))
         (cint (intern-sxml rep sxml)))
    ;;
    (if (table-key-present? ix name)
        (let (((existing <content-node>) (table-lookup ix name)))
          (if (not (eq? cint (content existing)))
              (begin
                (set-content! existing cint)
                (set-modification-audit! existing *current-audit*))))
        (let* (((i <fixnum>) (next-oid rep))
               (c (make (content-node-class type)
                        creation-audit: *current-audit*
                        modification-audit: *current-audit*
                        oid: i
                        type: type
                        name: name
                        content: cint)))
          (set-next-oid! rep (add1 i))
          (table-insert! (oid-index rep) (oid c) c)
          (table-insert! ix name c)))))

#|
(access "/tmp/i.rep")

(land-on "home")
(xbegin (root-user))
(store '(user (@ (id "alice")) (fullname "Alice Lerner")))
(store '(user (@ (id "bob")) (fullname "Bob Alexander")))
(xend)

|#

(define (look)
  (let ((q (make-dequeue)))
    (for-each
     (lambda (info)
       (let ((label (cadr info))
             (key (car info)))
         ;;
         (format #t "~a::\n" label)
         (let ((extent (sort
                        (cond
                         ((table-lookup (indices (current-world)) key)
                          => value-sequence)
                         (else '()))
                        (lambda (a b)
                          (string-ci<=? (name a) (name b))))))
           (for-each 
            (lambda (o)
              (let* ((i (dequeue-count q))
                     (m (modification-audit o)))
                (dequeue-push-back! q o)
                (format #t "    ~d. ~a  (~a ~a)\n" i (name o)
                        (name (principal m))
                        (time->string (timestamp m)))))
            extent))))
     '((template "Templates")
       (user "Users")
       (static "Static Content")
       (webnode "Web Nodes")
       (image "Graphic Images")))
    q))

(define (search-top-level #key type sxml)
  (let ((key (intern-sxml (current-repository) sxml)))
    (call-with-current-continuation
     (lambda (exit)
       (table-for-each
        (table-lookup (indices (current-world)) type)
        (lambda (h k v)
          (if (memq key (sxml:children (content v)))
              (exit v))))
       (values)))))
  
(define (lookup #key type name)
  (cond
   ((table-lookup (indices (current-world)) type)
    => (lambda (ix)
         (table-lookup ix name)))
   (else
    #f)))

(define (template->quasiquote t)
  (define (rec t)
    (if (pair? t)
        (case (car t)
          ((t:unquote)
           (list 'unquote
                 (read (open-input-string (xpath:node->string t)))))
          ((t:unquote-splicing)
           (list 'unquote-splicing
                 (read (open-input-string (xpath:node->string t)))))
          (else
           (cons (car t) (map rec (cdr t)))))
        t))
  ;;
  (list 'quasiquote (rec t)))

(define (import-image name type file)
  (let ((s (file->string file)))
    ;;
    (store
     `(static (@ (id ,name)
                 (type ,type)
                 (encoding "applicaton/octet-stream"))
             (content (@ (size ,(to-string (string-length s))))
                      ,s)))))

(define (import-text-file name type file)
  (let ((s (string-split (file->string file) #\newline)))
    (store
     `(static (@ (id ,name)
                 (type ,type)
                 (encoding "applicaton/octet-stream"))
              (content
               ,@(map (lambda (l)
                        (string-append l "\n"))
                      s))))))
              

(define (store-template name lang args expr)
  (define (fx t)
    (if (pair? t)
        (case (car t)
          ((unquote)
           (list 't:unquote (~ "~s" (cadr t))))
          ((unquote-splicing)
           (list 't:unquote-splicing (~ "~s" (cadr t))))
          (else
           (cons (car t) (map fx (cdr t)))))
        t))
  ;;
  (store
   `(template (@ (id ,name)
                 (lang ,lang))
              (args
               ,@(map (lambda (a)
                        (list 'arg (to-string a)))
                      args))
              (expr
               ,(fx expr)))))

(define (compile-template sxml)
  (let ((lang (xpath-str sxml "@lang"))
        (args (map (lambda (m)
                     (string->symbol (xpath:node->string m)))
                   (xpath () sxml "args/arg"))))
    (eval `(lambda ,args
             ,(template->quasiquote (cadar (xpath () sxml "expr")))))))

#|
(compile-template (content (lookup type: 'template name: "common")))
|#
              
(define (scan-directory name)
  (map xpath:node->string
       (xpath ()
              (content (lookup type: 'directory 
                               name: name))
              "list/item")))
  
