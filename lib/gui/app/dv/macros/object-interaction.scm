,(use util.pretty-print)

(define *test0*
  '((actor name: "Actor")
    (group name: "elements"
           members: ((actor name: "X")
                     (actor name: "Y")
                     (actor name: "Z")
                     (actor name: "...")))
    (group name: "external systems"
           members: ((actor name: "A")
                     (actor name: "B")
                     (actor name: "...")))
    (message from: "Actor"              ; no data description
             to: "X"
             what: "input")
    (group name: "interactions"
           members: ((message from: "X"
                              to: "B")
                     (message from: "B"
                              to: "X")))
    (message from: "X"                  ; no data description
             to: "Actor"
             what: "output")))

(define *test*
  '((actor id: ft name: "Field\nTech" width: 48)
    (actor id: hp name: "Host\nProcessor" width: 48)
    (actor id: nms name: "\nNMS")
    (actor id: ms name: "Management\nServer" width: 64)
    ;;
    (message from: ft to: hp what: "power")
    (message from: hp to: ft what: "\"learning\"")
    (group name: "discovery"
           members: ((message from: nms to: hp what: "ping")
                     (message from: hp to: nms what: "ok")
                     (message from: nms to: hp what: "get(id)")
                     (message from: hp to: nms what: "\"BSW\"")
                     (message from: nms to: ms what: "discovered")))
    (message from: ms to: hp what: "get(info)")
    (message from: hp to: ft what: "\"run\"")
    (message from: hp to: ms what: "info")))

(define $oid-y-spacing          8)
(define $oid-y-spacing-w-text  16)
(define $oid-x-spacing         48)

(define-class <oi-group> (<object>)
  parent
  group-contents
  label)

(define-method group-contents-list ((self <oi-group>))
  (vector->list (dequeue-state (group-contents self))))

(define-class <oi-entity> (<object>)
  parent
  (x init-value: #f)
  (label type: <string>))


(define-class <oi-msg> (<object>)
  parent
  (y init-value: #f)
  label                         ; (or <string> #f)
  (from type: <oi-entity>)
  (to type: <oi-entity>))

(define-class <oi-diagram> (<object>)
  id-table
  entities
  messages
  root-group
  current-x
  current-y)

(define (parse-oi-decl (oid <oi-diagram>) parent decl)
  (case (car decl)
    ((actor) (apply parse-oi-entity oid parent (cdr decl)))
    ((group) (apply parse-oi-group oid parent (cdr decl)))
    ((message) (apply parse-oi-message oid parent (cdr decl)))
    (else (error "unknown oi type: ~s" (car decl)))))

(define (parse-oi-group (oid <oi-diagram>)
                        parent
                        #key
                        name
                        members)
  (let ((p (make <oi-group>
                 parent: parent
                 label: name
                 group-contents: (make-dequeue))))
    (if parent
        (dequeue-push-back! (group-contents parent) p))
    (for-each
     (lambda (m)
       (parse-oi-decl oid p m))
     members)
    p))

(define (parse-oi-message (oid <oi-diagram>)
                          parent
                          #key
                          from
                          to
                          (id default: #f)
                          (what default: #f))
  (set-current-y! oid (- (current-y oid) 
                         (if what
                             $oid-y-spacing-w-text
                             $oid-y-spacing)))
  (let ((m (make <oi-msg>
                 parent: parent
                 y: (current-y oid)
                 label: what
                 from: (table-lookup (id-table oid) (to-string from))
                 to: (table-lookup (id-table oid) (to-string to)))))
    (dequeue-push-back! (group-contents parent) m)
    (dequeue-push-back! (messages oid) m)
    m))

(define (parse-oi-entity (oid <oi-diagram>)
                         parent
                         #key
                         name
                         (id default: #f)
                         (width default: $oid-x-spacing))
  (let* ((w (if (string? width)
                (string-width $oi-afm width)
                width))
         (e (make <oi-entity>
                  parent: parent
                  x: (+ (current-x oid) (/ w 2))
                  label: name)))
    (dequeue-push-back! (group-contents parent) e)
    (dequeue-push-back! (entities oid) e)
    (set-current-x! oid (+ (current-x oid) w))
    (table-insert! (id-table oid) (to-string (or id name)) e)
    e))
  
 
(define $oi-corner-x 50)
(define $oi-corner-y 500)

(define (parse-object-interaction oi)
  (let* ((oid (make <oi-diagram>
                    current-x: $oi-corner-x
                    current-y: $oi-corner-y
                    root-group: #f
                    entities: (make-dequeue)
                    messages: (make-dequeue)
                    id-table: (make-string-table))))
    ;;
    (set-root-group! oid (parse-oi-group oid #f name: "$root" members: oi))
    oid))

;;;

(load "arrow.scm")

(define (text-block x y lineheight halign valign font str)
  (let ((l (string-split str #\newline)))
    (let loop ((l l)
               (r '())
               (y (if (eq? valign 'bottom)
                      (+ y (* (- (length l) 1) lineheight))
                      y)))
      (if (null? l)
          (if (null? (cdr r))
              (car r)
              (cons 'group (reverse! r)))
          (loop (cdr l)
                (cons `(text string: ,(car l)
                             origin-x: ,x
                             origin-y: ,y
                             alignment: ,halign
                             font: ,font)
                      r)
                (- y lineheight))))))

(define-method extern-rep ((self <oi-msg>) oid)
  (let* ((from-x (x (from self)))
         (to-x (x (to self)))
         (upstream? (< to-x from-x))
         (t (if (label self)
                (list (text-block (if upstream?
                                      (- from-x 6)
                                      (+ from-x 6))
                                  (+ (y self) 2)
                                  12
                                  (if upstream?
                                      'right
                                      'left)
                                  'bottom
                                  $oi-font
                                  (label self)))
                '())))
    `(group
      ,@t
      ,(hline-with-arrow (make-point from-x (y self))
                         (make-point to-x (y self))
                         (if upstream?
                             -1
                             1)))))
     
(define $oi-font '(font "Times" "Roman" 10))
(define $oi-afm (apply get-text-font (cdr $oi-font)))

(define $oi-group-width 20)
(define $oi-group-height 30)

(define (entity-group (self <oi-group>) oid)
  (braced-group self
                oid
                (+ $oi-corner-y $oi-group-height)
                (lambda (y node)
                  (make-point (x node) y))
                $oi-group-height))

(define (msg-group (self <oi-group>) oid)
  (braced-group self
                oid
                (current-x oid)
                (lambda (x node)
                  (make-point x (y node)))
                $oi-group-width))

(define (braced-group (self <oi-group>) oid initial-pt brace-posn nest-delta)
  (let ((sub0 (group-contents-list self)))
    (let loop ((sub sub0)
               (x initial-pt)
               (r '()))
      (if (null? sub)
          (let ((brace `(script 
                         name: "brace"
                         instance-vars: ((*from* ,(brace-posn x (car sub0)))
                                         (*to* ,(brace-posn x (last sub0)))))))
            (if (null? r)
                brace
                (values (cons* 'group brace r) (+ x nest-delta))))
          (if (instance? (car sub) <oi-group>)
              (bind ((sub-brace sub-x (braced-group (car sub)
                                                    oid
                                                    x
                                                    brace-posn
                                                    nest-delta)))
                (loop (cdr sub) sub-x (cons sub-brace r)))
              (loop (cdr sub) x r))))))
        

(define-method extern-rep ((self <oi-group>) oid)
  ;(print (dequeue-state (group-contents self)))
  (if (instance? (dequeue-ref (group-contents self) 0) <oi-msg>)
      (msg-group self oid)
      (entity-group self oid)))

(define-method extern-rep ((self <oi-entity>) oid)
  (list 'group
        (text-block (x self)
                    (+ $oi-corner-y 3)
                    12
                    'center
                    'bottom
                    $oi-font
                    (label self))
        `(line start-x: ,(x self)
               start-y: ,$oi-corner-y
               end-x: ,(x self)
               end-y: ,(- (current-y oid) $oid-y-spacing))))
          
(define-method extern-rep ((self <oi-diagram>) oid)
  (append
   '(group)
   (list (cons 'group
               (map (rcurry extern-rep oid)
                    (vector->list (dequeue-state (entities self))))))
   (list (cons 'group
               (map (rcurry extern-rep oid)
                    (vector->list (dequeue-state (messages self))))))
   (let ((gs (select (lambda (tl)
                       (instance? tl <oi-group>))
                     (vector->list
                      (dequeue-state (group-contents (root-group oid)))))))
     ;(print gs)
     (if (null? gs)
         '()
         (list (cons 'group (map (rcurry extern-rep oid) gs)))))))

(define (tpp)
  (let ((x (parse-object-interaction *test*)))
    (pp (extern-rep x x))
    x))

(define (tfw)
  (open-macro
   (lambda ()
     (let ((x (parse-object-interaction *test*)))
       (extern-rep x x)))))

,(use paths)

(define (teps)
  (let ((f (getenv "OI")))
    (macro->eps
     (lambda ()
       (let ((x (parse-object-interaction 
                 (if f
                     (call-with-input-file f read)
                     *test*))))
         (extern-rep x x)))
     (if f
         (pathname->os-path (extension-related-path (string->file f) "eps"))
         "/tmp/test00.eps"))))

(define (read-form-list port)
  (let loop ((r '()))
    (let ((i (read port)))
      (if (eof-object? i)
          (reverse r)
          (loop (cons i r))))))

(define (read-dv-macro port)
  (let* ((content (read-form-list port))
         (oi (parse-object-interaction content))
         (ex (extern-rep oi oi)))
    ;(pp ex)
    (document-w-macro (lambda () ex))))
