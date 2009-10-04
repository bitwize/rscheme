,(use sort tables)

(define-class <domain> (<object>)    ; an authority administration domain
  (name type: <string>)
  (parent init-value: #f))

(define-class <priviledge-group> (<object>)     ; a group of priviledges
  (name type: <string>)
  (priviledges type: <vector>))

(define-class <authority> (<object>)            ; an authority record
  (owner)
  (with-respect-to type: <domain>)
  (priviledge-group type: <priviledge-group>)
  (access-type type: <fixnum>))                 ; 0=DENY, 1=GRANT

;;; A claim about whether or not access is allowed from a given host

(define-class <access> (<object>)
  (owner)
  (user type: <string>)
  (host type: <string>)
  (options type: <vector> init-value: '#()))

(define-method run-ident-check? ((self <access>))
  (and (vmemq 'ident (options self)) 
       #t))

(define-method write-object ((self <domain>) port)
  (format port "#[~a ~a]" (class-name (object-class self)) (name self)))

(define-method write-object ((self <authority>) port)
  (format port "#[<authority> ~a ~a ~a ~a]"
          (owner self)
          (name (with-respect-to self))
          (name (priviledge-group self))
          (case (access-type self)
            ((0) "DENY")
            ((1) "GRANT"))))

(define (domain-heritage (self <domain>))
  (if (parent self)
      (cons self (domain-heritage (parent self)))
      (list self)))

(define (check-authority with-respect-to priviledge exploded)
  (let loop ((c with-respect-to))
    (if c
        (or (table-lookup exploded (cons (name c) priviledge))
            (loop (parent c)))
        #f)))

(define (string-symbol-pair-hash (p <pair>))
  (fixnum+ (string->hash (car p))
           (symbol->hash (cdr p))))

(define (explode-authority-vector (v <vector>))
  (let ((exploded (make-table equal? string-symbol-pair-hash))
        (conflicts '()))
    (for-each
     (lambda ((a <authority>))
       (let ((component-name (name (with-respect-to a)))
             (a-type (access-type a)))
         (for-each
          (lambda (priv-symbol)
            (let* ((k (cons component-name priv-symbol))
                   (x (table-lookup exploded k)))
              (if x
                  (if (not (eq? x a-type))
                      (set! conflicts 
                            (cons k conflicts)))
                  (table-insert! exploded k a-type))))
          (vector->list (priviledges (priviledge-group a))))))
     (vector->list v))
    (values exploded conflicts)))

(define (smaller-component? a b)
  (let loop ((a a))
    (if (eq? a b)
        #t
        (if (parent a)
            (loop (parent a))
            #f))))

;;;

#|
(define (mkc n o #optional p)
  (make <domain> 
        name: n
        owner: o
        description: (format #f "The `~a' component" n)
        parent: p))

(define (make-auth c g t)
  (make <authority>
        owner: "root"
        with-respect-to: c
        priviledge-group: g
        access-type: (case t
                       ((grant) 1)
                       ((deny) 0)
                       (else (error "Bad access type: ~s" t)))))

(define (t)
  (let* ((root "root")
         (w (mkc "world" root))
         (b (mkc "B" root w))
         (c (mkc "C" root w))
         (bx (mkc "BX" root b))
         (by (mkc "BY" root b))
         ;
         (pg1 (make <priviledge-group>
                    name: "PG1"
                    priviledges: '#(a b c d e)))
         (pg2 (make <priviledge-group>
                    name: "PG2"
                    priviledges: '#(e f g h)))
         ;
         (avec (vector (make-auth w pg2 'grant)
                       (make-auth c pg1 'grant)
                       (make-auth b pg2 'deny)
                       (make-auth b pg1 'grant)))
         (x (explode-authority-vector avec)))
    ;;
    (for-each
     (lambda (test)
       (format #t "~s? ==> ~s\n" test (check-authority (car test)
                                                       (cdr test)
                                                       x)))
     (list (cons bx 'a)
           (cons b 'a)
           (cons bx 'e)
           (cons b 'e)
           (cons bx 'h)
           (cons w 'a)
           (cons w 'e)
           (cons w 'h)
           (cons c 'a)
           (cons c 'e)
           (cons c 'h)))
    x))
|#
