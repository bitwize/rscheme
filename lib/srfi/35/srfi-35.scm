
;;; SRFI-35 has it's own notion of type membership...

(define-class <srfi-35-condition-type> (<condition>)
  name
  parent
  (field-names type: <list>)
  (all-field-names type: <vector>)
  all-fields-length)

(define-class <srfi-35-condition> (<condition>) :abstract)

(define-class <srfi-35-leaf-condition> (<srfi-35-condition>)
  the-condition-type
  (fields init-value: '#()))

(define-class <srfi-35-compound-condition> (<srfi-35-condition>)
  (membership init-value: '()))

(define-method condition-has-type? ((self <condition>) type)
  (eq? type <condition>))

(define-method condition-has-type? ((self <srfi-35-compound-condition>) type)
  (any? (lambda (m)
          (condition-has-type? m type))
        (membership self)))

(define-method condition-has-type? ((self <srfi-35-leaf-condition>) type)
  (condition-subtype? (the-condition-type self) type))

(define (condition-subtype? c type)
  (let loop ((c c))
    (cond
     ((not c) #f)
     ((eq? c type) #t)
     (else (loop (parent c))))))
      

(define-method condition-ref* ((self <srfi-35-compound-condition>) key contn)
  (define (scan l)
    (if (null? (cdr l))
        (condition-ref* (car l) key contn)
        (condition-ref* (car l) key (lambda ()
                                      (scan (cdr l))))))
  (if (null? (membership self))
      (contn)
      (scan (membership self))))

(define-method condition-ref* ((self <srfi-35-leaf-condition>) key contn)
  (cond
   ((vmemq key (all-field-names (the-condition-type self)))
    => (lambda (k)
         (vector-ref (fields self) k)))
   (else
    (contn))))

(define-method condition-ref ((self <srfi-35-condition>) key)
  (condition-ref* self 
                  key 
                  (lambda ()
                    (error "condition-ref: No such field: ~s" key))))

(define (make-compound-condition (first <srfi-35-leaf-condition>) . rest)
  (if (null? rest)
      first
      (begin
        ;;
        (for-each
         (lambda (a)
           (if (not (instance? a <srfi-35-leaf-condition>))
               (error "make-compound-condition: Only works on SRFI-35 conditions")))
         (cons first rest))
        ;;
        (make <srfi-35-compound-condition>
              membership: (cons first rest)))))

(define-method all-fields ((self <boolean>))
  '())

(define-method all-fields ((self <<class>>))
  '())

(define-method all-fields ((self <srfi-35-condition-type>))
  (append (all-fields (parent self)) (field-names self)))

(define (make-condition-type id parent field-names)
  (if (not (or (eq? parent <condition>)
               (instance? parent <srfi-35-condition-type>)))
      (error "`make-condition-type' can only be used to subtype SRFI-35 conditions"))
  ;;
  (let ((all (all-fields parent)))
    ;;
    (for-each
     (lambda (n)
       (if (not (symbol? n))
           (error "`make-condition-type': field name `~s' is not a symbol" n))
       (if (memq n all)
           (error "`make-condition-type': field name `~s' conflicts" n)))
     field-names)
    ;;
    (make <srfi-35-condition-type>
          name: id
          parent: (if (eq? parent <condition>) #f parent)
          field-names: field-names
          all-field-names: (vector-append
                            (if (eq? parent <condition>) 
                                '#()
                                (all-field-names parent))
                            (list->vector field-names))
          all-fields-length: (+ (length field-names) (length all)))))

(define (condition? x)
  (instance? x <condition>))

(define (condition-type? x)
  (or (eq? x <condition>)
      (instance? x <srfi-35-condition-type>)))

(define (make-condition type . args)
  (if (eq? type <condition>)
      (if (pair? args)
          (error "make-condition: field key unknown for &condition: ~s"
                 (car args))
          (make <srfi-35-leaf-condition>
                the-condition-type: #f))
      (apply make-condition* type args)))
    
(define-method extract-condition* ((self <srfi-35-compound-condition>) type)
  (let loop ((m (membership self)))
    (if (null? m)
        #f
        (or (extract-condition* (car m) type)
            (loop (cdr m))))))

(define-method extract-condition* ((self <srfi-35-leaf-condition>) type)
  (let ((t (the-condition-type self)))
    (cond
     ((eq? t type) self)
     ((condition-subtype? t type)
      (make <srfi-35-leaf-condition>
            the-condition-type: type
            fields: (subvector (fields self) (all-fields-length type))))
     (else
      #f))))

(define (extract-condition (self <srfi-35-condition>) type)
  (or (extract-condition* self type)
      (error "extract-condition: No ~s in ~s" type self)))

  

(define (make-condition* (type <srfi-35-condition-type>) . args)
  (let ((data (make-vector (all-fields-length type) #f))
        (flag (make-vector (all-fields-length type) #f))
        (names (list->vector (all-fields type)))
        (count 0))
    ;;
    (let loop ((l args)
               (count 0))
      (if (null? l)
          (if (= count (vector-length data))
              (make <srfi-35-leaf-condition>
                    the-condition-type: type
                    fields: data)
              (error "make-condition: field values not supplied: ~s" 
                     (select identity
                             (map (lambda (i)
                                    (if (vector-ref flag i)
                                        #f
                                        (vector-ref names i)))
                                  (range (vector-length data))))))
          (cond
           ((vmemq (car l) names)
            => (lambda (k)
                 (if (vector-ref flag k)
                     (error "make-condition: field supplied twice: ~s" 
                            (car l)))
                 (vector-set! flag k #t)
                 (vector-set! data k (cadr l))
                 (loop (cddr l) (+ count 1))))
           (else
            (error "make-condition: field key unknown for ~s: ~s" 
                   type 
                   (car l))))))))

(define-constant &condition <condition>)

;;;
;;;  Macros
;;;

(define-syntax (define-condition-type type super predicate . fields)
  (letrec-syntax ((deff 
                    (syntax-form ())
                    (syntax-form ((field accessor) . more)
                      (define (accessor item)
                        (condition-ref item (mquote field)))))
                  (flist
                   (syntax-form () '())
                   (syntax-form ((field accessor) . more)
                     (cons (mquote field) (flist . more)))))
    ;;
    (define type (make-condition-type (mquote type)
                                      super
                                      (flist . fields)))
    (deff . fields)))

  

;;;  testing...

(define &c (make-condition-type '&c &condition '(x)))
(define &d (make-condition-type '&d &c '(y)))
(define &e (make-condition-type '&e &c '(z)))

(define a (make-condition &d 'x 3 'y 4))
(define b (make-condition &e 'x 10 'z 40))

(define cc (make-compound-condition a b))

