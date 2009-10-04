
(define *all-indices* '())

(define-class <index> (<object>) :abstract
  name
  subject-class
  (slot-name type: <symbol>)
  (index type: <fixnum>)                ; slot index
  table-ctor
  initter)

(define (all-indices-for type)
  (select (lambda ((ix <index>))
            (eq? (subject-class ix) type))
          *all-indices*))

(define-class <unique-index> (<index>))
(define-class <nonunique-index> (<index>))

(define-method index-query ((self <unique-index>) key required?)
  (let ((tbl (get-index-table self)))
    (if (table-key-present? tbl key)
        (list (table-lookup tbl key))
        (if required?
            (signal (make <no-such-key>
                          collection: tbl
                          key: key))
            '()))))

(define-method index-query ((self <nonunique-index>) key required?)
  (or (table-lookup (get-index-table self) key) 
      (if required?
          (signal (make <no-such-key>
                        collection: (get-index-table self)
                        key: key))
          '())))

(define (get-index-table (self <index>))
  (let ((ix (indices (current-universe)))
        (n (name self)))
    (or (table-lookup ix n)
        (let ((t ((table-ctor self))))
          (table-insert! ix n t)
          t))))

(define (make-init-proc (self <index>))
  (lambda (item)
    (update-index self '#uninit (gvec-ref item (index self)) item)))

(define-method update-index ((self <nonunique-index>) old-key new-key value)
  (let* ((tbl (get-index-table self)))
    ;;
    (if (not (eq? old-key '#uninit))
        (let ((old-chain (table-lookup tbl old-key)))
          (assert (memq value old-chain))
          (let ((edited-chain (delq! value old-chain)))
            (if (not (eq? edited-chain old-chain))
                (table-insert! tbl old-key edited-chain)))))
    ;;
    (table-insert! tbl new-key (cons value
                                     (or (table-lookup tbl new-key) '())))
    new-key))

(define-method update-index ((self <unique-index>) old-key new-key value)
  (let ((tbl (get-index-table self)))
    (if (not (eq? old-key '#uninit))
        (assert (eq? (table-remove! tbl old-key) value)))
    (if (table-key-present? tbl new-key)
        (error "duplicate key ~s of ~s" new-key value))
    (table-insert! tbl new-key value)
    new-key))

(define-glue (indexed-setter-template self)  :template
  literals: ((& invalid-setter-arg)
             (& update-index))
{
obj slotnum;
obj setter;

    setter = envt_reg;
    USE_FUNCTION_ENVT();
    COUNT_ARGS(2);
    slotnum = LEXREF0(0);
    assert( OBJ_ISA_PTR(self) 
	    && FXWORDS_TO_RIBYTES(slotnum) < SIZEOF_PTR(self) );

    if (instance_p( REG1, LEXREF0(1) ))
     {
       obj oldv = gvec_read( self, FXWORDS_TO_RIBYTES(slotnum) );

       gvec_write( self, FXWORDS_TO_RIBYTES(slotnum), REG1 );
       REG3 = self;
       REG0 = LEXREF0(2);
       REG1 = oldv;
       REG2 = REG1;
       APPLYF( 4, TLREF(1) );
     }
    else
     {
       REG2 = REG0;
       REG0 = setter;
       APPLYF( 3, TLREF(0) );
     }
})

(define (make-index-setter (self <setter>) (ix <index>))
  (make <setter>
        template: indexed-setter-template
        environment: (make-gvec <binding-envt>
                                (index self)
                                (type-restriction self)
                                ix)
        function-specializers: (function-specializers self)
        sync-method: #f
        index: (index self)
        type-restriction: (type-restriction self)
        slot-descriptor: (slot-descriptor self)))


;;;


  

(define (make-index name class slot-name #key 
                    (unique? default: #f)
                    (setter-generic default: #f))
  (let* ((s (slot-descriptor-by-name class slot-name))
         (mktable (cond
                   ((eq? (type-restriction s) <string>)
                    make-string-table)
                   ((eq? (type-restriction s) <symbol>)
                    make-symbol-table)
                   (else
                    make-table)))
         (ix (if unique?
                 (make <unique-index>
                       name: name
                       subject-class: class
                       slot-name: slot-name
                       initter: #f
                       index: (index s)
                       table-ctor: mktable)
                 (make <nonunique-index>
                       name: name
                       subject-class: class
                       slot-name: slot-name
                       index: (index s)
                       initter: #f
                       table-ctor: mktable))))
    (set-initter! ix (make-init-proc ix))
    ;;
    (set! *all-indices* (cons ix *all-indices*))
    ;;
    (if setter-generic
        (target-add-method setter-generic
                           (make-index-setter (setter s) ix)))
    ix))

(define-macro (define-index name (slot class) . opt)
  (let ((settern (symbol-append "set-" slot "!")))
    (if (memq ':unique opt)
        `(begin
           (define ,name
             (make-index ',name 
                         ,class 
                         ',slot
                         unique?: #t
                         setter-generic: ,settern))
           (define-method initialize ((self ,class))
             (initialize-indices self)))
        `(begin
           (define-method initialize ((self ,class))
             (initialize-indices self))
           (define ,name 
             (make-index ',name
                         ,class
                         ',slot
                         setter-generic: ,settern))))))

(define-method initialize-indices ((self <object>))
  (for-each (lambda ((ix <index>))
              (if (instance? self (subject-class ix))
                  (begin
                    ;(format #t "~s: initializing index: ~s\n" self ix)
                    ((initter ix) self))))
            *all-indices*))

