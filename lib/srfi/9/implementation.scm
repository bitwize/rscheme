
(define-class <record> (<object>))

(define-class <<record-type>> (<<standard-class>>)
  field-specs)

(define-method write-object ((self <<record-type>>) port)
  (format port "#{the record type ~s}" (class-name self)))

(define-method write-object ((self <record>) port)
  (format port "#[a ~s]" (class-name (object-class self))))

(define-method print ((self <record>))
  (format #t "~s\n" self)
  (let ((fs-list (field-specs (object-class self))))
    (for-each (lambda (i fs)
                (format #t "   [~d] ~s = ~#*@60s\n" 
                        i
                        (car fs) 
                        (gvec-ref self i)))
              (range (length fs-list))
              fs-list)
    self))
            
(define-macro (define-record-type type-name 
                (constructor-name . field-tags)
                predicate-name
                . field-specs)
  (define (fs-name fs) (car fs))
  (define (fs-accessor fs) (cadr fs))
  (define (fs-modifier fs) (and (pair? (cddr fs)) (caddr fs)))
  ;
  `(begin
     (define-class ,type-name (<record>) metaclass: <<record-type>>
       field-specs: ',field-specs)
     ;
     (define (,constructor-name ,@field-tags)
       (make-gvec ,type-name
                  ,@(map (lambda (fs)
                           (if (memq (fs-name fs) field-tags)
                               (fs-name fs)
                               #f))
                         field-specs)))
     ;
     (define (,predicate-name item)
       (instance? item ,type-name))
     ;
     ,@(map (lambda (i fs)
              `(define (,(fs-accessor fs) (self ,type-name))
                 (gvec-ref self ,i)))
            (range (length field-specs))
            field-specs)
     ,@(map (lambda (i fs)
              (if (fs-modifier fs)
                  `(define (,(fs-modifier fs) (self ,type-name) value)
                     (gvec-set! self ,i value)
                     (values))
                  '(begin)))
            (range (length field-specs))
            field-specs)))
     

(define-record-type pare (kons x y) pare? (x kar set-kar!) (y kdr))
