
(define-class <parameter-object> (<function>)
  (converter init-value: #f :sealed)
  (name init-value: #f :sealed))

(define-method write-object ((self <parameter-object>) port)
  (if (name self)
      (format port "#[<parameter> ~s]" (name self))
      (format port "#[<parameter> ~a]" (machine-bits->string self))))

(define (convert-parameter-arg (self <parameter-object>) value)
  (let ((f (converter self)))
    (if f
        (f value)
        value)))

(define (set-with-conversion (self <parameter-object>) value)
  (indirect-thread-var-set! self ((converter self) value))
  (values))

(define (set-no-conversion (self <parameter-object>) value)
  (indirect-thread-var-set! self value)
  (values))

(define-glue (parameter-template) :template
  literals: ((& indirect-thread-var-ref)
             (& set-no-conversion)
             (& set-with-conversion))
{
  obj me = THIS_FUNCTION();     /* we are our own key! */

  if (arg_count_reg == 0) {
    REG0 = me;
    APPLYF( 1, TLREFB(0) );
  } else {
    obj cnv = gvec_ref( me, SLOT(1) );

    if (EQ( cnv, FALSE_OBJ )) {
      REG1 = REG0;
      REG0 = me;
      APPLYF( 2, TLREFB(1) );
    } else {
      REG1 = REG0;
      REG0 = me;
      APPLYF( 2, TLREFB(2) );
    }
  }
})

(define (make-parameter init 
                        #optional
                        converter-proc
                        name)
  (let ((p (make <parameter-object>
                 template: parameter-template
                 converter: converter-proc
                 name: name)))
    (if converter-proc
        (add-thread-var! p (converter-proc init))
        (add-thread-var! p init))
    p))

(define-syntax extend-parameterization
  (syntax-form (base) base)
  (syntax-form (base (key value) . more)
    (make-gvec <vector> 
               key 
               value
               (extend-parameterization base . more))))

(define-syntax parameterize
  (syntax-form (bindings . body)
    (dynamic-call-thunk
     #f
     #f
     (lambda () (begin . body))
     (get-dynamic-state-reg)
     (let ((direct (get-thread-state-reg)))
       (make-gvec 
        <vector>
        (extend-parameterization (gvec-ref direct 0) . bindings)
        (gvec-ref direct 1)             ; *input-port*
        (gvec-ref direct 2)             ; *output-port*
        (gvec-ref direct 3))))))        ; *handler-chain*


     
