
(define-class <mrg32k5a-state> (<object>) :bvec)

(define (random-source? obj)
  (instance? obj <mrg32k5a-state>))

(define-macro (define-rand-glue args . body)
  `(define-safe-glue ,args
     properties: ((other-h-files "mrg32k5a.h")
                  (other-c-files "mrg32k5a.c"))
     type-handler: (<mrg32k5a-state> (direct-instance? <mrg32k5a-state>)
                       ("struct mrg32k5a_state *~a"
                        "((struct mrg32k5a_state *)PTR_TO_DATAPTR( ~a ))"))
     ,@body))


(define-rand-glue (make-random-source)
  literals: ((& <mrg32k5a-state>))
{
  obj r = bvec_alloc( sizeof( struct mrg32k5a_state ), TLREF(0) );
  struct mrg32k5a_state *p = PTR_TO_DATAPTR( r );

  mrg32k5a_init( p, 0x1234567, 0x89ABCD );
  REG0 = r;
  RETURN1();
})

(define-rand-glue (random-source-pseudo-randomize! (self <mrg32k5a-state>)
                                                   (i <raw-int>)
                                                   (j <raw-int>))
{
  mrg32k5a_init( self, i, j );
  RETURN0();
})

(define-rand-glue (random-source-pseudo-randomize-str! (self <mrg32k5a-state>)
                                                       (src <string>))
{
  REG0 = int2fx( mrg32k5a_init_str( self, 
                                    string_text(src),
                                    string_length(src) ) );
  RETURN1();
})


(define-rand-glue (random-source-state-ref (self <mrg32k5a-state>))
{
  char tmp[500];
  mrg32k5a_serialize( self, tmp, sizeof(tmp) );
  REG0 = make_string( tmp );
  RETURN1();
})

(define-rand-glue (random-source-state-set! (self <mrg32k5a-state>)
                                            (arg <raw-string>))
{
  if (mrg32k5a_deserialize( self, arg ) < 0) {
    scheme_error( "~a: invalid random state ~s",
                  2, make_string(FUNCTION), raw_arg );
  }
  RETURN1();
})

(define-rand-glue (random-source-get-int (self <mrg32k5a-state>) 
                                         (arg <raw-int>))
{
  REG0 = int2fx( mrg32k5a_get_int( self, arg ) );
  RETURN1();
})

(define-rand-glue (random-source-get-float (self <mrg32k5a-state>))
{
  REG0 = make_float( mrg32k5a_get_float( self ) );
  RETURN1();
})

(define (random-source-make-integers (self <mrg32k5a-state>))
  (lambda ((n <integer>))
    (cond
     ((<= n 0)
      (error "random integer range must be positive: ~s" n))
     ((fixnum? n)
      (random-source-get-int self n))
     (else
      (random-source-get-large self n)))))

;;;
;;;  Handle large random numbers
;;;  (algorithm adapted from SRFI-27 reference implementation)
;;;

(define-constant $max-fixnum #x1FFFFFFF)

(define (random-source-get-large self n)
  (let loop ((k 2)
             (mk (* $max-fixnum $max-fixnum)))
    (if (< mk n)
        (loop (+ k 1)
              (* mk $max-fixnum))
        (let* ((mk/n (quotient mk n))
               (a (* mk/n n)))
          (let loop ()
            (let ((x (random-source-get-power self k)))
              (if (< x a)
                  (quotient x mk/n)
                  (loop))))))))
          
(define (random-source-get-power self k)
  (let-syntax ((r (syntax-form ()
                    (random-source-get-int self $max-fixnum))))
    (let loop ((accum (r))
               (k k))
      (if (= k 1)
          accum
          (loop (+ (* accum $max-fixnum) (r)) (- k 1))))))
        
;;;
;;;   XXX FIXME  `unit' argument is ignored
;;;

(define (random-source-make-reals (self <mrg32k5a-state>) #optional unit)
  (lambda ()
    (random-source-get-float self)))

;;;

(define *tbase* (time->string (time) "%Y%m%d %H%M%S"))
(define *tcount* 123)

(define (random-source-randomize! (self <mrg32k5a-state>))
  (random-source-pseudo-randomize-str!
   self
   (if (os-file-exists? "/dev/urandom")
       (call-with-input-file "/dev/urandom"
         (lambda (port)
           (read-string port 50)))
       (string-append (time->string (time) "%Y%m%d %H%M%S")
                      (~ " ~d " *tcount*)
                      *tbase*)))
  (let loop ((i 0))
    (if (< i 20)
        (begin
          (random-source-get-int self 10000000)
          (loop (+ i 1)))
        (values))))

      
;;;

(define default-random-source (make-random-source))
(define random-integer (random-source-make-integers default-random-source))
(define random-real (random-source-make-reals default-random-source))
