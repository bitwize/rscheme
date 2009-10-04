;;;@title SRFI-39: Parameter Objects
;;;@tag srfi-39
;;;@note <h2>Defining parameters objects</h2>
;;;@note <p>(<b>NOTE.</b> SRFI-39 is not included in the b25 build.)</p>
;;;@note <p>In RScheme, we have to import the module first:</p>
,(use srfi.39)
;;;@note <p>Then we can start defining some parameters</p>
;;;@reprompt
(define radix
  (make-parameter 10))
(define write-shared
  (make-parameter
   #f
   (lambda (x)
     (if (boolean? x)
         x
         (error "only booleans are valid 'write-shared' values")))))
;;;@note <p>Now we can start setting and checking some values</p>
;;;@reprompt
(radix)
;;;@expect (= % 10)
(radix 2)
(radix)
;;;@expect (= % 2)
;;;@note <p>The following demonstrates how the <i>converter</i>
;;;@note    procedure can be used to do type checking</p>
;;;@reprompt
(write-shared #t)
(write-shared 0)
,top
;;;@expect (eq? (write-shared) #t)
;;;@note <p>Conversion can also normalize values for convenience.
;;;@note (In <i>RScheme</i>, the <code>to-string</code> generic
;;;@note is especially useful here.)</p>
;;;@reprompt
(define prompt
  (make-parameter
    123
    to-string))
(prompt)
;;;@expect (equal? % "123")
(prompt ">")
(prompt)
;;;@expect (equal? % ">")
(prompt 'cons)
(prompt)
;;;@expect (equal? % "cons")
;;;@note <h2>Establishing new parameter value scopes</h2>
;;;@reprompt
(radix)
;;;@expect (= % 2)
(parameterize ((radix 16)) (radix))
;;;@expect (= % 16)
(radix)
;;;@expect (= % 2)
(define (f n)
  (number->string n (radix)))
(f 10)
;;;@expect (equal? % "1010")
(parameterize ((radix 8)) (f 10))
;;;@expect (equal? % "12")
(parameterize ((radix 8) (prompt (f 10))) (prompt))
;;;@expect (equal? % "1010")
;;;@note <h2>RScheme-Specific Features</h2>
;;;@note <p>In <i>RScheme</i>, parameter objects are first-class procedures,
;;;@note    <i>but</i> they are their own subclass of 
;;;@note    <code>&lt;function&gt;</code>.</p>
;;;@reprompt
(instance? radix <function>)
(object-class radix)
;;;@note <p>You can also give parameter objects names, but to do
;;;@note so you have to supply a converter argument.  A converter
;;;@note of <b><code>#f</code></b> means <code>identity</code>.</p>
;;;@reprompt
(define radix (make-parameter 10 #f 'radix))
radix
;;;@note <p>The <code>name</code> generic can extract the name from
;;;@note a parameter object.</p>
;;;@reprompt
(name radix)
(name prompt)
;;;@note <p>In <i>RScheme</i>, parameter objects are implemented as
;;;@note    thread variables, but the name of one is itself.</p>
;;;@reprompt
,(use tables)
(define (reflect-on-thread-vars)
  (let ((ts (get-thread-state-reg))
        (found (make-object-table)))
    ;;
    (define (scan l indir)
      (let loop ((l l)
                 (indir indir))
        (if (eq? indir $thread-var-end)
            l
            (let ((k (gvec-ref indir 0)))
              (if (not (table-lookup found k))
                  (begin
                    (table-insert! found k #t)
                    (loop (cons (list k (gvec-ref indir 1)) l)
                          (gvec-ref indir 2)))
                  (loop l (gvec-ref indir 2)))))))
    ;;
    (reverse!
     (scan
      (scan (map (lambda (i)
                   (list (vector-ref *direct-names* i)
                         (gvec-ref ts (+ i 1))))
                 (range (vector-length *direct-names*)))
            (gvec-ref ts 0))
      *thread-var-init-values*))))
(print (reflect-on-thread-vars))

