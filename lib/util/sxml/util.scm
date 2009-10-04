;(define-constant $port-chunk-size 7)            ; debugging...
;(define-constant $port-chunk-size 8192)         ; moderate
(define-constant $port-chunk-size 102400)          ; large

;;;

(define-class <xml-error> (<condition>)
  respecting
  message)

(define-method display-object ((self <xml-error>) port)
  (format port "XML Error [~a] ~a\n" (respecting self) (message self)))

(define (xml-error stream respecting fmt . args)
  (signal (make <xml-error>
                respecting: respecting
                message: (if (null? args)
                             fmt
                             (apply format #f fmt args)))))
  
;;;

(define-syntax (sxml:debug . body)
  ;(begin . body)
  (values))

(define-syntax (sxml:debug-print msg . args)
  (sxml:debug (format #t msg . args)))


;;;
;;;  Build a list of parsed-out items in reverse
;;;
;;;  e.g., if the input XML is:
;;;
;;;     STR = "see, &eg;, CATS"
;;;
;;;  then the input items (from <xml-text>) are:
;;;
;;;     (#(STR 0 5) eg #(STR 9 15))
;;;
;;;  and we want the output to be something like:
;;;
;;;     (", CATS" (foreignphrase "e.g.") "see, ")
;;;
;;;  (note it's reversed so that we can just append it onto the
;;;  children list that we're accumulating above)

(define (resolve-no-entities name)
  #f)

(define *standard-xml-entities* (make-symbol-table))

(table-insert! *standard-xml-entities* 'quot "\"")
(table-insert! *standard-xml-entities* 'apos "'")
(table-insert! *standard-xml-entities* 'lt "<")
(table-insert! *standard-xml-entities* 'gt ">")
(table-insert! *standard-xml-entities* 'amp "&")

(define (no-entity (name <symbol>))
  #f)

(define-thread-var *xml-application-entity-ref* no-entity)

(define (with-xml-application-entity-ref (fn <function>) thunk)
  (thread-let ((*xml-application-entity-ref* fn))
    (thunk)))

(define (set-xml-application-entity-ref! (fn <function>))
  (set! *xml-application-entity-ref* fn))

(define (resolve-standard-entities name)
  (or (table-lookup *standard-xml-entities* name)
      (let ((f *xml-application-entity-ref*))
        (and f (f name)))))

;;;
;;;  Note that we build up the parsed text in reverse, because
;;;  it will be reversed later...

(define (expand-text-parsed lst 
                            #optional (resolver default: resolve-no-entities))
  (let ((q (make-dequeue))
        (accum #f))
    ;;
    (define (addnonstr item)
      (if accum
          (dequeue-push-front! q (get-output-string accum)))
      (if (pair? item)
          (for-each (lambda (x)
                      (dequeue-push-front! q x)
                      (values))
                    item)
          (dequeue-push-front! q item))
      (set! accum #f))
    ;;
    (define (addstr str)
      (if (not accum)
          (set! accum (open-output-string)))
      (write-string accum str))
    ;;
    (define (add item)
      (if (string? item)
          (addstr item)
          (addnonstr item)))
    ;;
    ;;
    (for-each
     (lambda (i)
       (cond
        ((vector? i)
         (addstr (substring (vector-ref i 0)
                            (vector-ref i 1)
                            (vector-ref i 2))))
        ((char? i)
         (addstr (string i)))
        ((symbol? i)
         (let ((x (resolver i)))
           (add (or x (list '*ENTITY* (symbol->string i))))))))
     lst)
    ;;
    (if accum
        (dequeue-push-front! q (get-output-string accum)))
    (vector->list (dequeue-state q))))

;;;  Flatten out entity refs, etc., but can't handle parsed
;;;  entities.  This is used, for example, in attribute value
;;;  processing

(define (expand-text lst #optional 
                     (resolver default: resolve-standard-entities))
  ;; special case the most common ones
  (cond
   ((null? lst)
    "")
   ((and (null? (cdr lst))
         (string? (car lst)))
    (car lst))
   ((and (null? (cdr lst))
         (vector? (car lst)))
    (let ((i (car lst)))
      (substring (vector-ref i 0)
                 (vector-ref i 1)
                 (vector-ref i 2))))
   (else
    (call-with-output-string
     (lambda (port)
       (for-each
        (lambda (i)
          (cond
           ((vector? i)
            (write-string port (substring (vector-ref i 0)
                                          (vector-ref i 1)
                                          (vector-ref i 2))))
           ((char? i)
            (write-char port i))
           ((symbol? i)
            (cond
             ((resolver i)
              => (lambda (s)
                   (if (string? s)
                       (write-string port s);; XXX need to recursively expand
                       (signal
                        (make <xml-error>
                              respecting: 0
                              message: (~ "Entity '&~s;' not valid here" i))))))
             (else
              (error "Unknown entity '&~s;'" i))))
           ((string? i)
            (write-string port i))
           (else
            (error "Unexpected ~s in textlist" i))))
        lst))))))
