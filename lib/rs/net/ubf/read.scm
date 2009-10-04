

(define-class <ubf-input-context> (<object>)
  (registers init-value: #f))

(define (read-ubf str)
  (let* ((c (make <ubf-input-context>
                  registers: (make-vector 256 #f)))
         (v (ubf-run* c (open-input-string str) (make-dequeue))))
    ;;
    (if (= (vector-length v) 1)
        (vector-ref v 0)
        (error "ubf: result not a unique object"))))

(define (for-each-ubf port proc)
  (let* ((c (make <ubf-input-context>
                  registers: (make-vector 256 #f))))
    ;;
    (let loop ()
      (let ((v (ubf-run* c port (make-dequeue))))
        (case (vector-length v)
          ((1)
           (proc (vector-ref v 0))
           (loop))
          ((0)
           (values))
          (else
           (error "ubf: result not a unique object")))))))
        

(define (ubf-run* (self <ubf-input-context>) inp q)
  (let loop ()
    (let ((item (ubf-scan self inp)))
      (case item
        ((#\$)
         (dequeue-state q))
        ((#\{)
         (dequeue-push-back! q (ubf-read-struct self inp))
         (loop))
        ;;
        ((#\}) #\})
        ((#\&) 
         (let* ((h (dequeue-pop-back! q))
                (t (dequeue-pop-back! q)))
           (dequeue-push-back! q (cons h t))
           (loop)))
        ((#\#) 
         (dequeue-push-back! q '())
         (loop))
        ;;
        ((#\>)
         (vector-set! (registers self)
                      (char->integer (read-char inp))
                      (dequeue-pop-back! q))
         (loop))
        ;;
        ((#\~)
         (let ((data (read-string inp (dequeue-pop-back! q))))
           (dequeue-push-back! q (string->byte-vector data))
           ;(print data)
           (if (not (eq? (read-char inp) #\~))
               (error "ubf: binary not followed by #\\~"))
           (loop)))
        ;;
        (else
         (if (eof-object? item)
             (dequeue-state q)
             (begin
               (assert (not (char? item)))
               (dequeue-push-back! q item)
               (loop))))))))

(define (ubf-read-struct (self <ubf-input-context>) inp)
  (let* ((q (make-dequeue))
         (e (ubf-run* self inp q)))
    (if (eq? e #\})
        (dequeue-state q)
        (error "ubf: structure still open"))))

(define (readdelim inp d)
  (let ((a (open-output-string)))
    (let loop ()
      (let ((ch (read-char inp)))
        (cond
         ((eq? ch d)
          (get-output-string a))
         ((eq? ch #\\)
          (write-char (read-char inp) a)
          (loop))
         ((eof-object? ch)
          (error "ubf: eof inside delimited object"))
         (else
          (write-char ch a)
          (loop)))))))
        
(define (ubf-scan (self <ubf-input-context>) inp)
  (let ((ch (read-char inp)))
    (case ch
      ((#\-)
       (if (char-numeric? (peek-char inp))
           (- (ubf-scan self inp))
           (error "ubf: #\\- not followed by digit")))
      ((#\0)
       0)
      ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (let loop ((lst (list ch)))
         (if (char-numeric? (peek-char inp))
             (loop (cons (read-char inp) lst))
             (string->number (list->string (reverse! lst))))))
      ((#\space #\tab #\cr #\lf)
       ;; ignore common whitespace
       (ubf-scan self inp))
      ((#\$ #\> #\& #\{ #\} #\# #\~)
       ch)
      ((#\')
       (string->symbol (readdelim inp #\'))) 
      ((#\")
       (readdelim inp #\"))
      (else
       (if (eof-object? ch)
           ch
           (or (vector-ref (registers self) (char->integer ch))
               (cond
                ((table-lookup *ubf-scan-hook* ch)
                 => (lambda (h)
                      (h inp)))
                (else
                 (error "~s: Register value not defined or invalid char"
                        ch)))))))))

(define *ubf-scan-hook* (make-object-table))
