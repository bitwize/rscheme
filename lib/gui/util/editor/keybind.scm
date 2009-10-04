
(define-class <key-binding> (<object>) :abstract)

(define-class <key-prefix> (<key-binding>)
  (follows type: <table>))

(define-class <key-command> (<key-binding>)
  procedure)

  
(define (insert-key-binding (p <key-prefix>) (seq <pair>) proc)
  (if (null? (cdr seq))
      (table-insert! (follows p) (car seq) (make <key-command>
                                                 procedure: proc))
      (let ((next (table-lookup (follows p) (car seq))))
        (if (not next)
            (begin
              (set! next (make <key-prefix>
                               follows: (make-table)))
              (table-insert! (follows p) (car seq) next)))
          (insert-key-binding next (cdr seq) proc))))


(define-class <key-state-machine> (<object>)
  (bindings type: <key-prefix>)
  (accumulator init-value: '())
  (current-state type: <key-prefix>))
  
(define (make-key-state-machine)
  (make <key-state-machine>
        bindings: *default-key-binding*
        current-state: *default-key-binding*))
        
;;;

(define-method process-hit-key ((self <key-prefix>) (mach <key-state-machine>))
  (set-current-state! mach self))

(define-method process-hit-key ((self <key-command>) 
                                (mach <key-state-machine>))
  ;;
  (set-current-state! mach (bindings mach))
  (set-accumulator! mach '())
  ;;
  ((value (procedure self))))

(define (process-key (sm <key-state-machine>) event)
  (let ((n (table-lookup (follows (current-state sm)) (data event))))
    (set-accumulator! sm (cons (data event) (accumulator sm)))
    ;;
    (if n
        (process-hit-key n sm)
        (begin
          (format #t "*** BEEP *** ~s not key-bound\n"
                  (reverse (accumulator sm)))
          (set-current-state! sm (bindings sm))
          (set-accumulator! sm '())))))

;;;

(define (make-default-key-bindings)
  (let ((top (make <key-prefix>
                   follows: (make-table))))
    ;;
    (for-each (lambda (ch)
                (insert-key-binding top (list ch) (& kbd-insert-self)))
              (string->list
               (string-append
                "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                "abcdefghijklmnopqrstuvwyxz"
                "0123456789"
                "!@#$%^&*()"
                "-=_+[]{};:'\",.<>/?\\|`~ ")))
    ;;
    (insert-key-binding top '(#\C-a) (& beginning-of-line))
    (insert-key-binding top '(#\C-e) (& end-of-line))
    (insert-key-binding top '(#\C-l) (& redisplay))
    (insert-key-binding top '(backspace) (& backward-delete-char))
    top))


(define *default-key-binding* (make-default-key-bindings))

