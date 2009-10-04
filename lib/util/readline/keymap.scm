
(define *debug-port* #f)

(define (debug msg . args)
  #|
  (if (not *debug-port*)
      (set! *debug-port* (open-output-file "/dev/pts/8")))
  (apply format *debug-port* msg args)
  |#
  (values))

(define (whazzup out seq)
  ;(write-string out "\033[L\015")
  (debug "??? What's up with ~s\n" seq)
  ;(write-string out "\033\.7") ;; re-save cursor on this line
  #f)

(define-class <key-state-map> (<object>)
  (char-branch init-value: '()))

(define-method insert-binding! ((self <key-state-map>) from to)
  (if (null? (cdr from))
      (set-char-branch! self (cons (cons (car from) to) (char-branch self)))
      (let ((a (assq (car from) (char-branch self))))
        (if a
            (set! a (cdr a))
            (begin
              (set! a (make <key-state-map>))
              (set-char-branch! self (cons (cons (car from) a)
                                           (char-branch self)))))
        (insert-binding! a (cdr from) to))))

(define *esc-key-map* (make <key-state-map>))

(insert-binding! *esc-key-map* '(#\[ #\A) 'history-prev)
(insert-binding! *esc-key-map* '(#\[ #\B) 'history-next)
(insert-binding! *esc-key-map* '(#\[ #\C) 'right)
(insert-binding! *esc-key-map* '(#\[ #\D) 'left)
(insert-binding! *esc-key-map* '(#\O #\P) 'f1)
(insert-binding! *esc-key-map* '(#\O #\Q) 'f2)
(insert-binding! *esc-key-map* '(#\O #\R) 'f3)
(insert-binding! *esc-key-map* '(#\O #\S) 'f4)
(insert-binding! *esc-key-map* '(#\[ #\1 #\5 #\~) 'f5)
(insert-binding! *esc-key-map* '(#\[ #\1 #\7 #\~) 'f6)
(insert-binding! *esc-key-map* '(#\[ #\1 #\8 #\~) 'f7)
(insert-binding! *esc-key-map* '(#\[ #\1 #\9 #\~) 'f8)
(insert-binding! *esc-key-map* '(#\[ #\2 #\0 #\~) 'f9)
(insert-binding! *esc-key-map* '(#\[ #\2 #\1 #\~) 'f10)
(insert-binding! *esc-key-map* '(#\[ #\2 #\3 #\~) 'f11)
(insert-binding! *esc-key-map* '(#\[ #\2 #\4 #\~) 'f12)
(insert-binding! *esc-key-map* '(#\[ #\1 #\~) 'home)
(insert-binding! *esc-key-map* '(#\[ #\2 #\~) 'insert-mode)
(insert-binding! *esc-key-map* '(#\[ #\3 #\~) 'delete-right)
(insert-binding! *esc-key-map* '(#\[ #\4 #\~) 'end)
(insert-binding! *esc-key-map* '(#\[ #\5 #\~) 'page-up)
(insert-binding! *esc-key-map* '(#\[ #\6 #\~) 'page-down)

(define (process-key-map self inp out keypath)
  (let* ((ch (read-char inp))
         (a (assq ch (char-branch self)))
         (xpath (cons ch keypath)))
    (if a
        (let ((n (cdr a)))
          (if (instance? n <key-state-map>)
              (process-key-map n inp out xpath)
              (values n (reverse xpath))))
        (whazzup out (reverse xpath)))))
