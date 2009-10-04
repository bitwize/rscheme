;;;
;;;   A Test Suite for SRFI-18 ("Multithreading Support")
;;;   written using the facilities of SRFI-64
;;;

(test-begin "SRFI-18")

(test-begin "thread-specific")
(define a (make-thread (lambda () 123) "hello"))
(test-equal (begin (thread-specific-set! a '(a b c))
                   (thread-specific a))
            '(a b c))
(test-end)

(test-begin "thread-join!")

(define-class <foo> (<condition>))

(define (a)
  (thread-start!
   (make-thread
    (lambda ()
      123))))

(define (crash)
  (vector-ref '#(1 2) 3))

(define (b)
  (thread-start!
   (make-thread
    (lambda ()
      (crash)))))

(define (make-foo)
  'foo)

(define (m)
  (thread-start! (make-thread (lambda () (values 1 (make-foo))))))

(test-equal "1. Single return value" (thread-join! (a)) 123)
(test-error "2. Uncaught exception" uncaught-exception? (thread-join! (b)))
(test-equal "3. Multiple return values" 
        (values->list (thread-join! (m))) 
        '(1 foo))
  
(test-end "thread-join!")

(test-begin "thread-terminate!")

(define (c kill-when)
  (let ((killee (make-thread
                 (lambda ()
                   (let loop ()
                     (thread-yield!)
                     (loop))))))
    ;;
    (if (eq? kill-when 'before-start)
        (thread-terminate! killee))
    ;;
    (thread-start! killee)
    ;;
    (if (eq? kill-when 'soon-after-start)
        (thread-terminate! killee))
    ;;
    (if (memq kill-when '(after-start much-later))
        (thread-start!
         (make-thread
          (lambda ()
            (if (eq? kill-when 'much-later)
                (thread-sleep! 0.5))
            (thread-terminate! killee)))))
    ;;
    (thread-sleep! 0.1)
    killee))

(test-error "1. terminate before start" terminated-thread-exception? (thread-join! (c 'before-start)))
(test-error "2. terminate soon after start" terminated-thread-exception? (thread-join! (c 'soon-after-start)))
(test-error "3. terminate before join" terminated-thread-exception? (thread-join! (c 'after-start)))
(test-error "4. terminate after join" terminated-thread-exception? (thread-join! (c 'much-later)))
(test-end "thread-terminate!")

(test-end "SRFI-18")
