
(define (iteration->list proc)
  (let ((r '()))
    (proc (lambda (item)
            (set! r (cons item r))))
    (reverse! r)))

(define (proc->iterator proc)
  ;;
  ;; the argument `proc' takes a single argument, which is
  ;; a procedure to be called with the results value(s)
  ;; to be returned from the iterator.  When the proc is done,
  ;; the iterator returns no values ad infinitum
  ;;
  (call-with-current-continuation
   (lambda (return)
     (let ((exit #f)
           (next #f))
       ;;
       (define (emit . item)
         (set! exit (call-with-current-continuation
                      (lambda (cc)
                        (set! next cc)
                        (apply exit item)))))
       ;;
       (set! exit
             (call-with-current-continuation
              (lambda (cc)
                (set! next cc)
                (return (lambda ()
                          (call-with-current-continuation next))))))
       ;;
       (proc emit)
       (let loop ()
         (emit)
         (loop))))))

#|
   (foo a b #rest x)
==>
   REG0 = a
   REG1 = b
   t = x;
   if (PAIR_P(t)) {             /* inline_expand(2,t) */
     REG2 = car(t);
     t = cdr(t);
     if (PAIR_P(t)) {
       REG3 = car(t);
       t = cdr(t);
       if (PAIR_P(t)) {
         REG4 = car(t);
         t = cdr(t);
         if (PAIR_P(t)) {
           full_expand( 5, t );
         } else {
           arg_count_reg = 5;
         }
       } else {
         arg_count_reg = 4;
       }
     } else {
       arg_count_reg = 3;
     }
   } else {
     arg_count_reg = 2;
   }
|#
