
(define (list-iterator list-get list-set #key (auto-status? default: #f))
  (let ((prev-prev #f)
        (prev #f)
        (posn 0)
        (current (list-get)))
    ;;
    (define (status #optional op)
      (if op
          (format #t "~s ==> [" op)
          (format #t "["))
      (if prev-prev
          (format #t "~s " (car prev-prev))
          (format #t "- "))
      (if prev
          (format #t "~s " (car prev))
          (format #t "- "))
      (format #t "<~d> ~s]\n" posn current))
    ;;
    (lambda (op #optional data)
      (case op
        ((status)
         (status))
        ;;
        ((back)
         (if prev
             (begin
               (set! current prev)
               (set! prev prev-prev)
               (set! prev-prev #f)
               (set! posn (- posn 1))
               (if auto-status? (status op))
               (values))
             (if (= posn 0)
                 (error "cannot 'back' at start of list")
                 (error "exceeded 'back' depth"))))
        ;;
        ((fwd)
         (if (pair? current)
             (begin
               (set! prev-prev prev)
               (set! prev current)
               (set! current (cdr current))
               (set! posn (+ posn 1))
               (if auto-status? (status op))
               (values))
             (error "cannot 'fwd' at end of list")))
        ;;
        ((prev)
         (and prev (car prev)))
        ((prev-prev)
         (and prev-prev (car prev-prev)))
        ((current)
         (and (pair? current)
              (car current)))
        ;;
        ((start?)
         (eq? posn 0))
        ((end?)
         (null? current))
        ;;
        ((insert-before)
         (set! current (cons data current))
         (if prev
             (set-cdr! prev current)
             (list-set current))
         (if auto-status? (status op)))
        ;;
        ((delete-current)
         (let ((x (car current)))
           (set! current (cdr current))
           (if prev
               (set-cdr! prev current)
               (list-set current))
           (if auto-status? (status op))
           x))))))

         
(define (foo #optional (x default: '(a b c d e f)))
  (list-iterator (lambda () x) (lambda (y) (set! x y))))

;;;


(define-syntax (li-back l) (l 'back))
(define-syntax (li-fwd l) (l 'fwd))
(define-syntax (li-prev l) (l 'prev))
(define-syntax (li-prev-prev l) (l 'prev-prev))
(define-syntax (li-current l) (l 'current))
(define-syntax (li-start? l) (l 'start?))
(define-syntax (li-end? l) (l 'end?))

(define-syntax (li-insert-before! l x) (l 'insert-before x))
(define-syntax (li-delete-current! l) (l 'delete-current))
