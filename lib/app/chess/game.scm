(define-class <square> (<object>)
  (rank type: <fixnum>)
  (file type: <symbol>)
  (bits type: <bit-board>)
  (is-white? type: <boolean>))

(define (white-square? f r)
  (is-white? (sq f r)))

(define *all-files* '(a b c d e f g h))
(define *all-ranks* '(1 2 3 4 5 6 7 8))

(define *all-squares*
  (list->vector
   (map (lambda (rank)
          (list->vector
           (map (lambda (file)
                  (make <square>
                        is-white?: (= (modulo rank 2)
                                      (if (vmemq file '#(a c e g))
                                          0
                                          1))
                        rank: rank
                        file: file
                        bits: (bbset (new-bit-board) file rank)))
                *all-files*)))
        *all-ranks*)))

(define (sq f r)
  (vector-ref (vector-ref *all-squares* (- r 1))
              (vmemq f '#(a b c d e f g h))))

  
(define-class <piece> (<object>)
  (square type: <square>)
  (type type: <symbol>)
  (num-moves type: <fixnum> init-value: 0)
  (color type: <symbol>))

(define-class <player> (<object>)
  (color type: <symbol>)
  (pieces type: <list>)
  (occupies type: <bit-board>))

(define-class <board> (<object>)
  (white type: <player>)
  (black type: <player>)
  (to-move init-value: 'white)
  (game init-value: '()))

(define (piece-char (self <piece>))
  (case (type self)
    ((pawn) "P")
    ((rook) "R")
    ((king) "K")
    ((bishop) "B")
    ((knight) "N")
    ((queen) "Q")))

(define-method piece-at ((self <player>) file rank)
  (let ((s (sq file rank)))
    (let loop ((l (pieces self)))
      (if (null? l)
          (values)
          (if (eq? (square (car l)) s)
              (values (car l) self)
              (loop (cdr l)))))))

(define-method piece-at ((self <board>) file rank)
  (if (bb-is-set? (occupies (white self)) file rank)
      (piece-at (white self) file rank)
      (piece-at (black self) file rank)))

(define-method print ((self <board>))
  (print-board
   (lambda (f r)
     (if (bb-is-set? (occupies (white self)) f r)
         (string-append "  " (piece-char (piece-at (white self) f r)))
         (if (bb-is-set? (occupies (black self)) f r)
             (string-append " *" (piece-char (piece-at (black self) f r)))
             "  ."))))
  self)

(define (make-pawn-rank r c)
  (map (lambda (f)
         (make <piece>
               square: (sq f r)
               type: 'pawn
               color: c))
       *all-files*))

(define (make-back-rank r c)
  (map (lambda (f t)
         (make <piece>
               square: (sq f r)
               type: t
               color: c))
       '(e a b c d f g h)
       '(king rook knight bishop queen bishop knight rook)))

(define (make-player color plist)
  (make <player>
        color: color
        pieces: plist
        occupies: (all-occupied (map square plist))))

(define-method opponent ((self <player>) (board <board>))
  (case (color self)
    ((white) (black board))
    ((black) (white board))))
  

;;;
;;;  We'll make sure the King is always the first piece on the list
;;;

(define (king (self <player>))
  (car (pieces self)))

(define (make-white-player)
  (make-player (append (make-back-rank 1 'white)
                       (make-pawn-rank 2 'white))))

(define (make-black-player)
  (make-player (append (make-back-rank 8 'black)
                       (make-pawn-rank 7 'black))))

(define (all-occupied lst)
  (reduce (lambda ((a <bit-board>) (b <square>))
            (format #t "combining...\n")
            (print a)
            (print b)
            (bexec a "0|" (bits b)))
          (new-bit-board)
          lst))

(define (new-board)
  (make <board>
        white: (make-white-player)
        black: (make-black-player)))

(define (board (desc <string>))
  (let ((w '())
        (b '()))
    (define (white-piece p) (set! w (cons p w)))
    (define (black-piece p) (set! b (cons p b)))
    ;
    (let loop ((i 0)
               (f *all-files*)
               (r 8))
      (if (< i (string-length desc))
          (let ((ch (string-ref desc i)))
            (case ch
              ((#\/) (loop (+ i 1) *all-files* (- r 1)))
              ((#\1 #\2 #\3 #\4 #\5 #\6 #\7)
               (loop (+ i 1) (list-tail f (string->number (string ch))) r))
              ((#\p #\n #\b #\r #\q #\k)
               (black-piece
                (make <piece>
                  color: 'black
                  square: (sq (car f) r)
                  type: (cdr 
                         (assq ch
                               '((#\p . pawn)
                                 (#\k . king)
                                 (#\n . knight)
                                 (#\b . bishop)
                                 (#\r . rook)
                                 (#\q . queen))))))
               (loop (+ i 1) (cdr f) r))
              ((#\P #\N #\B #\R #\Q #\K)
               (white-piece
                (make <piece>
                  color: 'white
                  square: (sq (car f) r)
                  type: (cdr 
                         (assq ch
                               '((#\P . pawn)
                                 (#\K . king)
                                 (#\N . knight)
                                 (#\B . bishop)
                                 (#\R . rook)
                                 (#\Q . queen))))))
               (loop (+ i 1) (cdr f) r))))
          (make <board>
                white: (make <player>
                             color: 'white
                             pieces: (king-first w)
                             occupies: (all-occupied (map square w)))
                black: (make <player>
                             color: 'black
                             pieces: (king-first b)
                             occupies: (all-occupied (map square b)))
                to-move: 'white)))))

(define (king-first lst)
  (append (select (lambda (p) (eq? (type p) 'king)) lst)
          (select (lambda (p) (not (eq? (type p) 'king))) lst)))
                    
