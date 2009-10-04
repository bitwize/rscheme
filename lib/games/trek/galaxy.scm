
(define-class <galaxy> (<object>)
  quadrants)            ;; 10x10 vector of <quadrants>

(define-class <quadrant> (<object>)
  explored?
  num-klingons
  num-bases
  num-stars)

(define-class <ship-status> (<object>)
  (location init-value: '(5 5))
  (energy init-value: 5000)
  (torpedoes init-value: 10)
  (sector init-value: '(0 0)))

(define-class <current-quadrant> (<object>)
  sectors)

(define (make-grid w h #optional fill)
  (let ((row-template (make-vector w fill)))
    (vector-map
     (lambda (k) (clone row-template))
     (list->vector (range h)))))

(define (build-current-quadrant (self <quadrant>))
  (let ((m (make-grid 10 10)))
    (define (try-place item x y)
      (if (vector-ref (vector-ref m y) x)
          #f
          (begin
            (vector-set! (vector-ref m y) x item)
            #t)))
    ;;
    (define (place item)
      (let loop ()
        (let ((x (random 10))
              (y (random 10)))
          (if (try-place item x y)
              (list x y)
              (loop)))))
    ;;  Put in the stars
    (for-each (lambda (s)
                (place 'star))
              (range (num-stars self)))
    ;;  Put in the klingons
    (for-each (lambda (s)
                (place 'klingon))
              (range (num-klingons self)))
    ;;  Put in the starbases
    (for-each (lambda (s)
                (place 'base))
              (range (num-bases self)))
    ;; Put in the Enterprise
    (values (place 'enterprise)
            (make <current-quadrant>
                  sectors: m))))

;;

(define (short-range-scan (ss <ship-status>) (q <current-quadrant>))
  (format #t "     0  1  2  3  4  5  6  7  8  9\n")
  (format #t "   +------------------------------+\n")
  (for-each
   (lambda (y)
     (format #t " ~d |" y)
     (let ((r (vector-ref (sectors q) y)))
       (for-each (lambda (x)
                   (display
                    (case (vector-ref r x)
                      ((star) " * ")
                      ((klingon) "<K>")
                      ((base) "(B)")
                      ((enterprise) " E ")
                      ((#f) " . "))))
                 (range 10)))
     (format #t "|  ")
     (case y
       ((0) (format #t "Energy: ~d\n" (energy ss)))
       ((1) (format #t "Torpedoes: ~d\n" (torpedoes ss)))
       ((2) (format #t "Quadrant: ~d,~d\n"
                    (car (location ss))
                    (cadr (location ss))))
       ((3) (format #t "Sector: ~d,~d\n"
                    (car (sector ss))
                    (cadr (sector ss))))
       (else (newline))))
   (range 10))
  (format #t "   +------------------------------+\n"))


(define (make-galaxy)
  (make <galaxy>
        quadrants: (map (lambda (y)
                          (map (lambda (x)
                                 (make <quadrant>
                                       explored?: #f
                                       num-klingons: (max 0 (- (random 6) 2))
                                       num-stars: (+ (random 9) 1)
                                       num-bases: (if (= (random 10) 0) 1 0)))
                               (range 10)))
                        (range 10))))

(define-method galactic-map ((self <galaxy>))
  (define (brk)
    (display "    +")
    (for-each (lambda (x)
                (display "-----+"))
              (range 10))
    (newline))

  (display "    ")
  (for-each (lambda (x)
              (format #t "   ~d  " x))
            (range 10))
  (newline)
  (for-each
   (lambda (y)
     (brk)
     (format #t "  ~d |" y)
     (for-each (lambda (q x)
                 (if (not (explored? q))
                     (format #t "     |")
                     (format #t "  ~d~a |"
                             (num-klingons q)
                             (if (> (num-bases q) 0) "*" " "))))
               (list-ref (quadrants self) y)
               (range 10))
     (newline))
   (range 10))
  (brk))

(define-method long-range-scan ((self <galaxy>) posn)
  (for-each
   (lambda (dy)
     (display " +---------+---------+---------+\n")
     (display " :")
     (for-each
      (lambda (dx)
        (let ((x (+ (car posn) dx))
              (y (+ (cadr posn) dy)))
          (if (or (< x 0) (> x 9)
                  (< y 0) (> y 9))
              (display "    *    :")
              (let ((q (get-quandrant self x y)))
                (set-explored?! q #t)
                (format #t "  ~d,~d,~d  :"
                        (num-bases q)
                        (num-klingons q)
                        (num-stars q))))))
      '(-1 0 1))
     (newline))
   '(-1 0 1))
  (display " +---------+---------+---------+\n"))

(define (get-quandrant (self <galaxy>) x y)
  (list-ref (list-ref (quadrants self) y) x))

(define-class <game> (<object>)
  ship-status
  galaxy
  current-quadrant)

(define (enter-new-quadrant! (g <game>))
  (bind ((ss (ship-status g))
         (sector cq (build-current-quadrant
                     (get-quandrant (galaxy g)
                                    (car (location ss))
                                    (cadr (location ss))))))
    (set-current-quadrant! g cq)
    (set-sector! ss sector)
    (values)))


(define (new-game)
  (bind ((ss (make <ship-status>
                   location: (list (random 10)
                                   (random 10))))
         (g (make <game>
                  galaxy: (make-galaxy)
                  ship-status: ss
                  current-quadrant: #f)))
    (enter-new-quadrant! g)
    g))


(define *game* (new-game))

(define (lrs)
  (long-range-scan
   (galaxy *game*)
   (location (ship-status *game*))))

(define (gmap)
  (galactic-map (galaxy *game*)))

(define (srs)
  (short-range-scan (ship-status *game*)
                    (current-quadrant *game*)))

(define (impulse dir speed)
  (plan-route (sector (ship-status *game*)) dir speed))

(define (plan-route start-posn dir dist)
  (let* ((angle (* $Pi (/ dir 180)))
         (startx (car start-posn))
         (starty (cadr start-posn))
         (endx (round (+ startx (* dist (cos angle)))))
         (endy (round (- starty (* dist (sin angle)))))
         (nsteps (max (abs (- endx startx))
                      (abs (- endy starty))))
         (dx (/ (- endx startx) nsteps))
         (dy (/ (- endy starty) nsteps))
         (g (make-grid 10 10)))
    ;;
    (define (mark x y ch)
      (vector-set! (vector-ref g y) x ch))
    ;;
    (define (debug n)
      (display-sector g (vector
                         (~ "Heading: ~d" dir)
                         (~ "Distance: ~d" dist)
                         (~ "Steps: ~d" n)
                         (~ "dx: ~d" dx)
                         (~ "dy: ~d" dy)
                         (~ "end.x: ~d" endx)
                         (~ "end.y: ~d" endy))))
    ;;
    (let loop ((i 0)
               (route '())
               (x startx)
               (y starty))
      (format #t "   ~d: (~d,~d)\n" i x y)
      (if (or (< x 0) (>= x 10)
              (< y 0) (>= y 10))
          (begin
            (mark (round startx) (round starty) #\S)
            (debug i)
            (values (reverse! route) #t))
          (if (and (= x endx)
                   (= y endy))
              (begin
                (mark (round startx) (round starty) #\S)
                (mark endx endy #\F)
                (debug (+ i 1))
                (values (reverse! (cons (list endx endy) route)) #f))
              (let ((xi (round x))
                    (yi (round y)))
                (mark xi yi #\O)
                (loop (+ i 1)
                      (cons (list xi yi) route)
                      (+ x dx)
                      (+ y dy))))))))

(define (compass)
  (format #t "      90\n")
  (format #t "       |\n")
  (format #t "180 <--+--> 0\n")
  (format #t "       |\n")
  (format #t "      270\n"))



;;;

(define (display-sector (data <vector>) #optional (sidebar type: <vector>
                                                           default: '#()))
  (format #t "     0  1  2  3  4  5  6  7  8  9\n")
  (format #t "   +------------------------------+\n")
  (for-each
   (lambda (y)
     (format #t " ~d |" y)
     (let ((r (vector-ref data y)))
       (for-each (lambda (x)
                   (display
                    (case (vector-ref r x)
                      ((star) " * ")
                      ((klingon) "<K>")
                      ((base) "(B)")
                      ((enterprise) "0-[")
                      ((#f) " . ")
                      (else (string #\space (vector-ref r x) #\space)))))
                 (range 10)))
     (format #t "|  ")
     (if (< y (vector-length sidebar))
         (format #t "~a\n" (vector-ref sidebar y))
         (newline)))
   (range 10))
  (format #t "   +------------------------------+\n"))

