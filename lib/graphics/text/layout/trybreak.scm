

(define (compute-line-width-and-end-of-class l)
  ;(values 16000 l)
  (*line-width-computer* l))

;;

#|
(define (tffc)
  (let ((v (make-fitness-class-vector)))
    ;;
    (let ((fc (vector-ref v $loose-fit)))
      (set-minimal-demerits! fc 666)
      (set-best-place-line! fc 1)
      (set-best-place! fc #f))
    ;;
    (flush-fitness-class-vector *test*
                                v
                                (active *test*)
                                *htest*
                                'unhyphenated
                                50000
                                (make <space>)
                                (make <space> natural: 9))))

(define (ttb)
  (set-active-width! *test* (make <space> natural: 16000))
  (try-break *test*
             (list-tail (hlist *test*) 2)
             0
             'unhyphenated
             #f))
|#

;;; TeX#859.  Compute the demerits, d, from r to cur_p

(define (compute-demerits ctx b fit-class penalty break-type r p)
  (let ((d (+ (line-penalty ctx) b)))
    (set! d (* d d))
    (if (> penalty 0)
        (set! d (+ d (* penalty penalty)))
        (if (> penalty $eject-penalty)
            (set! d (- d (* penalty penalty)))))
    (if (and (eq? break-type 'hyphenated)
             (eq? (type r) 'hyphenated))
        (if (null? p)
            (set! d (+ d (final-hyphen-demerits ctx)))
            (set! d (+ d (double-hyphen-demerits ctx)))))
    (if (> (abs (- fit-class (fitness r))) 1)
        (set! d (+ d (adj-demerits ctx))))
    d))
