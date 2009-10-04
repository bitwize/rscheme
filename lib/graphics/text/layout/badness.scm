(define (badness (t <fixnum>) (s <fixnum>))
  (cond
   ((= t 0) 0)
   ((<= s 0) $infinitely-bad)
   (else
    (let ((r (cond
              ((<= t 7230584) (quotient (* t 297) s))
              ((>= s 1663497) (quotient t (quotient s 297)))
              (else t))))
      (if (> r 1290)
          $infinitely-bad
          (quotient (+ (* r r r) #o400000) #o1000000))))))

(define (compute-fit-and-bad line-width (width <space>))
  (debug
   (format #t "compute-fit-and-bad: ~s / ~s\n" line-width width))
  ;; TeX#852, TeX#853, and the shortfall calculation and <> 0 dispatch
  ;; logic of TeX#851
  (let ((shortfall (- line-width (natural width))))
    (if (> shortfall 0)
        ;; the line is too short so needs to be stretched
        (if (or (not (zero? (fil-stretch width)))
                (not (zero? (fill-stretch width)))
                (not (zero? (filll-stretch width))))
            ;; there is infinite stretch available, so no problem!
            (values 0 $decent-fit)
            (let ((b (badness shortfall (pt-stretch width))))
              (cond
               ((<= b 12)
                (values b $decent-fit))
               ((<= b 99)
                (values b $loose-fit))
               (else
                (values b $very-loose-fit)))))
        ;; the line is too long so needs to be shrunk
        ;; note that we don't support infinite shrinkability in paragraphs
        (if (> (- shortfall) (pt-shrink width))
            ;; more shrinkability required than available
            (values (+ $infinitely-bad 1) $tight-fit)
            (let ((b (badness (- shortfall) (pt-shrink width))))
              (values b
                      (if (> b 12)
                          $tight-fit
                          $decent-fit)))))))
