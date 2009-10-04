
;;;
;;;  Insert new active nodes corresponding to a break at `p'
;;;  if they are feasible breaks.  [Can we insert more than one?]
;;;
;;;  Delete active nodes that are impossible to satisfy at `p'
;;;
;;;
;;;  try_break() is TeX#829

(define (try-break (ctx <para-context>) p penalty type)
  (let ((oldl 0)
        (line-width 0)
        (easy-line -1)
        (min-dem $awful-bad)
        (fitvec (make-fitness-class-vector))
        (break-width (delay (compute-break-width ctx p type)))
        (r (list-iterator (lambda ()
                            (active ctx))
                          (lambda (l)
                            (set-active! ctx l)))))
    ;;
    (debug
     (format #t "/// try-break (penalty=~d)\n" penalty)
     (print p)
     (print ctx)
     (format #t "\\\\\\ ---------------------------\n"))
    ;;
    (define (end-of-line cur-active-width)
      (debug
       (format #t "End of line (cur-active-width = ~s)...\n" cur-active-width))
      ;; flush-fitness-class-vector is ~TeX#836
      (flush-fitness-class-vector ctx
                                  fitvec
                                  r
                                  p
                                  type
                                  min-dem
                                  cur-active-width
                                  (force break-width))
      (set! min-dem $awful-bad))
    ;;
    (define (out-of-rope?)
      (and (second-pass? ctx)
           (= min-dem $awful-bad)
           (li-end? r)
           (li-start? r)))
    ;;
    (define (feasible-break badness fitclass r)
      ;; TeX#855...
      (debug
       (format #t "New feasible break...\n"))
      ;;
      (let* ((d (compute-demerits ctx badness fitclass penalty type (li-current r)
                                  p))
             (dr (total-demerits (li-current r)))
             (fc (vector-ref fitvec fitclass)))
        (debug
         (format #t "   demerits (d) = ~s + ~d\n" d dr))
        (if (<= (+ d dr) (minimal-demerits fc))
            (begin
              (set-minimal-demerits! fc (+ d dr))
              (set-best-place! fc (passive (li-current r)))
              ;; what is `l' at this point in TeX#855?
              ;; Is it (line-number-after l) or is it that -1?
              (set-best-place-line! fc (line-number-after (li-current r)))
              (set! min-dem (min min-dem (+ d dr)))))))
      ;;
    (let loop ((cur-active-width (active-width ctx)))
      ;;
      (debug
       (format #t "\n...try-break loop...\n   LIST: ")
       (r 'status)
       (format #t "   WIDTH: ~s\n" cur-active-width))
      ;;
      (if (li-end? r)
          (if (< min-dem $awful-bad)
              (end-of-line cur-active-width))
          (let ((n (li-current r)))
            (if (delta-node? n)
                (begin
                  (li-fwd r)
                  (loop (space+ cur-active-width (delta n))))
                (begin
                  ;; update the line-width and oldl
                  (if (> (line-number-after n) oldl)
                      (begin
                        (if (and (< min-dem $awful-bad)
                                 (not (= oldl easy-line)))
                            (end-of-line cur-active-width))
                        (bind ((lw eoc (compute-line-width-and-end-of-class
                                        (line-number-after n))))
                          (debug
                           (format #t "Set line width = ~s (class ~s)\n" lw eoc))
                          (set! line-width lw)
                          (set! oldl eoc))))
                  ;;
                  (bind ((b fit (compute-fit-and-bad line-width
                                                     cur-active-width)))
                    (debug
                     (format #t "badness=~s fit=~s\n" b fit))
                    (if (or (> b $infinitely-bad)
                            (= penalty $eject-penalty))
                        (if (or (out-of-rope?)
                                (<= b (threshold ctx)))
                            (begin
                              (feasible-break b fit r)
                              (loop (deactivate ctx r cur-active-width)))
                            (begin
                              (loop (deactivate ctx r cur-active-width))))
                        (begin
                          (if (<= b (threshold ctx))
                              (feasible-break b fit r))
                          (li-fwd r)
                          (loop cur-active-width)))))))))))


;;

;;;
;;;  [portions of TeX#836]
;;;
;;;  Create new active nodes for the best feasible breaks just found
;;;

(define (flush-fitness-class-vector (ctx <para-context>)
                                    (v <vector>)
                                    r
                                    p 
                                    type 
                                    minimum-demerits
                                    cur-active-width
                                    break-width)
  ;;
  (assert (< minimum-demerits $awful-bad))
  ;;
  (let* ((d (space- cur-active-width break-width))
         (minimum-demerits (+ minimum-demerits (abs (adj-demerits ctx))))
         (any-active? #f))
    ;; prepend the LEADING delta node (TeX#843)
    (if (li-start? r)
        (set-active-width! ctx break-width)
        (if (delta-node? (li-prev r))
            (set-delta! (li-prev r) (space- (delta (li-prev r)) d))
            (begin
              (li-insert-before! r (make <delta-node>
                                         delta: (space- (make <space>) d)))
              (li-fwd r))))
    ;;
    (vector-for-each
     (lambda ((fc <fitness-class>))
       (debug
        (format #t "checking fitness class[~d]: ~s <> ~s\n" 
                (fitness fc)
                (minimal-demerits fc)
                minimum-demerits))
       (if (<= (minimal-demerits fc) minimum-demerits)
           (begin
             (set! any-active? #t)
             (li-insert-before! r (make-active-node ctx fc p type))
             (li-fwd r)))
       (set-minimal-demerits! fc $awful-bad))
     v)
    ;;
    (if (not any-active?)
        (error "expected at least one active node in flushing fitness"))
    ;; initialize the result list with the TRAILING delta node
    ;; (TeX#844)
    (if (not (li-end? r))
        (begin
          (li-insert-before! r (make <delta-node> delta: d))
          (li-fwd r)))))
