;;;
;;;  We use Scheme lists instead of the link pointers...
;;;

(define-class <active-node> (<object>)
  ;; Points to the corresponding passive node
  passive                                       ; TeX: break_node
  ;; The ordinal number of the line that will follow this breakpoint
  (line-number-after type: <fixnum>)
  ;; The fitness classification of the line that would precede this breakpoint
  ;;   0 = very loose
  ;;   1 = loose
  ;;   2 = decent
  ;;   3 = tight
  (fitness type: <fixnum> init-value: 2)
  ;; Type is either 'hyphenated or 'unhyphenated, according to whether
  ;; this breakpoint is a discretionary node
  (type type: <symbol> init-value: 'unhyphenated)
  ;; The total demerits is the minimum possible sum of demerits over 
  ;; all lines from the beginning of the paragraph up to this breakpoint
  (total-demerits type: <fixnum>))

(define-class <delta-node> (<object>)
  delta)

(define-syntax (delta-node? x)
  (instance? x <delta-node>))

;; cf TeX#821
(define-class <passive-node> (<object>)
  ;; The position of the break
  break
  ;; Points to the passive node that precedes this one on an optimal
  ;; path to this breakpoint
  prev-break)

;;;
;;; TeX uses #o7777777777 for $infinitely-bad, but we want
;;; to use the max <fixnum>

(define-constant $infinitely-bad #x1fffffff)                ; TeX: inf_bad
(define-constant $awful-bad $infinitely-bad)

(define-constant $fitness-classes '(0 1 2 3))
(define-constant $very-loose-fit 0)
(define-constant $loose-fit 1)
(define-constant $decent-fit 2)
(define-constant $tight-fit 3)

(define-constant $inf-penalty $infinitely-bad)
(define-constant $eject-penalty (- $inf-penalty))        ;??

(define-class <fitness-class> (<object>)
  (fitness type: <fixnum>)
  (minimal-demerits init-value: $awful-bad)
  (best-place init-value: #f)           ; a <passive-node> or #f
  (best-place-line init-value: #f))     ; #f or a line number

(define (make-fitness-class-vector)
  (vector (make <fitness-class> fitness: $very-loose-fit)
          (make <fitness-class> fitness: $loose-fit)
          (make <fitness-class> fitness: $decent-fit)
          (make <fitness-class> fitness: $tight-fit)))


(define-class <packed-line> (<object>)
  (glue-set type: <glue-set>)
  (height type: <fixnum>)
  (depth type: <fixnum>)
  (line-number type: <fixnum>)
  (line type: <vector>))
  
