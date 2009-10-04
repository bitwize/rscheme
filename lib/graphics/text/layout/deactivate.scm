;;;
;;;  TeX#845
;;;
;;;  Insert a new active node from fc to p
;;;


(define (splice-active-nodes! (ctx <para-context>) prev-r lst)
  (let ((l (last-pair lst)))
    (if prev-r
        (begin
          (set-cdr! l (cdr prev-r))
          (set-cdr! prev-r lst)
          (values))
        (begin
          (set-cdr! l (active ctx))
          (set-active! ctx lst)))))

(define (make-delta-node (delta <space>))
  (make <delta-node>
        delta: delta))

(define (make-active-node (ctx <para-context>)
                          (fc <fitness-class>)
                          (p <list>)  ; in hlist
                          type)
  (assert (memq type '(hyphenated unhyphenated)))
  ;;
  (let* ((p-node (make <passive-node>
                       break: p
                       prev-break: (best-place fc)))
         (a-node (make <active-node>
                       passive: p-node
                       line-number-after: (+ 1 (best-place-line fc))
                       fitness: (fitness fc)
                       type: type
                       total-demerits: (minimal-demerits fc))))
    ;;
    (set-passive! ctx (cons p-node (passive ctx)))
    ;;
    a-node))




(define (make-para-context #key 
                           hlist 
                           (second-pass? default: #f)
                           (previous-line-number default: 0)
                           (threshold default: 100))    ; pretolerance=100
  (make <para-context>
        hlist: hlist
        second-pass?: second-pass?
        threshold: threshold
        active-width: (make <space>)
        active: (list
                 ;;  TeX#864  The active node that represents
                 ;;           the start of the paragraph, and does not
                 ;;           need a passive node
                 (make <active-node>
                       type: 'unhyphenated
                       fitness: $decent-fit
                       passive: #f         ; not needed
                       line-number-after: (+ previous-line-number 1)
                       total-demerits: 0))
        passive: '()))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

(define *htest* 
  (let ((f (get-text-font "Times" "Roman" 10)))
    (list (make <char-node>
                width: 8000
                height: 8000
                depth: 2000
                content: #\A
                font: f)
          (make <char-node>
                width: 8000
                height: 8000
                depth: 2000
                content: #\b
                font: f)
          (make <glue-node>
                content: (make <glue> natural: 5000))
          (make <char-node>
                width: 8000
                height: 8000
                depth: 2000
                content: #\c
                font: f))))

(define *test* (make-para-context
                hlist: *htest*))

(define (crude-insert-1)
  (let ((f1 (make <fitness-class> 
                  fitness: 1
                  minimal-demerits: 666
                  best-place-line: 1))
        (f2 (make <fitness-class> 
                  fitness: 2
                  minimal-demerits: 555
                  best-place-line: 1)))
    ;;
    ;; insert some stuff at the end
    ;;
    (splice-active-nodes!
     *test* 
     (active *test*)
     (list (make-delta-node (space+ (make <space>) -3))
           (make-active-node *test* f1 (cdr *htest*) 'unhyphenated)
           (make-active-node *test* f2 (cdr *htest*) 'unhyphenated)))
    ;;
    ;;  insert some more stuff at the (new) end
    (splice-active-nodes!
     *test*
     (last-pair (active *test*))
     (list (make-delta-node (space+ (make <space>) -3))
           (make-active-node *test* f1 (cdr *htest*) 'unhyphenated)
           (make-active-node *test* f2 (cdr *htest*) 'unhyphenated)))))

;(print *test*)

(define (di i #optional (caw default: (make <space>)))
  (deactivate *test* 
              (if (< i 2) #f (list-tail (active *test*) (- i 2)))
              (if (< i 1) #f (list-tail (active *test*) (- i 1)))
              (list-tail (active *test*) i)
              caw))

(define (d)
  (di 2))

|#
