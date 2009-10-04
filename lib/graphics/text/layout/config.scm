
;; from plain.tex

(define *final-hyphen-demerits* 5000)
(define *double-hyphen-demerits* 10000)
(define *adj-demerits* 10000)
(define *line-penalty* 10)

(define *space-skip* 0.3333em)          ; glue between words
(define *xspace-skip* 0.5em)            ; glue after sentences
(define *pretolerance*  100)
(define *tolerance*   10000)

(define *hbadness* 1000)
(define *hfuzz* 100)                   ; overfull slop within which we won't complain

;;;

(define-class <para-context> (<object>)
  hlist
  threshold
  second-pass?
  active-width
  (active init-value: '())
  (passive init-value: '())
  (properties init-value: '#()))

;;;  Reconfigurable property accessors

(define (final-hyphen-demerits ctx)
  (get-property ctx 'final-hyphen-demerits *final-hyphen-demerits*))

(define (double-hyphen-demerits ctx)
  (get-property ctx 'double-hyphen-demerits *double-hyphen-demerits*))

(define (adj-demerits ctx)
  (get-property ctx 'adj-demerits *adj-demerits*))

(define (line-penalty ctx)
  (get-property ctx 'line-penalty *line-penalty*))


(define (space-skip)            ; glue between words
  (make <glue>
        natural: *space-skip*))

(define (xspace-skip)           ; glue after sentences
  (make <glue>
        natural: *xspace-skip*))

;;; (1) To center lines of a paragraph, set left-skip and right-skip to
;;;     something like 1fill
;;;
;;; (2) To justify lines, set left-skip and right-skip to $zero-glue
;;;
;;; (3) For ragged-right margins, set right-skip to 1fill
;;;
;;; (4) For right-justified lines (ragged-left), set left-skip to 1fill

;;; XXX this is a little evil; we should be using some kind
;;; of paragraph context instead of a dynamic variable...

(define-thread-var *para-config* '())

(define (with-config-parameter key value thunk)
  (thread-let ((*para-config* (cons (cons key value) *para-config*)))
    (thunk)))

(define (left-skip ctx) 
  (cond
   ((assq 'left-skip *para-config*)
    => cdr)
   (else
    $zero-glue)))

(define (right-skip ctx)
  (cond
   ((assq 'right-skip *para-config*)
    => cdr)
   (else
    $zero-glue)))

(define (background ctx)
  ;; most of TeX#827
  (space+ (space+ (make <space>) (left-skip ctx))
          (right-skip ctx)))

(define-thread-var *line-width-computer* (lambda (l)
                                           (values (qty->scaled (* 72 4)) l)))

(define (with-line-width width-in-pt thunk)
  (thread-let ((*line-width-computer* (lambda (l)
                                        (values (qty->scaled width-in-pt) l))))
    (thunk)))
