(load "arrow.scm")

(define-class <oi-diagram> (<object>)
  max-time
  oi-actors
  oi-items)

(define (oi-actor (self <oi-diagram>) n)
  (or (table-lookup (oi-actors self) n)
      (error "unknown actor `~s'" n)))
  
(define-class <item> (<object>)
  (position init-value: #f))

(define-class <actor> (<item>)
  name
  label
  ;; these are times in object-interaction time units
  ;;    0 = start of picture,
  ;; with the special value,
  ;;    end = maximum value 
  (start-time init-value: #f)            ; time of construction
  (end-time init-value: #f))             ; time of destruction

(define-class <activation> (<item>)
  of-actor
  (start-time init-value: #f)
  (end-time init-value: #f))

;; a time gap -- useful to not only draw a gap symbol,
;; but also to reset the timebase

(define-class <gap> (<item>)
  start-time
  end-time)

;;

(define-class <invocation> (<item>)
  from-actor
  to-actor
  to-activation
  label
  (start-time init-value: #f))

;;;

(define-method referenced-actors ((self <item>))
  '())

(define-method referenced-actors ((self <activation>))
  (list (of-actor self)))

(define-method referenced-actors ((self <invocation>))
  (list (from-actor self)
        (to-actor self)))

;;;

(define (figscale n)
  ; convert 4x mm to points
  (* 72 (/ n 4 25.4)))

(define (time->y t)
  (if (eq? t 'end)
      (time->y *max-time*)
      (- (* t (figscale 0.5)))))


(define-class <construction> (<invocation>))
(define-class <destruction> (<invocation>))

;;;

(define (text-block x y lineheight halign valign font str)
  (let ((l (string-split str #\newline)))
    (let loop ((l l)
               (r '())
               (y (if (eq? valign 'bottom)
                      (+ y (* (- (length l) 1) lineheight))
                      y)))
      (if (null? l)
          (if (null? (cdr r))
              (car r)
              (cons 'group (reverse! r)))
          (loop (cdr l)
                (cons `(text string: ,(car l)
                             origin-x: ,x
                             origin-y: ,y
                             alignment: ,halign
                             font: ,font)
                      r)
                (- y lineheight))))))
;;;

(define *header-font* '(font "Helvetica" "Regular" 6))
(define *header-linespacing* (cadddr *header-font*))

(define *message-font* '(font "Helvetica" "Regular" 5))
(define *message-linespacing* (cadddr *message-font*))
(define *message-origin-dx* (figscale 3))
(define *message-origin-dy* (figscale 5))

(define *no-such-object-dash* '(1.5 2))
(define *header-margin* (figscale 11))
(define *actor-spacing* (figscale 128))
(define *line-weight* 0.5)

(define *gap-m-dx* (figscale 2.5))
(define *gap-m-dy* (figscale 5))
(define *gap-m-overlap* (figscale 1))

(define *max-time* 100)
(define *t-min* 22)
(define *t-first* 15)
(define *t-next* 25)
(define *t-space* 25)
(define *t-gap* 12)     ; above and beyond the *t-next* before and after

(define *activation-box-width* (figscale 10))
(define *pre-activation-margin* (figscale 2.5))

(define (build-items src in t)
  (format #t "~d (~s) - ~#@*50s\n" t in src)
  (case (car src)
    ((gap)
     (values (list (make <gap> 
                         start-time: t
                         end-time: (+ t *t-gap*)))
             (+ t *t-gap*)))
    ((space)
     (values '() (+ t *t-space*)))
    ((seq)
     (if (null? (cdr src))
         (values '() t)
         (let loop ((sub (cdr src))
                    (t (+ t *t-first*))
                    (r '()))
           (bind ((l tn (build-items (car sub) in t)))
             (if (null? (cdr sub))
                 (values (append r l) tn)
                 (loop (cdr sub)
                       (+ tn *t-next*)
                       (append r l)))))))
    ((activation)
     (let* ((s (car (cadr src)))
            (sub (cddr src))
            (a (make <activation>
                     of-actor: s
                     start-time: t
                     end-time: #f)))
       (bind ((l tend (if (null? sub)
                          (values '() (+ t *t-min*))
                          (build-items (cons 'seq sub) s t))))
         (set-end-time! a tend)
         (values (cons a l) tend))))
    ((call create delete)
     (let* ((s (car (cadr src)))
            (d (cadr (cadr src)))
            (m (caddr (cadr src)))
            (sub (cddr src))
            (a (make <activation>
                     of-actor: d
                     start-time: t
                     end-time: #f))
            (c (make (case (car src)
                       ((call) <invocation>)
                       ((create) <construction>)
                       ((delete) <destruction>))
                     from-actor: (if (eq? s '...) in s)
                     to-actor: d
                     to-activation: a
                     label: m
                     start-time: t)))
       (bind ((l tend (if (null? sub)
                          (values '() (+ t *t-min*))
                          (build-items (cons 'seq sub) d t))))
         (set-end-time! a tend)
         (values (cons* c a l) tend))))))

(define (t)
  (bind ((l t (build-items *test* #f 0)))
    (set! *max-time* (+ t *t-space*))
    (values l t)))

(define *test* '(seq
                 (activation (aCaretaker)
                             (call (... anOriginator "CreateMemento()")
                                   (create (... aMemento "new Memento"))
                                   (call (... aMemento "SetState()"))))
                 (gap)
                 (activation (aCaretaker)
                             (call (aCaretaker anOriginator "SetMemento(aMemento)")
                                   (call (... aMemento "GetState()"))
                                   (space)))))


#|
call aCaretaker -> anOriginator "CreateMemento()"
{
  create -> aMemento "new Memento" {}
  call -> aMemento "SetState()" {}
}
:
call aCaretaker -> anOriginator "SetMemento(aMemento)"
{
  call -> aMemento "GetState()" {}
}
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-thread-var *oi*)

(define-method extern-rep ((self <oi-diagram>))
  (thread-let ((*oi* self))
    (list 'group
          (cons 'group
                (map extern-rep (value-sequence (oi-actors self))))
          (cons 'group
                (map extern-rep (oi-items self))))))
  
(define-method extern-rep ((self <item>))
  '(group))

(define-method extern-rep ((self <invocation>))
  (let* ((a-from (oi-actor *oi* (from-actor self)))
         (a-to (oi-actor *oi* (to-actor self)))
         (x-from (x (position a-from)))
         (x-to (x (position a-to)))
         (y (time->y (start-time self))))
    `(group
      ,(text-block (+ x-from 
                      (/ *activation-box-width* 2) 
                      *message-origin-dx*)
                   (+ y *message-origin-dy*)
                   *message-linespacing*
                   'left
                   'bottom
                   *message-font*
                   (label self))
      ,(hline-with-arrow (make-point (+ x-from (/ *activation-box-width* 2)) y)
                         (make-point (- x-to (/ *activation-box-width* 2)) y)
                         1
                         (dash-type self)))))

(define-method dash-type ((self <invocation>))
  #f)

(define-method dash-type ((self <construction>))
  *no-such-object-dash*)

(define-method dash-type ((self <destruction>))
  *no-such-object-dash*)

(define-method extern-rep ((self <activation>))
  (let ((hw (/ *activation-box-width* 2)))
    `(box origin-x: ,(- (x (position (oi-actor *oi* (of-actor self)))) hw)
          origin-y: ,(time->y (end-time self))
          width: ,*activation-box-width*
          height: ,(- (+ (time->y (start-time self))
                         *pre-activation-margin*)
                      (time->y (end-time self)))
          fill-color: white
          stroke-color: black)))
  
(define-method extern-rep ((self <actor>))
  (let ((ax (x (position self))))
    `(group
      ,(text-block ax
                   *header-margin*
                   *header-linespacing*
                   'center
                   'bottom
                   *header-font*
                   (label self))
      (path stroke-width: ,*line-weight*
            stroke-color: black
            subpaths: ((subpath points: ((path-point
                                          x: ,ax
                                          y: ,(time->y (start-time self)))
                                         (path-point
                                          x: ,ax
                                          y: ,(time->y (end-time self)))))))
      ,@(if (zero? (start-time self))
            '()
            (list (dash-line *no-such-object-dash* 
                             ax 0 
                             ax (time->y (start-time self)))))
      ,@(if (eq? (end-time self) 'end)
            '()
            (list (dash-line *no-such-object-dash*
                             ax (time->y (end-time self)) 
                             ax (time->y 'end)))))))

(define (dash-line d x0 y0 x1 y1)
  `(path dashing: ,d
         stroke-width: ,*line-weight*
         stroke-color: black
         subpaths: ((subpath points: ((path-point x: ,x0 y: ,y0)
                                      (path-point x: ,x1 y: ,y1))))))

(define-method extern-rep ((self <gap>))
  (let* ((mid-y (time->y (/ (+ (start-time self)
                               (end-time self))
                            2)))
         (y0 (+ mid-y *gap-m-dy*))
         (y1 (- mid-y *gap-m-dy*))
         (o (/ *gap-m-overlap* 2)))
    (cons 'group
          (map
           (lambda (a)
             (let ((x0 (- (x (position a)) *gap-m-dx*))
                   (x1 (+ (x (position a)) *gap-m-dx*)))
               `(group
                 (path fill-color: white
                       subpaths: ((subpath 
                                   points: ((path-point x: ,x0
                                                        y: ,(- mid-y o))
                                            (path-point x: ,x1
                                                        y: ,y0)
                                            (path-point x: ,x1
                                                        y: ,(+ mid-y o))
                                            (path-point x: ,x0
                                                        y: ,y1))
                                   closed?: #t)))
                 (path stroke-color: black
                       stroke-width: ,*line-weight*
                       subpaths: ((subpath
                                   points: ((path-point x: ,x0
                                                        y: ,(- mid-y o))
                                            (path-point x: ,x1
                                                        y: ,y0)))))
                 (path stroke-color: black
                       stroke-width: ,*line-weight*
                       subpaths: ((subpath
                                   points: ((path-point x: ,x0
                                                        y: ,y1)
                                            (path-point x: ,x1
                                                        y: ,(+ mid-y o)))))))))
           (value-sequence
            (oi-actors *oi*))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-object-interaction oi)
  (bind ((items (cdr oi))
         (actors (car oi))
         (l t (build-items (cons 'seq (cdr oi)) #f 0)))
    (set! *max-time* (+ t *t-space*))
    (let* ((at (make-symbol-table))
           (d (make <oi-diagram>
                    max-time: *max-time*
                    oi-items: l
                    oi-actors: at)))
      (define (get-actor n)
        (let ((a (table-lookup at n)))
          (or a
              (let ((a (make <actor>
                             name: n
                             label: (symbol->string n))))
                (table-insert! at n a)
                (format (current-error-port) "Unknown actor: `~s'\n" n)
                a))))
      ;;
      (for-each 
       (lambda (i n)
         (let ((a (make <actor>
                        name: n
                        label: (symbol->string n)
                        position: (make-point (* i *actor-spacing*) 0))))
           (table-insert! at n a)))
       (range (length actors))
       actors)
      ;;
      (for-each
       get-actor
       (apply append (map referenced-actors l)))
      ;;
      (for-each
       (lambda (i)
         (set-start-time! (get-actor (to-actor i)) (start-time i)))
       (select (lambda (i) (instance? i <construction>)) l))
      ;;
      (for-each
       (lambda (i)
         (set-end-time! (get-actor (to-actor i)) 
                        (end-time (to-activation i))))
       (select (lambda (i) (instance? i <destruction>)) l))
      ;;
      (for-each
       (lambda (a)
         (if (not (start-time a))
             (set-start-time! a 0))
         (if (not (end-time a))
             (set-end-time! a 'end)))
       (value-sequence at))
      ;;
      d)))

(define (read-form-list port)
  (let loop ((r '()))
    (let ((i (read port)))
      (if (eof-object? i)
          (reverse r)
          (loop (cons i r))))))

(define (read-dv-macro port)
  (let* ((content (read-form-list port))
         (oi (parse-object-interaction content))
         (ex (extern-rep oi)))
    ;(pp ex)
    (document-w-macro (lambda () ex))))
