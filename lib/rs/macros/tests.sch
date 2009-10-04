; A short test suite.
; Uses PRETTY-PRINT and EVAL, neither of which is standard Scheme.
; Redefine f and g as desired to fix this.

; The value returned by MACRO-EXPAND is expressed using keywords
; defined in "prefs.sch", which might not be the keywords expected
; by EVAL.

(define begin0  'begin)
(define define0 'define)
(define quote0  'quote)
(define lambda0 'lambda)
(define if0     'if)
(define set!0   'set!)

(define begin2  begin1)
(define define2 define1)
(define quote2  quote1)
(define lambda2 lambda1)
(define if2     if1)
(define set!2   set!1)

(define original-code #f) ; assigned by f, used by g
(define expanded-code #f) ; assigned by f, used by g

(define (f x)
  (set! original-code x)
  (set! begin1  begin2)
  (set! define1 define2)
  (set! quote1  quote2)
  (set! lambda1 lambda2)
  (set! if1     if2)
  (set! set!1   set!2)
  (set! expanded-code (macro-expand x))
  (pretty-print expanded-code)
  )

(define (g answer)
  (set! begin1  begin0)
  (set! define1 define0)
  (set! quote1  quote0)
  (set! lambda1 lambda0)
  (set! if1     if0)
  (set! set!1   set!0)
  (if (not (equal? (eval (macro-expand original-code)) answer))
      (begin (newline)
             (display "TEST FAILED!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
             (newline)
             (display "Original code was:")
             (newline)
             (pretty-print original-code)
             (newline)
             (newline))
      #t))

(f '(let ((a 3)) a))
(g 3)

(f '(let ((x 'outer))
         (let-syntax ((m (syntax-rules () ((m) x))))
           (let ((x 'inner))
             (m)))))
(g 'outer)

(f '(let-syntax ((when (syntax-rules
                        ()
                        ((when ?test ?stmt1 ?stmt2 ...)
                         (if ?test (begin ?stmt1 ?stmt2 ...))))))
                (let ((if #t))
                  (when if (set! if 'now))
                  if)))
(g 'now)

(f '(letrec-syntax
     ((or (syntax-rules
           ()
           ((or) #f)
           ((or ?e) ?e)
           ((or ?e1 ?e2 ...)
            (let ((temp ?e1))
              (if temp temp (or ?e2 ...)))))))
     (let ((x #f)
           (y 7)
           (temp 8)
           (let odd?)
           (if even?))
       (or x
           (let temp)
           (if y)
           y))))
(g '7)

(f '(let ((=> #f))
         (cond (#t => 'ok))))
(g 'ok)

; This syntax of set*! matches that of an example in the R4RS.
; That example was put forth as an example of a hygienic macro
; that supposedly couldn't be written using syntax-rules.  Hah!

(f '(define-syntax set*!
      (syntax-rules
       ()
       ((set*! (?var ?val) ...)
        (set*!-help (?val ...) () (?var ?val) ...)))))

(f '(define-syntax set*!-help
      (syntax-rules
       ()
       ((set*!-help () (?temp ...) (?var ?val) ...)
        (let ((?temp ?val) ...)
          (set! ?var ?temp) ...))
       ((set*!-help (?var1 ?var2 ...) ?temps ?assignments ...)
        (set*!-help (?var2 ...) (temp . ?temps) ?assignments ...)))))

(f '(let ((x 3)
          (y 4)
          (z 5))
         (set*! (x (+ x y z))
                (y (- x y z))
                (z (* x y z)))
         (list x y z)))
(g '(12 -6 60))

(f
 '(let ((else #f))
       (cond (#f 3)
             (else 4)
             (#t 5))))
(g '5)

(f '(define-syntax push
      (syntax-rules ()
        ((push item place)
         (set! place (cons item place))))))

(f '(let* ((cons (lambda (name)
                   (case name
                     ((phil)  '("three-card monte"))
                     ((dick)  '("secret plan to end the war"
                                "agnew"
                                "not a crook"))
                     ((jimmy) '("why not the best"))
                     ((ron)   '("abolish the draft"
                                "balance the budget"))
                     (else    '()))))
           (scams (cons 'phil)))
          (push (car (cons 'jimmy)) scams)
          (push (cadr (cons 'ron)) scams)
          scams))
(g '("balance the budget" "why not the best" "three-card monte"))

; Tests of quasiquote and the vector extension.

(f '`(list ,(+ 1 2) 4))
(g '(list 3 4))

(f '(let ((name 'a)) `(list ,name ',name)))
(g '(list a (quote a)))

(f '`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b))
(g '(a 3 4 5 6 b))

(f '`((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))))
(g '((foo 7) . cons))

(f '`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8))
(g '#(10 5 2 4 3 8))

(f '(let ((who "hoo")
          (are "r")
          (you "u"))
         `#(#(,who ,are ,you)
            #(,@(list you are who))
            #(you are who?))))
(g '#(#("hoo" "r" "u") #("u" "r" "hoo") #(you are who?)))

; Tests of the ::: escape symbol.

; This syntax of set*! matches the syntax of an example in
; Clinger, "Macros in Scheme".  It is a slightly more difficult
; syntax than the syntax in R4RS.
; Note that pattern variables within the scope of the ::: are
; still expanded, so the auxiliary macro's pattern variables
; must be different from those of the outer macro.

(f '(define-syntax set*!
      (syntax-rules ()
       ((set*! i1 e1 more ...)
        (letrec-syntax
          ((set*!-aux
            (::: (syntax-rules ()
                  ((set*!-aux ((i1_ e1_ t1) ...))
                   (let ((t1 e1_) ...)
                     (set! i1_ t1) ...))
                  ((set*!-aux ((i1_ e1_ t1) ...) i2 e2 more_ ...)
                   (set*!-aux ((i1_ e1_ t1) ... (i2 e2 newtemp)) more_ ...))))))
          (set*!-aux () i1 e1 more ...))))))

(f '(let ((x 3)
          (y 4)
          (z 5))
         (set*! x (+ x y z)
                y (- x y z)
                z (* x y z))
         (list x y z)))
(g '(12 -6 60))

; Tests of the scoping extension.

(f '(define-syntax set! let*
      (syntax-rules (car cdr vector-ref)
        ((set! (car x) y)          (set-car! x y))
        ((set! (cdr x) y)          (set-cdr! x y))
        ((set! (vector-ref x e) y) (vector-set! x e y))
        ((set! x y)                (set! x y)))))

(f '(let* ((days (list 'monday 'wednesday 'friday))
           (day1 '(sunday)))
          (set! (car days) 'tuesday)
          (set! day1 (car days))
          day1))
(g 'tuesday)

(f '(define-syntax set! let*
      (syntax-rules (string-ref)
        ((set! (string-ref x e) y) (string-set! x e y))
        ((set! x y)                (set! x y)))))

(f '(let* ((o (make-string 3 #\o))
           (v (vector o o o))
           (s "woo"))
          (set! (string-ref o 0) #\h)
          (set! (vector-ref v 0) "boo")
          (set! s (string-append (vector-ref v 0)
                                 (vector-ref v 1)
                                 s))
          s))
(g '"boohoowoo")
