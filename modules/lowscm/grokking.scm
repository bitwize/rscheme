#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/lowscm/grokking.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.12
 | File mod date:    2004-02-24 08:57:58
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  low-scheme
 |
 | Purpose:          General lower level list operations
 `------------------------------------------------------------------------|#

; List Access Functions

(define-macro (define-cNr (name arg) expr)
  (let ((full (string->symbol
	       (string-append
		(symbol->string name)
		"-proc"))))
    `(begin
       (define (,full ,arg) ,expr)
       (define-syntax ,name
	 (syntax-form (,arg)
	   ,expr)
	 (else ,full)))))

; c[ad]^2r

(define-cNr (caar x) (car (car x)))
(define-cNr (cadr x) (car (cdr x)))
(define-cNr (cddr x) (cdr (cdr x)))
(define-cNr (cdar x) (cdr (car x)))

; c[ad]^3r

(define-cNr (caaar x) (car (car (car x))))
(define-cNr (caadr x) (car (car (cdr x))))
(define-cNr (caddr x) (car (cdr (cdr x))))
(define-cNr (cadar x) (car (cdr (car x))))

(define-cNr (cdaar x) (cdr (car (car x))))
(define-cNr (cdadr x) (cdr (car (cdr x))))
(define-cNr (cdddr x) (cdr (cdr (cdr x))))
(define-cNr (cddar x) (cdr (cdr (car x))))

; c[ad]^4r

(define-cNr (caaaar x) (car (car (car (car x)))))
(define-cNr (caaadr x) (car (car (car (cdr x)))))
(define-cNr (caaddr x) (car (car (cdr (cdr x)))))
(define-cNr (caadar x) (car (car (cdr (car x)))))

(define-cNr (cadaar x) (car (cdr (car (car x)))))
(define-cNr (cadadr x) (car (cdr (car (cdr x)))))
(define-cNr (cadddr x) (car (cdr (cdr (cdr x)))))
(define-cNr (caddar x) (car (cdr (cdr (car x)))))

(define-cNr (cdaaar x) (cdr (car (car (car x)))))
(define-cNr (cdaadr x) (cdr (car (car (cdr x)))))
(define-cNr (cdaddr x) (cdr (car (cdr (cdr x)))))
(define-cNr (cdadar x) (cdr (car (cdr (car x)))))

(define-cNr (cddaar x) (cdr (cdr (car (car x)))))
(define-cNr (cddadr x) (cdr (cdr (car (cdr x)))))
(define-cNr (cddddr x) (cdr (cdr (cdr (cdr x)))))
(define-cNr (cdddar x) (cdr (cdr (cdr (car x)))))

; other stuff

(define-syntax (ok-so-far seen item)
  (and (pair? item)
       (not (memq item seen))))

;;;  `list?' is non-trivial because we're required to reject
;;;  circular structures.


(define (list? x)
  (cond
   ((pair? x)
    (if (pair? (cdr x))
        ;; start racing through the structure.
        ;; this approach is linear in proving listness, 
        ;; which is better than the old quadratic implementation
        (let loop ((a (cdr x))          ; a is the fast mover
                   (b x))               ; b is the slow mover
          (if (eq? a b)
              #f                ; common object in chasings; must be circular
              (if (and (pair? a)
                       (pair? (cdr a)))
                  (loop (cddr a) (cdr b))
                  (or (null? a)
                      (and (pair? a)
                           (null? (cdr a)))))))
        (null? (cdr x))))
   ((null? x)
    #t)
   (else
    #f)))

(define (last-pair x)
  (if (pair? x)
      (let loop (((t <pair>) x))
	(let ((c (cdr t)))
	  (if (pair? c)
	      (loop c)
	      (if (null? c)
		  t
		  (error "last-pair: not a list at: ~s" t)))))
      (error "last-pair: not a pair to start: ~s" x)))

(define (append! target . new-tails)
  (let tloop ((target target)
	      (new-tails new-tails))
    (if (null? target)
	(if (pair? new-tails)
	    (tloop (car new-tails) (cdr new-tails))
	    '())
	(let loop ((t new-tails)
		   (p target))
	  (if (null? t)
	      target
	      (let ((new-tail (car t)))
		(if (null? new-tail)
		    (loop (cdr t) p)
		    (begin
		      (set-cdr! (last-pair p) new-tail)
		      (loop (cdr t) new-tail)))))))))

(define (last l)
  (car (last-pair l)))

(define (nth-cdr* lst (n <fixnum>) oops)
  (let loop ((l lst) 
	     ((i <fixnum>) n))
    (cond
     ((eq? i 0)
      l)
     ((pair? l)
      (loop (cdr l) (sub1 i)))
     ((not (null? l))
      (signal-improper-list lst l))
     (else
      (oops lst n)))))

(define (list-tail lst (n <fixnum>))
  (nth-cdr* lst n signal-no-such-key))

(define (nth-pair lst k)
  (let ((t (nth-cdr* lst k signal-no-such-key)))
    (cond
     ((pair? t)
      t)
     ((null? t)
      (signal-no-such-key lst k))
     (else
      (signal-improper-list lst t)))))

(define (list-ref lst k)
  (car (nth-pair lst k)))

(define (list-set! lst k value)
  (let* (((p <pair>) (nth-pair lst k))
         (o (car p)))
    (set-car! p value)
    o))

