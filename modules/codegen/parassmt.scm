#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/codegen/parassmt.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.3
 | File mod date:    1997-11-29 23:10:33
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  codegen
 |
 `------------------------------------------------------------------------|#

;;;
;;;  Given a sequence of expressions which commute with each other,
;;;  and a set of registers to which they should be assigned,
;;;  compute a sequence of assignments (perhaps involving temporary
;;;  registers) which accomplishes the parallel assignment.
;;;
;;;  Note that the expressions may involve uses of registers

;;;
;;;  GIVEN:
;;;     exprs     (list <aml-expr>)
;;;     dest      (list <fixnum>)
;;;

(define (next-temp-to-use dest-regs in-regs)
  (let loop ((nxt (length in-regs))
	     (d dest-regs))
    (if (null? d)
	nxt
	(loop
	 (if (>= (car d) nxt)
	     (+ (car d) 1)
	     nxt)
	 (cdr d)))))
    
(define (gen-parallel-assmt (exprs <list>) (dest <list>) in-regs)
  (let ((stmts (help-par-assmt (map (lambda (d expr)
				      (cons* expr d (regs-used expr)))
				    dest
				    exprs)))
	(fix-temp! (let ((next-temp (next-temp-to-use dest in-regs)))
		     (lambda (ea)
		       (if (pair? ea)
			   (if (car ea)
			       (car ea)
			       (begin
				 (set-car! ea next-temp)
				 (set! next-temp (+ next-temp 1))
				 (car ea)))
			   ea)))))
    (map (lambda (s)
	   (let ((rhs (car s))
		 (lhs (fix-temp! (cadr s))))
	     (if (eq? rhs '=)
		 `(set! (reg ,lhs #f) 
			(<obj> ref (reg ,(fix-temp! (caddr s)) #f)))
		 `(set! (reg ,lhs #f) ,(obj-expr rhs)))))
	 stmts)))

#|
(define t1 '((x 1 2) (y 2 0)))
(define t2 '((a 0 1) (b 1 3) (c 3 2) (d 2 4) (e 4 0)))
(define t3 '((a 0 1) (b 1 3) (c 3 2)))
(define t4 '((a 1 2) (b 2 0) (c 5 3) (d 3 5)))
(define t5 '((a 1 2) (b 2 1) (c 4 3) (d 3 5)))
(define t6 '((a 0 3) (b 1 2) (c 2 1 0) (d 4 5) (e 5 4)))
(define t7 '((a 0 0 1)
	     (b 1 0 1)
	     (c 2 0 1)
	     (d 3 4)
	     (e 4 3)))

(define (t)
  (help-par-assmt t7))
|#

;;;

(define $nop '(nop))

(define (help-par-assmt (assmts <list>))
  (let* ((temps (map (lambda (k)
		       (list #f))
		     (range (length assmts))))
	 (work (map (lambda (t a)
		   (cons* (car a) t (cddr a)))
		 temps
		 assmts))
	 (finish (map (lambda (t a)
			(list '= (cadr a) t))
		      temps
		      assmts))
	 (vec (list->vector (append work finish))))
    (for-each (lambda (w f)
		(jam-together vec w f))
	      work
	      finish)
    (select (lambda (i)
	      (not (eq? i $nop)))
	    (vector->list vec))))

(define (jam-together (stmts <vector>) a b)
  (let ((i (move-stmt! stmts a 1))
	(j (move-stmt! stmts b -1)))
    (if (eq? (+ i 1) j)
	(begin
	  ;;-(format #t "moved together ~s and ~s...\n" a b)
	  (set-car! (cdr a) (cadr b))
	  (vector-set! stmts j $nop)))))

(define (vmemq+ key (v <vector>))
  (let (((n <fixnum>) (gvec-length v)))
    (let loop (((i <fixnum>) 0))
      (if (eq? i n)
	  #f
	  (if (eq? (vector-ref v i) key)
	      i
	      (loop (add1 i)))))))

(define (move-stmt! (stmts <vector>) (stmt <list>) (dir <fixnum>))
  (let ((lim (vector-length stmts))
	(i (vmemq+ stmt stmts)))
    (assert i)
    (let loop ((i i))
      (let ((try (+ i dir)))
	;;-(format #t "for ~s at ~d: try ~d\n" stmt i try)
	(if (and (>= try 0) 
		 (< try lim)
		 (not (overlap? stmt (vector-ref stmts try))))
	    (begin
	      (vector-set! stmts i (vector-ref stmts try))
	      (vector-set! stmts try stmt)
	      (loop try))
	    i)))))
	      
(define (overlap? (s1 <list>) (s2 <list>))
  (any? (lambda (i)
	  (memq i (cdr s2)))
	(cdr s1)))
