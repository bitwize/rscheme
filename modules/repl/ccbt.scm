#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/repl/ccbt.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.7
 | File mod date:    2003-05-30 21:27:26
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  repl
 |
 | Purpose:          continuation chain backtrace
 `------------------------------------------------------------------------|#

(define (partial-continuation? thing)
  (instance? thing <partial-continuation>))

(define *bci-addrs* #f)

(define (get-bci-addrs)
  (or *bci-addrs*
      (bind ((module-descr (find-linked-module "bci"))
             (part-descr (find-part-in-linked-module module-descr 8902))
             (entry fn-descr (find-code-ptr-in-part part-descr 0))
             (name addrs (get-c-function-descr fn-descr)))
        (set! *bci-addrs* addrs)
        addrs)))

(define (partial-continuation-bci? (pc <partial-continuation>))
  (memq (pc-jump-addr pc) (get-bci-addrs)))

(define (pc-envt-reg pc)          (gvec-ref pc 0))
(define (pc-template-reg pc)      (gvec-ref pc 1))
(define (pc-jump-addr pc)         (gvec-ref pc 2))
(define (pc-continuation-reg pc)  (gvec-ref pc 3))

(define (pc-bci-program-counter pc)  (gvec-ref pc 4))

(define (pc-regs pc)
  (let ((n (if (partial-continuation-bci? pc) 5 4)))
    (let loop ((i (gvec-length pc))
               (r '()))
      (if (> i n)
          (loop (sub1 i) (cons (gvec-ref pc (sub1 i)) r))
          r))))

(define (pc-reg-ref pc reg)
  (list-ref (pc-regs pc) reg))

(define (pc-envt-ref pc frame slot)
  (let loop ((f frame)
             (e (pc-envt-reg pc)))
    (if (zero? f)
        (gvec-ref e (+ slot 1))
        (loop (- f 1) (gvec-ref e 0)))))

;; llc is a <closure> whose binding envt
;; contains a partial cont

(define (ll->partial (llc <function>))
  (gvec-ref (environment llc) 1))

(define (print-bdg-envt frame-number frame)
  (let ((num-slots (sub1 (gvec-length frame))))
    (let loop ((slot 0))
      (if (< slot num-slots)
	  (begin
	    (format #t "     ~d:~d => " frame-number slot)
	    (display (object->bounded-string (current-display-limit) 
					     (gvec-ref frame (+ slot 1))))
	    (newline)
	    (loop (+ slot 1)))))))

(define (print-bdg-envt-chain frame-number frame)
  (if (instance? frame <binding-envt>)
      (begin
	(print-bdg-envt frame-number frame)
	(print-bdg-envt-chain (+ frame-number 1) (gvec-ref frame 0)))))

(define (print-1-pc (pc <partial-continuation>))
  (let ((r (pc-regs pc))
	(t (pc-template-reg pc))
	(e (pc-envt-reg pc)))
    (format #t "will continue at:\n")
    (format #t "    program counter: ")
    (if (partial-continuation-bci? pc)
        (format #t "~d (bci) " (pc-bci-program-counter pc))
	(format #t "#x~r  " (pc-jump-addr pc)))
    (format #t "~a\n" (name t))
    (let reg-loop ((i 0) 
		   (r r))
      (if (pair? r)
	  (begin
	    (format #t "    REG~d => " i)
	    (display (object->bounded-string (current-display-limit) (car r)))
	    (newline)
	    (reg-loop (+ i 1) (cdr r)))
	  (if (not (null? e))
	      (if (instance? e <binding-envt>)
		  (begin
		    (format #t "    in binding environment:\n")
		    (print-bdg-envt-chain 0 e))
		  (format #t "    in: ~s\n" e)))))))

(define-method print ((self <partial-continuation>))
  (let loop ((pc self))
    (if (partial-continuation? pc)
	(begin
	  (print-1-pc pc)
	  (loop (pc-continuation-reg pc)))
	self)))

(define (ccbt)
  (low-level-call/cc
   (lambda (ll-continuation)
     (print (ll->partial ll-continuation))))
  (values))

(define (show-bt envt) 
  (ccbt))

