#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/regex/pdaclass.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.4
 | File mod date:    1997-11-29 23:10:30
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  regex
 |
 | Purpose:          pushdown automata intermediate-code nodes
 `------------------------------------------------------------------------|#

(define $nop 0)
(define $match-char 1)
(define $match-any 2)
(define $match-char-set 3)

(define $match-str 4)
(define $accept 5)
(define $branch 6)
(define $save-place 7)

(define $match-star 9)    ;; kleene star of a single-char-matching thing
(define $match-plus 10)   ;; kleene plus of a single-char-matching thing

(define $match-end 11)    ;; match end of input
(define $match-start 12)  ;; match start of input

(define $reject 13) ;; not used as a <pda-state> opcode; implicit in next==#f
(define $jump 14)   ;; not used as a <pda-state> opcode;
                    ;; used in flattening machine to (regex) bytecodes

(define $opcode-vector 
  '#(nop 
     match-char any set match-str accept branch save ?
     star plus $ ^))

(define-class <pda-state> (<object>)
  (addr init-value: #f)	;; filled in when generating bytecodes
  (opcode type: <fixnum>)
  (data init-value: '())
  (next init-value: #f)
  (info init-value: #f))

(define-method write-object ((self <pda-state>) port)
  (format port "#<~s ~s>" 
	  (vector-ref $opcode-vector (opcode self)) 
	  (data self)))

;;
;; prints the entire machine
;;

(define-method print ((self <pda-state>))
  (let ((table (make-object-table))
	(n 0)
	(q '()))
    ;;
    (define (label thing)
      (if thing
	  (or (table-lookup table thing)
	      (let ((i n))
		(table-insert! table thing i)
		(set! n (+ i 1))
		(set! q (append q (list thing)))
		i))
	  'halt))
    ;;
    (label self)
    ;;
    (let loop ()
      (if (null? q)
	  (format #t "(~d pda-states)\n" n)
	  (let ((a (car q)))
	    (set! q (cdr q))
	    (format #t "   [~d] ~s\n" (label a) a)
	    (format #t "        next => [~s]\n" (label (next a)))
	    (if (and (pair? (data a))
		     (instance? (car (data a)) <pda-state>))
		(format #t "        data => [~s]\n" (label (car (data a)))))
	    (loop))))))
