#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/hacks/disassem.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.9
 | File mod date:    2003-10-13 13:02:44
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  hacks
 |
 | Purpose:          functions for hacking on bytecoded programs
 `------------------------------------------------------------------------|#

;;;

(define-constant $default-tab-stops '(8 16 24 32 40 48 56 64 72))

(define-class <tabbing-port> (<output-port>)
  (current-column type: <fixnum> init-value: 1)
  (tab-stops type: <list> init-value: $default-tab-stops)
  (underlying-output-port type: <output-port>))

(define (with-output-to-tabbing-port thunk #key 
				     (tabs default: $default-tab-stops) 
				     (port default: (current-output-port)))
  (with-output-to-port
      (make <tabbing-port>
	    underlying-output-port: port
	    tab-stops: tabs)
    thunk))

(define (find-next-tab (self <tabbing-port>))
  (let ((x (current-column self)))
    (let loop ((t (tab-stops self)))
      (if (null? t)
	  x
	  (if (> (car t) x)
	      (car t)
	      (loop (cdr t)))))))
	
(define-method output-port-write-char ((self <tabbing-port>) ch)
  (case ch
    ((#\newline)
     (set-current-column! self 1)
     (newline (underlying-output-port self)))
    ((#\tab)
     (let ((x (find-next-tab self)))
       (write-string (underlying-output-port self)
		     (spaces (- x (current-column self))))
       (set-current-column! self x)))
    (else
     (write-char ch (underlying-output-port self))
     (set-current-column! self (+ (current-column self) 1)))))

;;;

(define (spaces n)
  (if (<= n 0)
      ""
      (make-string n #\space)))

;;
;;  return the bytecoded program (and instance of <byte-coded>)
;;  for the given template or function
;;

(define-method bytecodes ((self <function>))
  (bytecodes (template self)))

(define-method bytecodes ((self <template>))
  (let ((s (and (fixnum>? (gvec-length self) 3)
		(gvec-ref self 3))))
    (if (instance? s <byte-coded>)
	s
	(error "~s: not a byte-coded template" self))))

;;;

(define *bytecode-vec* #f)
(define *primop-vec* #f)
(define *tried-loading-disassembly-info* #f)

(define (load-disassembly-table bcstuff)
  ;;
  (let ((p (append-path bcstuff (string->file "bctable.dat"))))
    (if (file-exists? p)
	(let ((v (make-vector 256)))
	  (set! *bytecode-vec* v)
	  (for-each (lambda (opcode)
		      (vector-set! v (car opcode) opcode))
		    (with-input-from-file (pathname->os-path p) read)))))
  ;;
  (let ((p (append-path bcstuff (string->file "potable.dat"))))
    (if (file-exists? p)
	(let ((v (make-vector 256)))
	  (set! *primop-vec* v)
	  (for-each (lambda (primop)
		      (vector-set! v
				   (cdr (assq 'bytecode (vector-ref primop 3)))
				   primop))
		    (with-input-from-file (pathname->os-path p) read))))))

(define (force-disassembly-info)
  (if (not *tried-loading-disassembly-info*)
      (begin
	(set! *tried-loading-disassembly-info* #t)
	(load-disassembly-table 
	  (string->dir "[resource]/compiler/bytecode")))))

;;;  set up some handlers for disassembling additional info
;;;  about an instruction

(define (with-litnum self pc in-template vinsn proc)
  (if in-template
      (let ((litnum (cadr vinsn)))
	(if (symbol? litnum)
	    ;; this could be improved by indirecting through the symbol
	    ;; name via the bytecode arguments structure, which is knowable
	    ;; here
	    (proc (bvec-ref self (+ pc 1)))
            (if (and (pair? litnum)
                     (pair? (cdr litnum)))
                (case (cadr litnum)
                  ((<uint-8>)
                   (proc (bvec-ref self (+ pc 1))))
                  ((<uint-16>)
                   (proc (+ (* 256 (bvec-ref self (+ pc 1)))
                            (bvec-ref self (+ pc 2)))))
                  (else
                   (error "Don't know how to disassemble ~s" vinsn)))
                (proc litnum))))))

(define (disassem-tl-var (self <byte-coded>) (pc <fixnum>) in-template vinsn)
  (with-litnum self pc in-template vinsn 
	       (lambda (k)
		 (let ((v (gvec-ref in-template (+ k 4))))
		   (if (instance? v <binding>)
		       (format #t "\t; ~#*@33a" (name v))
		       (format #t "\t; ?? ~#*@30s" v))))))

(define (disassem-literal (self <byte-coded>) (pc <fixnum>) in-template vinsn)
  (with-litnum self pc in-template vinsn 
	       (lambda (k)
		 (format #t "\t; '~#*@32s" (gvec-ref in-template (+ k 4))))))


(define (disassem-lex-ref (self <byte-coded>) (pc <fixnum>) in-template vinsn)
  (let ((frame-info (and in-template
			 (get-debug-info in-template pc 'envt-frames))))
    (if frame-info
	(format #t "\t; ~s" 
		(vector-ref
		 (vector-ref frame-info (cadr vinsn))
		 (caddr vinsn))))))

(define (disassem-branch-if-false (self <byte-coded>) (pc <fixnum>) 
                                  in-template vinsn)
  (let ((offset (+ (* 256 (bvec-ref self (+ pc 1)))
                   (bvec-ref self (+ pc 2)))))
    (format #t "\t; ~d" offset)))

(%early-once-only
(define *bytecode-arg-disassemblers*
  (list (cons 'tl-ref disassem-tl-var)
	(cons 'tl-ref/bound disassem-tl-var)
	(cons 'literal disassem-literal)
	(cons 'lex-ref disassem-lex-ref)
        (cons 'branch-if-false disassem-branch-if-false))))

(define (disassemble-insn (self <byte-coded>) (pc <fixnum>) in-template)
  ;; it's a normal bytecode
  (let ((d (vector-ref *bytecode-vec* (bvec-ref self pc))))
    ;(format #t "~d => ~s\n" (bvec-ref self pc) d)
    (if d
	(let ((vinsn (cadr d))
	      (args (caddr d))
	      (extra-bytes (cadddr d)))
	  ;; render the extra bytes
	  (let iloop ((j (+ pc 1)) 
		      (n extra-bytes))
	    (if (> n 0)
		(begin
		  (format #t " ~02x" (bvec-ref self j))
		  (iloop (+ j 1) (- n 1)))
		(let ((v (format #f "~s" vinsn)))
		  (format #t "\t| ~a" v)
		  (let ((x (assq (car vinsn) *bytecode-arg-disassemblers*)))
		    (if x
			((cdr x) self pc in-template vinsn))
		    (newline)
		    j)))))
	(begin
	  (format #t "\t?\n")
	  (+ pc 1)))))

(define (disassemble-primop (self <byte-coded>) (pc <fixnum>))
  (let* ((pon (bvec-ref self (+ pc 1)))
	 (po (vector-ref *primop-vec* pon)))
    (if po
	;; check to see if it has a return value
	(if (vector-ref po 2)
	    (format #t 
		    " ~02x\t| ~s ~s => ~s\n"
		    pon
		    (vector-ref po 0)
		    (vector-ref po 1)
		    (vector-ref po 2))
	    (format #t 
		    " ~02x\t| ~s ~s\n"
		    pon
		    (vector-ref po 0)
		    (vector-ref po 1)))
	(format #t " ~d ILLEGAL PRIMOP\n" pon))
    (+ pc 2)))

;;; if `in-template' is not #f, it should be a <template>,
;;; and this procedure will lookup objects referenced by the
;;; bytecodes

(define (disassemble* (self <byte-coded>) 
                      in-template
                      #optional program-counter)
  (force-disassembly-info)
  (let loop ((i 0))
    (if (< i (bvec-length self))
	(let ((c (bvec-ref self i)))
          (if program-counter
              (if (= program-counter i)
                  (format #t "===> ")
                  (format #t "     ")))
	  (format #t "~-3d: ~02x" i c)
	  (loop (if (eq? c 255)
		    (disassemble-primop self i)
		    (disassemble-insn self i in-template))))
	self)))

(define (disassemble (self <byte-coded>) 
                     in-template
                     #optional program-counter)
  (with-output-to-tabbing-port
   (lambda ()
     (disassemble* self in-template program-counter))
   tabs: '(20 42)))

;;;

(define-method print ((self <byte-coded>))
  (force-disassembly-info)
  (if (not (and *bytecode-vec* *primop-vec*))
      (print-bvec self)
      (disassemble self #f)))

(define-method print ((self <template>))
  (let ((bc (gvec-ref self 3)))
    (if (instance? bc <byte-coded>)
	(begin
	  (force-disassembly-info)
	  (if (not (and *bytecode-vec* *primop-vec*))
	      (print-gvec self)
	      (disassemble bc self)))
	(print-gvec self))))

#|

(define (test-proc a b c)
  (let ((sum (+ a b c)))
    (list 'sum sum (lambda () b))))

;;; NOTE -- this could easily be invalidated by changes in the compiler

(define $test-debug-info
  '((envt-frames . #(2 18 #(#(a b c))))
    (source-location . #(0 2 (274 "disassem.scm")
			 2 10 (275 "disassem.scm")
			 10 18 (276 "disassem.scm")))
    (registers . #(0 2 (c b a)
		   9 11 (sum)))))

(define (t)
  (set-function-descr! 
   (template test-proc)
   (cons (cons 'debug-info $test-debug-info)
	 (function-descr (template test-proc))))
  (print (template test-proc)))

|#
