#|------------------------------------------------------------*-Scheme-*--|
 | File:    compiler/cgen/wrvinsn.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.3
 | File mod date:    1997-11-29 23:10:27
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  (rsc)
 |
 `------------------------------------------------------------------------|#


(define (symbol+integer->hash si)
  (if (symbol? si)
      (symbol->hash si)
      (integer->hash si)))

(define (write-literal-c name literal-code c-name part-name)
    (newline)
    (center-* (current-output-port) "Raw glue `~a'" name)
    (for-each (lambda (i arg)
    		(format #t "#define ~a REG~d\n" arg i))
	      (range (length (vector-ref literal-code 0)))
	      (vector-ref literal-code 0))
    (newline)
    (display-some-monotones
	name
	c-name
	(let ((i -1))
	    (map (lambda (lc)
		    (if (pair? lc)
			;; the monotone was given a fixed name,
			;; so the glue code can refer to it
			lc
			;; it wasn't given a fixed name,
			;; so, give it a name of our own choosing
			(begin
			    (assert (string? lc))
			    (set! i (+ i 1))
			    (cons (string-append c-name "_" (number->string i))
			    	  lc))))
		(vector-ref literal-code 1)))
	(lambda (lit)
	    (display lit))
	part-name)
    (for-each (lambda (arg)
    		(format #t "#undef ~a\n" arg))
	      (vector-ref literal-code 0)))
		
(define (write-vinsns name vinsns c-name part-name)
    (newline)
    (center-* (current-output-port) "Function `~a'" name)
    (let ((new-names (make-table eq? symbol+integer->hash)))
	(display-some-monotones
	    name
	    c-name
	    (relabel-monotones
		c-name
		(vinsns->monotones* 0 vinsns)
		new-names)
	    (lambda (vinsns)
		(write-asm-stmts new-names vinsns))
	    part-name)))


(define (display-some-monotones name c-name monotones splat-proc part-name)
    (format #t "static char rsfn_~a_name[] = \"~a\";\n" c-name name)
    (format #t "#define FUNCTION rsfn_~a_name\n\n" c-name)
    (format #t "PROLOGUE(~a)\n\n" c-name)
    (format #t "BEGIN_FWD(~a)\n" c-name)
    (for-each
	(lambda (m)
	    (format #t "  FWD_MONOTONE(~a)\n" (car m)))
	monotones)
    (format #t "END_FWD(~a)\n\n" c-name)
    (for-each
     (lambda (i m)
       ;; hack a temporary definition of fplace codes for C coded
       ;; procs that is just the monotone #
       (format #t "#define FPLACE_CODE (1000+~d)\n" i)
       (format #t "MONOTONE(~a)\n{" (car m))
       (splat-proc (cdr m))
       (display "}\n#undef FPLACE_CODE\n\n"))
     (range (length monotones))
     monotones)
    (format #t "EPILOGUE(~a)\n\n" c-name)

    (format #t "BEGIN_BACK(~a)\n" c-name)
    (for-each
	(lambda (m)
	    (format #t "  BACK_MONOTONE(~a)\n" (car m)))
	monotones)
    (format #t "END_BACK(~a)\n\n" c-name)

    (format #t "static struct function_descr ~a_descr = {\n" 
		    c-name)
    ;;
    ;; function_descr.in_part
    ;;
    (format #t "\t&~a_part_~a,\n" 
	          (link-name *current-module*)
		  part-name)
    ;;
    ;; function_descr.monotones
    ;;
    (format #t "\tJUMP_TABLE( ~a ),\n" c-name)
    ;;
    ;; function_descr.name
    ;;
    (format #t "\trsfn_~a_name };\n" c-name)
    (format #t "#undef FUNCTION\n\n"))
