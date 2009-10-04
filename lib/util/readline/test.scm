(define (t)
  (init-for-readline 0)
  (with-output-to-port
      (open-output-nvt (current-output-port))
    (lambda ()
      (display "greetings\n")
      (format #t 
	      "line -> ~s\n" 
	      (xterm-readline "? " 
			      (current-input-port)
			      (current-output-port)
			      (make <readline-state>
				    history: '("foo" "bar")
				    completions: '("cons" "car" "cdr"))))))
				    
  (terminal-set-attr 0 *state*)
  (process-exit 0))

(define (r)
  (init-for-readline 0)
  (with-output-to-port
      (open-output-nvt (current-output-port))
    (lambda ()
      (with-input-from-port
	  (my-readline (current-input-port) (current-output-port))
	(lambda ()
	  (thread-let ((*console-input-port* (current-input-port))
		       (*console-output-port* (current-output-port)))
	    (with-module repl
	      (cmd-loop *self* "top[~d]> ")))))))
  (terminal-set-attr 0 *state*)
  (process-exit 0))

,(export t r)

|#
;; RUN
;(set-open-edit-port-proc! my-readline)
;;(r)


;; Need to figure out how to parse output from, eg, `infocmp -L xterm'

#|
interpretation of parameterized strings

so "%p1%d" means:  ((push-arg 1) (print "~d" (pop)))

            %%        outputs `%'
            %d        print pop() as in printf
            %2d       print pop() like %2d
            %3d       print pop() like %3d
            %02d
            %03d      as in printf
            %x        print pop() as in printf
            %2x       print pop() like %2x
            %3x       print pop() like %3x
            %02x
            %03x      as in printf
            %c        print pop() gives %c
            %s        print pop() gives %s

            %p[1-9]   push ith parm
            %P[a-z]   set variable [a-z] to pop()
            %g[a-z]   get variable [a-z] and push it
            %'c'      char constant c
            %{nn}     integer constant nn

            %+ %- %* %/ %m
                      arithmetic (%m is mod): push(pop() op pop())
            %& %| %^  bit operations: push(pop() op pop())
            %= %> %<  logical operations: push(pop() op pop())
            %A, %O    logical and & or operations (for conditionals)
            %! %~     unary operations push(op pop())
            %i        add 1 to first two parms (for ANSI terminals)

            %? expr %t thenpart %e elsepart %;
                      if-then-else, %e elsepart is optional.
                      else-if's are possible ala Algol 68:
                      %? c1 %t b1 %e c2 %t b2 %e c3 %t b3 %e c4 %t b4 %e %;
                      ci are conditions, bi are bodies.
|#
