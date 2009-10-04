
(define *show-xlation* #f)

(define (show-xlation msg . args)
  (if *show-xlation*
      (apply format #t msg args)))

(define (read-dylan-expr port)
  (if (instance? port <edit-input-port>)
      (set-use-secondary?! port #f))
  ;; read lines of input until we get a valid expr
  (let loop ((src (read-line port)))
    (if (equal? src "")
	(loop (read-line port))
	(if (eof-object? src)
	    (values 'eof src)
	    (bind ((expr left saw-end? (parse src)))
	      (if expr
		  (begin
		    (if (instance? port <edit-input-port>)
			(set-use-secondary?! port #f))
		    (values 'ok expr))
		  (if saw-end?
		      ;; the parse made it to the end of input,
		      ;; so maybe getting another line will improve things
		      (begin
			(if (instance? port <edit-input-port>)
			    (set-use-secondary?! port #t))
			(let ((more (read-line port)))
			  (if (eof-object? more)
			      (values 'eof more)
			      (loop (string-append src "\n" more)))))
		      ;; the parse never made it to the end of
		      ;; the input, so we have no hope of fixing things
		      ;; by trying to get more...
		      (begin
			(if (instance? port <edit-input-port>)
			    (set-use-secondary?! port #f))
			(if (and (> (string-length src) 1)
				 (eq? (string-ref src 0) #\,))
			    (values 'escape src)
			    (error "sorry, cannot parse input! ~s\n"
				   src))))))))))
  

(define (dylan-cmd-loop envt . opt)
  (let ((port (current-input-port)))
    (if (instance? port <edit-input-port>)
	(set-secondary-prompt! port "  "))
    (let loop ((i 0))
      (if (instance? port <edit-input-port>)
	  (set-primary-prompt! port
			       (if (null? opt)
				   "? "
				   (format #f (car opt) i))))
      (bind ((kind expr (read-dylan-expr port)))
	(if (eq? kind 'eof)
	    (values)
	    (if (eq? kind 'ok)
		(begin
		  (show-xlation "// ~@#*70s\n" expr)
		  (let ((x (xlate-forms envt expr)))
		    (show-xlation "// translated:\n")
		    (if *show-xlation* (pp x))
		    (for-each (lambda (e)
				(show-xlation "// eval ~@#*60s\n" e)
				(bind ((#rest r (eval e envt)))
				  (display-values r)))
			      x)
		    (loop (+ i 1))))
		(if (eq? kind 'escape)
		    (if (string=? expr ",,")
			(begin
			  (cmd-loop envt "RScheme[~d]=>")
			  (loop (+ i 1)))
			(bind ((#rest r (eval (escaped-datum expr) envt)))
			  (display-values r)
			  (loop (+ i 1)))))))))))

(define (escaped-datum str)
  (read (open-input-string
	 (if (and (> (string-length str) 2)
		  (string=? (substring str 0 2) ",,"))
	     (substring str 2)
	     str))))

#|
(define (rdyl-main args)
  (let ((p $console-output-port)
	(e (open-edit-port)))
(format p " ____  ____        _  _____________________________________________
|  _ \\|  _ \\ _   _| | A Not-Dylan System (Version 0.1)
| |_) | | | | | | | | based on ~a
|  _ <| |_| | |_| | | by Donovan Kolbly <donovan@tkg.com>
|_| \\_\\____/ \\__, |_| of The Kernel Group, Inc. http://www.tkg.com/
             |___/    _____________________________________________
"
*version*)

    (fluid-let ((*console-input-port* e)
		(*input-port* e)
		(*console-output-port* p)
		(*output-port* p)
		(*signal-handler* repl-signal-handler)
		(*top-level-envt* *self*))
      (register-interrupt-handler! 'control-c repl-interrupt)
      (dylan-cmd-loop))))
|#
#|
(define (main arg)
  (let ((p $console-output-port))
    (format p "/// Dylan Parser/Converter (1.0)\n")
    (format p "/// Raw output (raw.out)\n")
    (pp arg)
    (with-output-to-file
	"raw.out"
      (lambda ()
	(pp arg)))
    (let ((r (xlate-forms arg)))
      (format p "/// Translated output\n")
      (pp r))
    (fluid-let ((*console-input-port* (open-edit-port))
		(*console-output-port* $console-output-port)
		(*signal-handler* repl-signal-handler)
		(*top-level-envt* envt))
      (register-interrupt-handler! 'control-c repl-interrupt)
      (interpret-repl-args (cons "--" args) *self*))))
|#
