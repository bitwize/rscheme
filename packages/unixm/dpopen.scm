#|------------------------------------------------------------*-Scheme-*--|
 | %Z%1.2 %W% %G% 12:13:48
 |
 | Purpose:	`Direct' popen, using fork, exec, etc.
 | 
 |------------------------------------------------------------------------|
 | Notes:
 |	This is to provide more control over the arguments to
 |	the opened process (since popen uses shell expansion, it is
 |	a pain to deal with arguments with spaces, quotes, etc.
 |	in them)
 `------------------------------------------------------------------------|#


(define-class <pipe-input-port> (<fd-input-port>)
  subprocess)

(define-method close-input-port ((self <fd-input-port>))
  (fd-close (file-descriptor self))
  (wait-for (subprocess self)))
  
(define (popen*read program arglist)
  (format #t "popen* ==> ~s\n" program)
  (format #t "args => ~s\n" arglist)
  (bind ((read-side write-side (pipe))
	 (pid (fork)))
    (if pid
	(begin
	  (fd-close write-side)
	  (values read-side pid))
	(begin
	  (fd-close 1)
	  (fd-close read-side)
	  (fd-dup2 write-side 1)
	  (fd-close write-side)
	  (export-environ) ;; sync the process environment vars
	  (apply* program arglist exec)))))

(define (open-input-process2 program arglist)
  (bind ((fd pid (popen*read program arglist)))
    (make <pipe-input-port>
	  file-descriptor: fd
	  subprocess: pid)))

(define (open-input-process* . args)
  (let (((p <string>) (car args)))
    (open-input-process2 (if (string-search p #\/)
			     p
			     (find-in-path p))
			 args)))


