,(use rs.util.msgs)
,(use rs.util.subprocess)
,(use rs.sys.threads.manager)

(define-class <ps-font> (<object>)
  font-postscript-name
  font-size)

(define *tr12* (make <ps-font>
		     font-postscript-name: "Times-Roman"
		     font-size: 12))

;;; use ghostscript and pnm tools to generate exact (and smoothed) text

(define (exact-text ctm text font)
  (let ((tmp (string-append "/tmp/txt" (number->string (random 1000)))))
    ;;
    ;; generate the PostScript
    ;;
    (call-with-output-file
	(string-append tmp ".ps")
      (lambda (port)
	(format port "/~a findfont ~d scalefont setfont\n"
		(font-postscript-name font)
		(font-size font))
	(format port "2 2 scale\n")
	(format port "0 0 moveto\n")
	(format port "(~a) show\n" text)
	(format port "showpage\n")))
    ;;
    ;; generate the basic pbm
    ;;
    (let ((rc (exit-status (run "gs"
				"-sDEVICE=pbm"
				"-g200x200"
				(format #f "-sOutputFile=~a.pbm" tmp)
				(format #f "~a.ps" tmp)))))
      (if (equal? rc '#(exited 0))
	  tmp
	  (begin
	    (wm 210 "Ghostscript exec failed: ~s" rc)
	    #f)))))

(define (t)
  (exact-text #f "Bob" *tr12*))
