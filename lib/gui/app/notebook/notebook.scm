
(define-class <cell-formatter> (<object>) :abstract)

(define-class <std-cell-formatter> (<cell-formatter>)
  (font type: <text-font>)
  (alignment type: <symbol>))

(define-class <std-prefix-formatter> (<std-cell-formatter>)
  (prefix type: <vector>)
  (prefix-font type: <text-font>))

(define-class <cell> (<object>) :abstract
  (properties type: <vector> init-value: '#())
  (children type: <vector> init-value: '#()))

(define-class <group-cell> (<cell>))
(define-class <root-cell> (<cell>))

(define-class <text-cell> (<cell>)
  (cell-lines type: <vector>)
  (cell-formatter type: <cell-formatter>))

(define *in-formatter* (make <std-prefix-formatter>
			     font: (find-font "Courier" "Bold" 12)
			     alignment: 'left
			     prefix: '#("In[~d]:=" number)
			      prefix-font: (find-font "Times" "Italic" 9)))
(define *out-formatter* (make <std-prefix-formatter>
			      font: (find-font "Courier" "" 12)
			      alignment: 'left
			      prefix: '#("Out[~d]=" number)
			      prefix-font: (find-font "Times" "Italic" 9)))
(define *def-formatter* (make <std-formatter>
			      font: (find-font "Courier" "Bold" 12)
			      alignment: 'left))
(define *h3-formatter* (make <std-formatter>
			     font: (find-font "Times" "Italic" 14)
			     alignment: 'left))
(define *h2-formatter* (make <std-formatter>
			     font: (find-font "Times" "Bold" 18)
			     alignment: 'left))
(define *h1-formatter* (make <std-formatter>
			     font: (find-font "Times" "Bold" 24)
			     alignment: 'center))

#|
(define-method compute-cell-size ((self <std-formatter>) cell)
  (make-size ...)

(define-method compute-cell-size ((self <std-prefix-formatter>) cell)
  (let ((size (next-method)))
    (make-size (width size)
	       (+ (height size) (font-size (prefix-font self))))))
  
(define-method render-cell ((self <std-prefix-formatter>) cell)
  (next-method)
  ...)
|#

(define (t)
  (let* ((out1 (make <text-cell>
		     properties: '#(number 1)
		     cell-lines: '#("(1 2 3)")
		     cell-formatter: *out-formatter*))
	 (in1 (make <text-cell>
		    properties: '#(number 1)
		    cell-lines: '#("(cdr (range 4))")
		    cell-formatter: *in-formatter*))
	 (io1 (make <group-cell>
		    contents: (vector in1 out1)))
	 (d3 (make <text-cell>
		   cell-lines: '#("(define (bar x)"
				   "  (list (foo x)"
				   "        (foo x)))")
		   cell-formatter: *def-formatter*))
	 (d2 (make <text-cell>
		   cell-lines: '#("(define (foo n)"
				   "  (list n 1))")
		   cell-formatter: *def-formatter*))
	 (d1 (make <text-cell>
		   cell-lines: '#("(define-module rs.lang ()")
		   cell-formatter: *def-formatter*))
	 (t3 (make <text-cell>
		   cell-lines: '#("Standard RScheme")
		   cell-formatter: *h3-formatter*
		   children: (vector d1 d2 d3 io1)))
	 (t2 (make <text-cell>
		   cell-lines: '#("RScheme Base Implementation")
		   cell-formatter: *h2-formatter*
		   children: (vector t3)))
	 (t1 (make <text-cell>
		   cell-lines: '#("RScheme")
		   cell-formatter: *h1-formatter*
		   children: (vector t2)))
	 (r (make <root-cell>
		  children: (vector t1))))
    r))
