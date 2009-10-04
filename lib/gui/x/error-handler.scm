(define-class <x-error> (<condition>)
  (x-display type: <x-display>)
  (error-type type: <symbol>)
  (properties type: <vector>))

(define-method display-object ((self <x-error>) port)
  (format port "** X error: ~a\n" (error-type self))
  (let loop ((i 0))
    (if (< i (vector-length (properties self)))
	(begin
	  (format port "** ~a ~s\n" 
		  (vector-ref (properties self) i)
		  (vector-ref (properties self) (+ i 1)))
	  (loop (+ i 2)))
	(values))))

(define (default-error-handler (display <x-display>)
	                       (type <symbol>)
			       #rest keywords)
  (signal (make <x-error>
		x-display: display
		error-type: type
		properties: (list->vector keywords))))

(define (invoke-error-handler (self <x-display>) (error-report <string>))
  (bind ((h (display-error-handler self))
	 (error-code error-type args (build-error-args error-report))
	 (proc (cond
		((procedure? h) h)
		((vector? h) (vector-ref h error-code))
		(else (list-ref h error-code)))))
    (apply proc self
	        error-type
		current-sequence: (sub1 (next-sequence-number self))
		args)))

(define (build-error-args (error-data <string>))
  (with-unpacked error-data
		 (u1: -
		  u1: code
		  u2: sequence-number
		  u4: info
		  u2: minor-opcode
		  u1: major-opcode)
   (let ((error-descr (vector-ref $error-descr (sub1 code))))
     (values code
	     (car error-descr)
	     (cons* major: major-opcode
		    minor: minor-opcode
		    sequence: sequence-number
		    (if (pair? (cdr error-descr))
			(list (cadr error-descr) info)
			'()))))))

(define $error-descr
  '#((request) ;; what the CLX code for a `request' error?
     (value value:)
     (window resource-id:)
     (pixmap resource-id:)
     (atom atom-id:)
     (cursor resource-id:)
     (font resource-id:)
     (match)
     (drawable resource-id:)
     (access)
     (alloc)
     (colormap resource-id:)
     (gcontext resource-id:)
     (id-choice resource-id:)
     (name)
     (length)
     (implementation)))
