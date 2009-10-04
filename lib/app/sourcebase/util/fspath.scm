
;; each step is either a <string>
;; or a <pair> whose car and cdr are both <string>s
;;
;; e.g.,
;;	foo/bar/baz ==>  ("foo" "bar" "baz")
;;      foo/bar/baz^1 => ("foo" "bar" ("baz" . "1"))
;;      foo/bar^2/baz^1 => ("foo" ("bar" . "2") ("baz" . "1"))

(define $version-delim #\^)


(define-class <fs-path> (<object>) :abstract
  (steps type: <list>))

(define-class <fs-relative-path> (<fs-path>))
(define-class <fs-absolute-path> (<fs-path>))

(define $root-path (make <fs-absolute-path>
			 steps: '()))

(define $null-path (make <fs-relative-path>
			 steps: '()))

(define (absolute-path? (p <fs-path>))
   (instance? p <fs-absolute-path>))

(define (fs-tail-path (self <fs-path>))
  (last (steps self)))

(define-method fs-parent-path ((self <fs-relative-path>))
  (if (pair? (steps self))
      (make <fs-relative-path>
            steps: (reverse! (cdr (reverse (steps self)))))
      #f))

(define-method fs-parent-path ((self <fs-absolute-path>))
  (if (pair? (steps self))
      (make <fs-absolute-path>
            steps: (reverse! (cdr (reverse (steps self)))))
      #f))

(define (fs-append-path (p1 <fs-path>) (p2 <fs-path>))
   (if (absolute-path? p2)
       p2
       (let ((s (append (steps p1) (steps p2))))
	(make (object-class p1)
	      steps: s))))

(define (fs-path->string (self <fs-path>))
    (let ((s (steps self)))
	(if (null? s)
	    (if (absolute-path? self)
		"/"
		".")
	    (let ((s (map (lambda (x)
	    		    (if (pair? x)
			        (string-append (car x) 
					       (string $version-delim) 
					       (cdr x))
				x))
		          s)))
		(string-join #\/ (if (absolute-path? self) (cons "" s) s))))))

(define-method write-object ((self <fs-path>) port)
   (format port "#[<fs-path> ~a]" (fs-path->string self)))

(define-method to-string ((self <fs-path>))
  (fs-path->string self))

;;

(define (steps->fs-path (steps <list>) #optional abs?)
  (if abs?
      (make <fs-absolute-path>
            steps: steps)
      (make <fs-relative-path>
            steps: steps)))


(define (string->fs-path (str <string>))
   (if (string=? str "/")
       $root-path
	(let* ((s (string-split str #\/))
		(abs? (string=? (car s) "")))
	(make (if abs? 
		    <fs-absolute-path>
		    <fs-relative-path>)
	    steps: (map (lambda ((i <string>))
			    (let ((x (string-search i $version-delim)))
				(if x
				    (cons (substring i 0 x) (substring i (+ x 1)))
				    i)))
			(if abs? (cdr s) s))))))

;;;  Allow the script programmer to write:  #,(path "/foo/bar")

(define-reader-ctor 'path string->fs-path)

;;
;; rest is a list of unprocessed path components, sharing structure
;; with the steps in the req-path
;; returns two values:  the remaining part as a relative path,
;;                      the visited part as a relative path
;; the first element in "rest" is the part in error, and will
;; be the LAST element in the visited part
;;  e.g.,  req-path := foo/bar/baz/quux
;;             rest := baz/quux
;; then
;;         visited = foo/bar/baz
;;         remains = quux

(define (rel-path-toq (req-path <fs-relative-path>) (rest <list>))
   (values
     (make <fs-relative-path>
           steps: (reverse (list-tail (reverse (steps req-path)) 
	   		              (- (length rest) 1))))
     (make <fs-relative-path>
           steps: (cdr rest))))


;;;

