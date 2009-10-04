,(use tables)
,(use rs.util.properties)

;;;

(define-method properties ((self <function>))
  (properties (template self)))

(define-method properties ((self <template>))
  (gvec-ref self 2))

(define-method set-properties! ((self <function>) p)
  (set-properties! (template self) p))

(define-method set-properties! ((self <template>) p)
  (gvec-set! self 2 p)
  p)

;;;

(define-class <rule> (<object>)
  (name type: <symbol>)
  (action type: <function>)
  (rhs type: <vector>)
  (hash-value type: <fixnum> init-function: random))

(define (add-rule! grammar nt act #rest rhs)
  (table-insert! grammar 
		 nt
		 (append (or (table-lookup grammar nt) '())
			 (list (make <rule>
				     name: nt
				     action: act
				     rhs: (list->vector rhs))))))


(define-class <tuple> (<object>)
  (rule type: <rule>)
  (dot type: <fixnum>)
  (back-pointer type: <fixnum>)
  (accum type: <list> init-value: '()))

(define (write-item port (r <rule>) (dot <fixnum>))
  (format port "~a ->" (name r))
  (let loop ((i 0))
    (if (= i dot)
	(format port " ."))
    (if (< i (vector-length (rhs r)))
	(let ((x (vector-ref (rhs r) i)))
	  (format port " ~a" 
		  (if (symbol? x)
		      x
		      (get-property x 'matches "[]")))
	  (loop (+ i 1))))))

(define-method write-object ((self <tuple>) port)
  (format port "#[<tuple> ")
  (write-item port (rule self) (dot self))
  (format port " ,~d <~j>]" (back-pointer self) (reverse (accum self))))

(define-method hash-value ((self <tuple>))
  (+ (hash-value (rule self))
     (* 77 (dot self))
     (* 71 (back-pointer self))))

(define (tuple=? (a <tuple>) (b <tuple>))
  (and (eq? (rule a) (rule b))
       (eq? (dot a) (dot b))
       (eq? (back-pointer a) (back-pointer b))))
  
(define (make-tuple-table)
  (make-table tuple=? hash-value))

;;

(define-class <parse-machine> (<object>)
  (input-posns type: <vector>)
  (input-string type: <vector>)
  (grammar type: <symbol-table>))

(define-class <input-posn> (<object>)
  (index type: <fixnum>)
  (next-posn init-value: #f)
  (queue init-value: '())
  (tuple-table init-function: make-tuple-table))

;;
(define (make-parse-machine (input <vector>) 
			    (grammar <symbol-table>)
			    (start <symbol>))
  (let ((v (make-vector (+ 1 (vector-length input)))))
    ;
    (let ((k (vector-length input)))
      (vector-set! v k
		   (make <input-posn>
			 index: k)))
    ;
    (for-each 
     (lambda (k)
       (vector-set! v k (make <input-posn>
			      index: k
			      next-posn: (vector-ref v (+ k 1)))))
     (reverse (range (vector-length input))))
    ;
    (let ((boot (first-tuple (vector-ref v 0) 
			     (make <rule>
				   name: '<start>
				   action: identity
				   rhs: (vector start)))))
      ;
      (set-queue! (vector-ref v 0) (list boot))
      (table-insert! (tuple-table (vector-ref v 0)) boot boot))
    ;
    (make <parse-machine>
	  input-string: input
	  grammar: grammar
	  input-posns: v)))


;;

(define (first-tuple (p <input-posn>) (r <rule>))
  (make <tuple>
	rule: r
	dot: 0
	back-pointer: (index p)
	accum: '()))

(define (next-tuple (t <tuple>) meaning)
  (make <tuple>
	rule: (rule t)
	dot: (+ 1 (dot t))
	back-pointer: (back-pointer t)
	accum: (cons meaning (accum t))))

;;

(define (close-posn (m <parse-machine>) (p <input-posn>))
  ;(format #t "---- close[~d] ---\n" (index p))
  (let loop ()
    (if (pair? (queue p))
	(let ((t (car (queue p))))
	  (set-queue! p (cdr (queue p)))
	  (advance-and-add m p t)
	  (loop))
	m)))

(define (add-tuple (m <parse-machine>) (p <input-posn>) (t <tuple>))
  (if (not (table-lookup (tuple-table p) t))
      (begin
	(table-insert! (tuple-table p) t t)
	(set-queue! p (cons t (queue p))))))

(define (do-completion (m <parse-machine>) (p <input-posn>) (t <tuple>))
  (let ((meaning (apply (action (rule t)) (reverse (accum t)))))
    (for-each
     (lambda (bt)
       ;(format #t "   (check ~s)\n" bt)
       (if (did-predict? bt t)
	   (add-tuple m p (next-tuple bt meaning))))
     (value-sequence 
      (tuple-table (vector-ref (input-posns m) (back-pointer t)))))))

(define (do-prediction (m <parse-machine>) (p <input-posn>) (t <tuple>) nt)
  (for-each
   (lambda (r)
     (add-tuple m p (first-tuple p r)))
   (table-lookup (grammar m) nt)))
  
(define (do-scan (m <parse-machine>) (p <input-posn>) (t <tuple>) f)
  (if (next-posn p)
      (bind ((tok (vector-ref (input-string m) (index p)))
	     (meaning ok? (f tok)))
	(if ok?
	    (add-tuple m (next-posn p) (next-tuple t meaning))))))
	   
(define (get-next (t <tuple>))
  (vector-ref (rhs (rule t)) (dot t)))

(define (has-more? (t <tuple>))
  (< (dot t) (vector-length (rhs (rule t)))))

(define (did-predict? (old-tuple <tuple>) (current-tuple <tuple>))
  (and (has-more? old-tuple)
       (eq? (get-next old-tuple) (name (rule current-tuple)))))

(define (advance-and-add (m <parse-machine>) (p <input-posn>) (t <tuple>))
  ;(format #t "advance-and-add: ~s\n" t)
  (if (has-more? t)
      (let ((n (get-next t)))
	(if (symbol? n)
	    (do-prediction m p t n)
	    (do-scan m p t n)))
      (do-completion m p t)))

;;;

(define (find-answer (m <parse-machine>))
  (let loop ((final (key-sequence (tuple-table 
				   (vector-ref 
				    (input-posns m) 
				    (vector-length (input-string m)))))))
    (if (null? final)
	(values)
	(if (eq? (name (rule (car final))) '<start>)
	    (car (accum (car final)))
	    (loop (cdr final))))))

(define (parse input grammar start)
  (let ((m (make-parse-machine input grammar start)))
    (run-machine m)
    (find-answer m)))

(define (run-machine (m <parse-machine>))
  (vector-for-each
   (lambda (p)
     (close-posn m p))
   (input-posns m)))
