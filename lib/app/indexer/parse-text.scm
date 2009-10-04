
(define *no-stop-words* (make-string-table))

(define (string-downcase! (str <string>))
  (let loop (((i <fixnum>) (string-length str)))
    (if (eq? i 0)
	str
	(let (((j <fixnum>) (sub1 i)))
	  (string-set! str j (char-downcase (string-ref str j)))
	  (loop j)))))

(define punct (reg-expr->proc '(or #\' #\-)))

(define (strip-punctuation str)
  (let ((ps (string-split str punct)))
    (if (null? (cdr ps))
        (list str)
        (cons (string-join "" ps) 
              (select (lambda (s)
                        (> (string-length s) 1))
                      ps)))))

(define (matches-ci (str <string>) (proc <function>) 
		    #optional (stopw type: <string-table> 
				     default: *no-stop-words*))
  (let ((q (make-dequeue)))
    (let loop ((i 0))
      (bind ((s e str (proc str i)))
	(if s
	    (begin
              (for-each
               (lambda (w)
                 (string-downcase! w)
                 (if (not (table-lookup stopw w))
                     (dequeue-push-back! q w))
                 (values))
               (strip-punctuation str))
              (loop e))
	    (vector->list (dequeue-state q)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(define *global-stop-words* (make-string-table))

(define (stop-words . words)
  (for-each (lambda (w)
	      (table-insert! *global-stop-words* w #t))
	    words))

(stop-words "the" "it" "if") ;; single-chars already excluded by pattern
(stop-words "of" "be" "in" "to" "is" "we")
(stop-words "re")

;;;

(define word-pat (reg-expr->proc 
		  '(save (seq alpha
			      (+ (or alpha digit #\'))))))

(define (parse-text str)
  (select (lambda (str)
	    (< (string-length str) 40))
	  (matches-ci str word-pat *global-stop-words*)))
