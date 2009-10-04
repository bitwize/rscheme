
,(use syscalls)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable Naming Conventions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; bs  : byte string
;; dn  : domain name
;; m/v : message as a vector
;; m/s : message as a byte string

;; Lisp tradition involves the use of descriptive identifiers for
;; functions, variables, parameters, etc. even if they are quite
;; long. However, keeping code concise has it's benefits. So as a
;; compromise, the general convention I've used is to keep function
;; names verbose and function parameter names short.

(define-macro (defmacro . args)
  `(define-macro ,@args))

(defmacro (def . args)
  `(define ,@args))

(def lref list-ref)
(def sref string-ref)
(def vref vector-ref)

(define-method len ((self <list>))   (length self))
(define-method len ((self <string>)) (string-length self))
(define-method len ((self <vector>)) (vector-length self))

(define-method remove ((item <string>) (list <list>))
  (cond ((null? list)
	 '())
	((string=? item (car list))
	 (cdr list))
	(else
	 (cons (car list)
	       (remove item (cdr list))))))

(defmacro (fn . args)
  `(lambda ,@args))

(def (rassq obj alist)
  (let ((t (filter (fn (entry)
		     (eq? obj (cdr entry)))
		   alist)))
    (if (null? t)
	#f
	(car t))))

;; f : Predicate
;; l : List

(def filter
  (letrec ((filter-acc (fn (f l a)
			 (cond ((null? l)
				a)
			       ((f (car l))
				(filter-acc f
					    (cdr l)
					    (append a (list (car l)))))
			       (else
				(filter-acc f
					    (cdr l)
					    a))))))
    (fn (f l)
      (filter-acc f l '()))))

(defmacro (define-enum-table name table)
  (let ((table-name (symbol-append '*
				   name
				   (string->symbol "-table*")))
	(to-num (symbol-append name (string->symbol "->num")))
	(from-num (symbol-append 'num-> name)))
    `(begin
       (define ,table-name ,table)
       (define (,to-num sym)
	 (cdr (assq sym ,table-name)))
       (define (,from-num num)
	 (car (rassq num ,table-name))))))
       
;; (defmacro (define-enum-table name table)
;;   (let ((table-name (string->symbol
;; 		     (string-append "*"
;; 				    (symbol->string name)
;; 				    "-table*")))
;; 	(-> (symbol-append name (string->symbol "->num")))
;; 	(<- 'num-> name))
;;     `(begin
;;        (define ,table-name ,table)
       
;;        (define (,-> sym)
;; 	 (cdr (assq sym ,table-name)))
       
;;        (define (,<- num)
;; 	 (car (rassq num ,table-name))))))

(def (string-downcase s)
  (list->string
   (map char-downcase (string->list s))))

(def pr format)
	       
(def (current-seconds)
  (time->epoch-seconds (time)))

(def (false? o)
  (eq? #f o))

(def (bool->num b)
  (if b
      1
      0))

(def (num->bool n)
  (if (= 0 n)
      #f
      #t))

(def (arithmetic-shift n i)
  (if (>= i 0)
      (logical-shift-left n i)
      (logical-shift-right n (- i))))

(def (list->number l)
  (if (null? (cdr l))
      (car l)
      (+ (arithmetic-shift (car l)
			   (* 8 (- (length l) 1)))
	 (list->number (cdr l)))))

(define bit-mask
  (lambda (length)
    (- (expt 2 length) 1)))

(def (find p l)
  (cond ((null? l)
	 #f)
	((p (car l))
	 (car l))
	(else
	 (find p (cdr l)))))

(def (write-object-to-string o)
  (let ((p (open-output-string)))
    (write-object o p)
    (close-output-port p)))

(def (list-ref-random l)
  (list-ref l (random (length l))))

(def (not-null? l)
  (not (null? l)))

(define-syntax (if-debug . body)
  ;(begin . body)
  (values))

(define-syntax (dbg fmt . x)
  (if-debug (format #t fmt . x)))
