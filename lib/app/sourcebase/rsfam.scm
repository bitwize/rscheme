(load "/u/rscheme/rsfam/legacy.scm")

(define (rsfam)
  (set! *do-auto-commit* #f)
  (database-connect "/tmp/foo1" 'read))

;;
;; return a procedure (<fs-absolute-path> <version>) => <list>
;; which returns a list of extra keys
;;

(define dash-num-name (reg-expr->proc '(suffix 
					(seq #\- 
					     (let d (+ (or #\. digit)))))))
(define dot-num-name (reg-expr->proc '(suffix (seq #\. 
						   (let d (+ digit))))))

(define (release-num (fs-name <string>))
  (bind ((s1 e1 n1 (dash-num-name fs-name))
	 (s2 e2 n2 (dot-num-name fs-name)))
    (if s1
	(string-append "v" n1)
	(string-append "r" n2))))

(define (build-name (snap <snapshot>))
  (let ((snap-name (name snap))
	(fs-name (name (versioned-object snap))))
    (if (or (string>? fs-name "rs-0.7")
	    (and (string=? fs-name "rs-0.7")
		 (string>=? snap-name "1.0")))
	(let ((vp (map string->number (string-split snap-name #\.))))
	  (if (eq? (cadr vp) 0)
	      (format #f "~a.~d" (release-num fs-name) (car vp))
	      (format #f "~a.~d.~04d" 
		      (release-num fs-name) 
		      (car vp)
		      (cadr vp))))
	(format #f "~a-~a" (release-num fs-name) snap-name))))

(define (parse-explicit-keys info)
  (map (lambda (i)
	 (if (and (> (string-length i) 2)
		  (char=? (string-ref i 1) #\=))
	     (cons (string-ref i 0) (substring i 2))
	     (error "invalid --def: ~a" i)))
       info))

(define (extra-keys-proc (fs <file-space>) req)
  (bind ((c (assq 'committed (if (instance? fs <snapshot>)
				 (properties fs)
				 '())))
	 (gks (if c
		  (list (cons #\b
			      (format #f "~a, ~a"
				      (build-name fs)
				      (time->string (timestamp (cdr c))
						    "%Y-%m-%d"))))
		  '()))
	 (expl (if (assq 'def req)
		   (append (parse-explicit-keys (cdr (assq 'def req))) gks)
		   gks)))
    (lambda (p v)
      expl)))

;; be a daemon

(define (sigint)
  (flush-output-port (current-output-port))
  (format (current-error-port) "*** Going down on SIGINT\n")
  (flush-output-port (current-error-port))
  (process-exit 1))

,(use start)

(define (bd . opts)
  ;;
  (register-c-signal-handler! 'sigint sigint)
  (handler-case
   (apply server-daemon opts)
   ((<condition> condition: c)
    (format (current-error-port) "ERROR: ~a" c)
    (process-exit 1))))
