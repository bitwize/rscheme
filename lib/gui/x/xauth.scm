
(define (initial-message auth)
  (let* ((apn (if auth (car auth) ""))
	 (apd (if auth (cadr auth) ""))
	 (apn-l (string-length apn))
	 (apd-l (string-length apd)))
    (string-append
     (make-buffer u1: (if (eq? (xbo-endianess) 'little-endian)
			  108 ;; #\l
			  66) ;; #\B
		  u1: 0     ;; unused
		  u2: 11    ;; protocol-major-version
		  u2: 0     ;; protocol-minor-version
		  u2: apn-l ;; authorization-protocol-name length
		  u2: apd-l ;; authorization-protocol-data length
		  u2: 0)    ;; unused
     apn
     (pad4str apn-l)
     apd
     (pad4str apd-l))))

(define (get-xauth dpy)
  (handler-case
   (let* ((p (open-input-process (string-append "xauth extract - " dpy)))
	  (s (port->string p)))
     (close-input-port p)
     (bind ((fam addr num name data (parse-x-authority s)))
       (dm 101 "X authorization for ~a: ~a" dpy name)
       (list name data)))
   ((<condition> condition: c)
    (dm 102 "X authorization for ~a: <none>" dpy)
    #f)))
#|
(define (port->string (port <input-port>))
  (call-with-output-string
   (lambda (out)
     (let loop ()
       (let ((ch (input-port-read-char port)))
	 (if (not (eof-object? ch))
	     (begin
	       (output-port-write-char out ch)
	       (loop))))))))
|#

(define (nbo-read-u2 str i)
  (+ (* (bvec-ref str i) 256) (bvec-ref str (+ i 1))))

(define (parse-x-authority str)
  (let ((family (nbo-read-u2 str 0))
	(i 2))
    (define (read-len-str)
      (let* ((len (nbo-read-u2 str i))
	     (str (substring str (+ i 2) (+ i 2 len))))
	(set! i (+ i len 2))
	str))
    (let* ((address (read-len-str))
	   (number (read-len-str))
	   (name (read-len-str))
	   (data (read-len-str)))
      (values family address number name data))))
