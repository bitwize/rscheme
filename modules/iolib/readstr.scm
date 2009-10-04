
;;; 
;;;  A condition to be signalled if a fixed-length read does not
;;;  read the requested number of characters
;;;
;;;  The restart protocol allows the handler to return the
;;;  value(s) that should be returned from `read-string'.
;;;  The returned string is not checked for correctness (ie,
;;;  to be the correct length); it is up to the application
;;;  (signal handler or read-string caller) to make sure that
;;;  returning a different-length string makes sense.
;;;
;;;  The port is left at the end of the stream; normally, this
;;;  is the EOF position, but a few ports are known to "come back
;;;  from the dead" and more stuff may be available thereafter

(define-class <partial-read> (<condition>)
  (reading-from-port type: <input-port>)
  (target-length type: <integer>)
  (partially-read type: <string>))

(define-method display-object ((self <partial-read>) port)
  (format port "read-string: on ~s,\n  only read ~s of ~s: ~#*@50s\n"
	  (reading-from-port self)
	  (string-length (partially-read self))
	  (target-length self)
	  (partially-read self)))

;;;
;;;  generic implementations for the base class, <input-port>
;;;  (broken out into separate procedures so we can call them
;;;  from elsewhere, like from `input-pipe-port')
;;;

(define (fallback-read-len (self <input-port>) (len <fixnum>))
  (let ((str (make-string len)))
    (let loop ((i 0))
      (if (< i len)
	  (let ((ch (input-port-read-char self)))
	    (if (char? ch)
		(begin
		  (string-set! str i ch)
		  (loop (+ i 1)))
		(signal
		 (make <partial-read>
		   reading-from-port: self
		   target-length: len
		   partially-read: (substring str 0 i)))))
	  str))))

(define (fallback-read-rest (self <input-port>))
  (let ((out (open-output-string)))
    (let loop ((any? #f))
      (if (input-port-char-ready? self)
	  (let ((ch (input-port-read-char self)))
	    (if (eof-object? ch)
		(if any?
		    (close-output-port out)
		    $eof-object)
		(begin
		  (write-char ch out)
		  (loop #t))))
	  (close-output-port out)))))

;;;
;;;  The protocol for `input-port-read-len' requires
;;;  that a string of length exactly `len' be returned.
;;;  If the request could not be satisfied, a <partial-read>
;;;  condition should be signalled (see above for a description
;;;  of the <partial-read> restart protocol)
;;;

(define-method input-port-read-len ((self <input-port>) (len <fixnum>))
  (fallback-read-len self len))

;;;
;;;  The protocol for `input-port-read-rest' requires
;;;  that it return EOF instead of indefinitely returning
;;;  empty strings.  So far, TWO methods have been fixed
;;;  because their naive implementation that returned empty strings
;;;  forever at EOF, causing `port->string' to loop.
;;;
;;;  `input-port-read-rest' in general should return as large
;;;  a chunk of input as convenient for the input port class
;;; 
;;;  It is acceptable to return a bounded number of empty
;;;  strings, although that is not usually useful
;;;

(define-method input-port-read-rest ((self <input-port>))
  (fallback-read-rest self))

;;;  -------------------------------------------------------
;;;  better implementations for some standard input ports...
;;;  -------------------------------------------------------

;;;
;;;  <buffered-input-port>
;;;

(define-method input-port-read-rest ((self <buffered-input-port>))
  (let (((m <fixnum>) (string-length (buffered-input-buffer self)))
        ((i <fixnum>) (buffered-input-posn self)))
    (if (< i m)
        (let ((save (if (= i 0)
                        (buffered-input-buffer self)
                        (substring (buffered-input-buffer self) i))))
          (set-buffered-input-buffer! self "")
          (set-buffered-input-posn! self 0)
          save)
        (let ((more (provide-more-input self)))
          (if (string? more)
              more
              $eof-object)))))

(define-method input-port-read-max ((self <buffered-input-port>) len)
  (let* ((buf (buffered-input-buffer self))
         (i (buffered-input-posn self))
         (n (- (string-length buf) i)))
    (cond
     ((= n 0)
      ;; nothing is available... get another chunk of input and
      ;; return all or some of it
      (let ((more (provide-more-input self)))
        (if (string? more)
            (if (<= (string-length more) len)
                more
                (begin
                  (set-buffered-input-buffer! self more)
                  (set-buffered-input-posn! self len)
                  (substring more 0 len)))
            $eof-object)))
     ((>= len n)
      ;; the max amount we can return is more than what's available; return it all
      (let ((save (if (= i 0)
                      buf
                      (substring buf i))))
        (set-buffered-input-buffer! self "")
        (set-buffered-input-posn! self 0)
        save))
     (else
      ;; the amount we can return is less than what's available; take some of it
      (set-buffered-input-posn! self (+ i len))
      (substring buf i (+ i len))))))
  
  
(define-method input-port-read-len ((self <buffered-input-port>) len)
  (let* (((len <fixnum>) (+ len 0))
         (out (bvec-alloc <string> (+ 1 len))))
    (let loop ((j 0))
      (let* ((i (buffered-input-posn self))
	     (buf (buffered-input-buffer self))
	     (n (- (string-length buf) i)))
	;; if the amount we need, `(- len j)', is less than
	;; the amount that's available, `n', then we can do
	;; one last copy and get outta here       
	(if (<= (- len j) n)
	    (let ((need (- len j)))
	      (set-buffered-input-posn! self (+ i need))
	      (bvec-copy out j buf i need)
	      out)
	    ;; otherwise,
	    ;; copy as much as we can and get some more
	    (begin
	      (bvec-copy out j buf i n)
	      ;; note that we advance the pointer past what we just
	      ;; read, on the off chance that `provide-more-input'
	      ;; will do something strange and come back and look at
	      ;; this port and be startled to see stuff still in the
	      ;; buffer that was allegedly read (after all, why would
	      ;; the provide-more procedure have been called if there
	      ;; was still stuff in the buffer!?).  just to provide
	      ;; some cleaner semantics...
	      (set-buffered-input-posn! self (string-length buf))
	      ;; read some more, if possible
	      (let ((more (provide-more-input self)))
		(if (string? more)
		    (begin
		      (set-buffered-input-buffer! self more)
		      (set-buffered-input-posn! self 0)
		      (loop (+ j n)))
		    ;; no more was available... error!
		    (signal
		     (make <partial-read>
			   reading-from-port: self
			   target-length: len
			   partially-read: (substring out 0 (+ j n))))))))))))

;;; NOTE!! refactor the classes so that <buffered-input-port>
;;; is above <string-input-port>, and <string-input-port> is final
;;;
;;;  <string-input-port>
;;;

(define-method input-port-read-rest ((self <string-input-port>))
  (let ((i (buffered-input-posn self))
	(buf (buffered-input-buffer self)))
    (if (= i (string-length buf))
        $eof-object
        (begin
          (set-buffered-input-posn! self (string-length buf))
          (substring buf i)))))

(define-method input-port-read-len ((self <string-input-port>) len)
  (let ((i (buffered-input-posn self))
	(buf (buffered-input-buffer self)))
    (if (<= (+ i len) (string-length buf))
	(begin
	  (set-buffered-input-posn! self (+ i len))
	  (substring buf i (+ i len)))
	(begin
	  (set-buffered-input-posn! self (string-length buf))
	  (signal (make <partial-read>
			reading-from-port: self
			target-length: len
			partially-read: (substring buf i)))))))

;;;
;;;  <input-pipe-port>
;;;

(define-method input-port-read-rest ((self <input-pipe-port>))
  (fallback-read-rest self))

(define-method input-port-read-len ((self <input-pipe-port>) len)
  (fallback-read-len self len))

;;;
;;;  <std-input-port>
;;;

(define-method input-port-read-len ((self <std-input-port>) n)
  (let* ((str (bvec-alloc <string> (+ n 1)))
	 (m (fread-fill (file-stream self) str 0 n)))
    (if (= m n)
	str
	(signal (make <partial-read>
		      reading-from-port: self
		      target-length: n
		      partially-read: (substring str 0 m))))))

(define-method input-port-read-rest ((self <std-input-port>))
  (let* ((f (file-stream self))
	 (at (ftell f))
	 (end (begin (fseek f 0 2)
		     (ftell f)))
	 (n (- end at))
	 (str (bvec-alloc <string> (+ n 1))))
    (fseek f at 0)
    (let ((m (fread-fill f str 0 n)))
      (if (zero? m) ; could have been expecting 0 (n=0), or really got 0
	  $eof-object
	  (if (= m n)
	      str
	      (substring str 0 m))))))


;;;  -------------------------------------------------------
;;;			    user-interface
;;;  -------------------------------------------------------

(define-inline read-string
  (nlambda
   (() (input-port-read-rest (current-input-port)))
   ((port) (input-port-read-rest port))
   ((port len) (input-port-read-len port len))))

;;;
;;;   a convenience function based on the above protocol
;;;

(define (port->string port)
  ;; special case to not create/collapse an output port
  ;; if everything gets slurped in the first gulp
  (let ((first (read-string port)))
    (if (eof-object? first)
        ""
        (let ((next (read-string port)))
          (if (eof-object? next)
              first
              (let ((out (open-output-string)))
                (write-string out first)
                (write-string out next)
                (let loop ()
                  (let ((another (read-string port)))
                    (if (eof-object? another)
                        (close-output-port out)
                        (begin
                          (write-string out another)
                          (loop)))))))))))

;;;

;;;  -----------------------------------------------------------------
;;;  An input port that reads only the first N characters
;;;  from some other input port.  Useful for cracking network
;;;  protocols and other known-length-ahead-of-time parsing tricks.

(define-class <fixed-length-input-port> (<buffered-input-port>)
  (remaining-length type: <fixnum>)
  (source-port type: <input-port>))

(define (make-fixed-length-input-port port len)
  (make <fixed-length-input-port>
        remaining-length: len
        source-port: port))

(define-method input-port-read-max ((self <input-port>) maxlen)
  (handler-bind (<partial-read> (lambda (e n)
                                  (let ((s (partially-read e)))
                                    (if (= (string-length s) 0)
                                        $eof-object
                                        s))))
                (read-string self maxlen)))

(define-method provide-more-input ((self <fixed-length-input-port>))
  (if (= (remaining-length self) 0)
      #f
      (let* ((s (input-port-read-max (source-port self)
                                     (remaining-length self))))
        (set-remaining-length! self (- (remaining-length self) 
                                       (string-length s)))
        (if (string? s)
            s
            #f))))

(define-method more-input-ready? ((self <fixed-length-input-port>))
  (or (= (remaining-length self) 0) ; always willing to return EOF
      (input-port-char-ready? (source-port self))))


