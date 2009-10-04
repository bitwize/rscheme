;;
;; block-aligned output...
;;

(define-class <blocked-output-port> (<output-port>)
   underlying-port
   (bytes-written type: <fixnum> init-value: 0)
   (block-size type: <fixnum>))

(define (open-blocked-port port block-size)
   (make <blocked-output-port>
         underlying-port: port
	 block-size: block-size))

(define-method write-string ((self <blocked-output-port>) str)
    (write-string (underlying-port self) str)
    (set-bytes-written! self (+ (bytes-written self) (string-length str))))

(define-method flush-to-blocks ((self <output-port>) num-blocks))

(define-method flush-to-blocks ((self <blocked-output-port>) num-blocks)
  (let ((n (remainder (bytes-written self) (* num-blocks (block-size self)))))
    (if (not (eq? n 0))
	(let ((m (- (* num-blocks (block-size self)) n)))
	  (write-string (underlying-port self)
			(make-string m #\nul))
	  (set-bytes-written! self (+ m (bytes-written self)))))))


;;


(define (make-tar-header (path <string>)
			 (mode <fixnum>)
			 (uid <fixnum>)
			 (gid <fixnum>)
			 (size <fixnum>)
			 (mtime <time>)
			 linkflag
			 linkname)
      (let* ((str1 (string-append path
				(make-string (- 100 (string-length path)) #\nul)
				(format #f "~-6o \0" mode)
				(format #f "~-6o \0" uid)
				(format #f "~-6o \0" gid)
				(format #f "~-11o " size)
				(format #f "~-11o " 
				 (raw-int-64->integer
				  (raw-int-64+
				   0
				   (bvec-read-signed-32 mtime 0))))))
	    (str2 (string-append (if linkflag
				     (string linkflag)
				     "\0")
				 (if linkname
				     linkname
				     "")))
	    (cksum (+ (apply + (map char->integer (string->list str1)))
	    	      (apply + (map char->integer (string->list str2)))
		      256))) ;; the checksum for 7 blanks (the checksum field)
	(string-append str1
		       (format #f "~06o\0 " cksum)
		       str2
		       (make-string (- 512
		       		       (string-length str1)
				       (string-length str2)
				       8)
				    #\nul))))

(define (write-one-tar-file port
			(path <string>)
			(mode <fixnum>)
			(uid <fixnum>)
			(gid <fixnum>)
			(mtime <time>)
			(content <string>))
    (write-string port
    		  (make-tar-header path 
		  		   mode 
				   uid 
				   gid 
				   (string-length content)
				   mtime
				   #f ; #\space
				   #f))
    (write-string port content)
    (flush-to-blocks port 1))

#|
(define (test)
   (call-with-output-file
     "test.tar"
     (lambda (port)
       (let ((p (open-blocked-port port 512)))
          (write-one-tar-file p 
       		       "test.foo" 
		       #o771 
		       123 
		       101 
		       (time) 
		       (file->string "util/tar.scm"))
	(flush-to-blocks p 20)))))

;	 (write-string port (make-string (* 512 (- 20 (remainder n 20))) #\nul))))))
|#
