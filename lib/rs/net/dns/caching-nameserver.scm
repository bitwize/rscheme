
,(use rs.sys.threads.manager)

(def *server-port* 53)
;(def *server-port* 5353)

(define (start-dns-server #key (port default: *server-port*))
  (let ((s (open-server-socket port)))
    (thread-resume
     (make-thread
      (lambda ()
        (tcp-listen-loop s))
      "dns/tcp")))
  (listen-loop (open-udp-socket port)))

(define (tcp-listen-loop sock)
  (let loop ()
    (let ((a (accept-client sock)))
      (thread-resume
       (make-thread
        (lambda ()
          (handler-case
           (handle-request-stream a)
           ((<condition> condition: c)
            (dbg "stream cnx failed: ~a" c)
            (close a))))
        "handle-request-stream"))
      (loop))))

(def (listen-loop sock)
  (bind ((buffer peer (receive-packet sock)))

    ;; DEBUG
    (dbg "~%")
    (dbg "Received packet...~%")
    
    (thread-resume
     (make-thread (fn () (handle-request-packet buffer peer sock))
                  "handle-request"))
    ;;
    (listen-loop sock)))

(define (handle-request-stream sock)
  (let loop ()
    (let* ((lbuf (read-string (input-port sock) 2))
           (len (+ (<< (bvec-ref lbuf 0) 8)
                   (bvec-ref lbuf 1))))
      (dbg "<< TCP: [~d bytes]\n" len)
      (let ((buffer (read-string (input-port sock) len)))
        (handle-request 
         buffer
         (lambda ((reply <string>))
           (let ((n (string-length reply)))
             (dbg ">> REPLY [~d bytes]\n" n)
             ;;
             (bvec-set! lbuf 0 (logical-shift-right n 8))
             (bvec-set! lbuf 1 (bitwise-and n #xFF))
             ;;
             (write-string (output-port sock) lbuf)
             (write-string (output-port sock) reply)
             (flush-output-port (output-port sock)))))
        (loop)))))

(define (handle-request-packet buffer peer sock)
  (handle-request
   buffer
   (lambda (reply)
     (send-packet sock reply peer))))
   
(define *last-request* #f)

(define (zone-reply (zone <zone>) msg reply)
  (set-answer-section! msg (vector->list (zdata zone)))
  (set-rcode! msg 'NO-ERROR)
  (reply (msg->bs msg)))

(def (handle-request buffer reply)
  (set! *last-request* buffer)
  (let* ((msg   (parse-message buffer))
	 (query (car (question-section msg))))

    ;; DEBUG
    (dbg "~%")
    (dbg "Inside handle-request...~%")
    (dbg "Received query: ~a~%" (write-object-to-string query))
    (if-debug
     (print msg)
     (print query))
    ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; The varied possibilities:

    ;; Matches NXDOMAIN in the cache

    ;; Answers are in the cache

    ;; Matches a CNAME record in the cache and we have the real answers

    ;; Answers received from Internet nameservers

    ;; NXDOMAIN result received from Internet nameservers

    ;; No answer found
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; Options common to all possibilities:
    
    (set-qr! msg #t)
    (set-aa! msg #f)
    (set-tc! msg #f)
    (set-ra! msg #t)

    (cond ((eq? 'NX (cache-get query))
    
	   ;; DEBUG
	   (dbg "Query matches an NXDOMAIN entry in cache~%")

	   (set-rcode! msg 'NAME-ERROR)
	   (set-authority-section! msg '())
	   (set-additional-section! msg '())

	   (reply (msg->bs msg)))

	  ((answers-from-cache query) =>
	   (fn (answers)

	     ;; DEBUG
	     (dbg "Sending answers from cache~%")
             (print answers)
             (if (instance? answers <zone>)
                 (zone-reply answers msg reply)
                 (begin
                   (set-rcode! msg 'NO-ERROR)
                   (set-answer-section! msg answers)

                   (reply (msg->bs msg))))))

	  (else ;; Try Internet nameservers

	   ;; DEBUG
	   (dbg "Trying Internet nameservers...~%")

	   (let* ((nameservers (find-closest-nameservers (name query)))
		  (results     (answers-from-servers nameservers query)))

	     (cond ((not (message? results))

		    ;; DEBUG
		    (dbg "Sending answer from Internet servers...~%")

		    ;; Not a message, so we got an answer.
		    
		    (set-rcode! msg 'NO-ERROR)
		    (set-answer-section!     msg results)
		    (set-authority-section!  msg '())
		    (set-additional-section! msg '())

		    (reply (msg->bs msg)))

		   ((message-nxdomain? results)

		    (dbg "Received NXDOMAIN response...~%")
		    
		    (set-rcode! msg 'NAME-ERROR)
		    (set-answer-section! msg '())
		    (set-authority-section! msg '())
		    (set-additional-section! msg '())
		    
		    (reply (msg->bs msg)))
		   
		   (else ;; No answer - weird. Just return...

		    (dbg "No records match query...~%")

		    (set-rcode! msg 'NO-ERROR)
		    (set-answer-section! msg '())
		    (set-authority-section! msg '())
		    (set-additional-section! msg '())

		    (reply (msg->bs msg)))))))))

;; (def (answers-from-cache q)
  
;;   (cond ((cache-get q) => identity)

;; 	((cache-get (make-query (name q) 'CNAME 'IN)) =>
;; 	 (fn (rrl)
;; 	   (let* ((canonical-name (rdata (car rrl)))
;; 		  (canonical-rrl  (cache-get (make-query canonical-name
;; 							 (type q)
;; 							 (class q)))))
;; 	     (if canonical-rrl
;; 		 (append rrl canonical-rrl)
;; 		 #f))))

;; 	(else #f)))

(def (answers-from-cache q)
  
  (cond ((cache-get q) => identity)

	((cache-get (make-query (name q) 'CNAME 'IN)) =>
	 (fn (rrl)
	   (let* ((canonical-name (rdata (car rrl)))
		  (canonical-rrl (answers-from-cache (make-query canonical-name
								 (type q)
								 (class q)))))
	     (if canonical-rrl
		 (append rrl canonical-rrl)
		 #f))))

	(else #f)))

(def (answers-from-servers srvl q)

  (let* ((msg             (recursive-query srvl q))
	 (direct-answer   (rr-find q (answer-section msg)))
	 (cname-record    (rr-find (make-query (name q) 'CNAME (class q))
				   (answer-section msg)))
	 (canonical-name  (if cname-record (rdata cname-record) #f))
	 (indirect-answer (if canonical-name
			      (rr-find (make-query canonical-name
						   (type q)
						   (class q))
				       (answer-section msg))
			      #f)))

    (cond (direct-answer   (answer-section msg))
	  (indirect-answer (answer-section msg))
	  (cname-record    (let ((dig-deeper
				  (answers-from-servers
				   (find-closest-nameservers canonical-name)
				   (make-query canonical-name
					       (type q)
					       (class q)))))
			     (if (message? dig-deeper)
				 dig-deeper
				 (cons cname-record dig-deeper))))
	  (else msg))))

;; Example of *dns-domain-specific-servers*
;;
;; (define *dns-domain-specific-servers*
;;   '(("dev.foobolics.com."       . ("10.0.0.1" "10.0.0.2"))
;;     ("marketing.foobolics.com." . ("10.0.1.1" "10.0.1.2"))
;;     ("research.foobolics.com."  . ("10.0.2.1" "10.0.2.2"))))

(def (name->root-nameservers name)
  (cond ((domain-name-null? name)
	 *root-nameservers*)

	((assoc name *domain-specific-servers*) =>
	 (fn (match) (cdr match)))

	(else
	 (name->root-nameservers (domain-name-cdr name)))))

;; Instead of always starting at the root nameservers, this function
;; should probably be used to find closer nameservers from within our
;; cache...
;;
;; This function takes care of referencing *domain-specific-servers*
;; so it should obsolete most calls to 'name->root-nameservers'.

(def (find-closest-nameservers name)

  (cond ((domain-name-null? name)
	 *root-nameservers*)

	((assoc name *domain-specific-servers*) =>
	 (fn (match) (cdr match)))
	
	((cache-get (make-query name 'NS 'IN)) =>
	 (fn (rrl) (map name->ip (map rdata rrl))))

	(else
	 (find-closest-nameservers (domain-name-cdr name)))))
