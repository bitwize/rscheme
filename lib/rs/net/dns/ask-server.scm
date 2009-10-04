
(def udp-sock
    (fn ()
      (open-udp-socket)))

(def tcp-sock
  (let ((family (socket-address-family->integer 'address-family/internet))
	(type   (socket-type->integer           'socket-type/stream)))
    (fn ()
      (socket-create family type 0))))

(def make-inet-addr string->inet-addr)

;; q : query
;; s : nameserver

;; (def ask-server/udp
;;   (let ((buf (make-string 1024)))
;;     (fn (q s)
;;       (let ((m/s  (msg->bs (query->message/no-rd q)))
;; 	    (sock (udp-sock))
;; 	    (addr (make-inet-socket-addr (make-inet-addr s) 53)))
;; 	(send-to sock m/s 0 (len m/s) #f addr)
;; 	(recv-from sock buf 0 1024 #f #f <object>)
;; 	(fd-close sock)
;; 	(parse-message buf)))))

;; Non-blocking version of ask-server/udp. The only change is the call
;; to recv-from*, our simple non-blocking version.

(def ask-server/udp
  (fn (q s)
    (let ((m/s  (msg->bs (query->message/no-rd q)))
          (sock (udp-sock))
          (addr (make-inet-socket-addr (make-inet-addr s) 53)))
      (send-packet sock m/s addr)
      (bind ((buf sender (receive-packet sock)))
        (close sock)
        (parse-message buf)))))

(def ask-server/tcp
  (let ((buf (make-string 1024)))
    (fn (q s)
      (let* ((m/s (msg->bs (query->message/no-rd q))) ;; message as string
	     (m/s+p (bs-append (uint16->bs (string-length m/s)) ;; +prefix
			       m/s))
	     (sock (tcp-sock))
	     (addr (make-inet-socket-addr (make-inet-addr s) 53)))
	(socket-connect/inet sock 53 s)
	(fd-write sock m/s+p 0 (string-length m/s+p))
	(fd-read sock buf 0 512)
	buf))))

(def dig
  (fn (name
       #key
       (type 'A)
       (class 'IN)
       (server "127.0.0.1")
       (port 53))

    (let* ((m/s  (msg->bs (query->message (make-query name type class))))
	   (sock (udp-sock))
	   (addr (make-inet-socket-addr (make-inet-addr server) port)))
	   
      (send-packet sock m/s addr)
      (let ((buf (receive-packet sock)))
        (close sock)
        (parse-message buf)))))
	 
;; (def dig
;;   (let ((buf (make-string 1024)))
;;     (fn (q s)
;;       (let ((m/s  (msg->bs (query->message q)))
;; 	    (sock (udp-sock))
;; 	    (addr (make-inet-socket-addr (make-inet-addr s) 53)))
;; 	(send-to sock m/s 0 (len m/s) #f addr)
;; 	(recv-from sock buf 0 1024 #f #f <object>)
;; 	(fd-close sock)
;; 	(parse-message buf)))))

;; (def message-transaction/udp
;;   (let ((buf (make-string 1024)))
;;     (fn (srv msg)
;;       (let ((m/s  (msg->bs msg))
;; 	    (sock (udp-sock))
;; 	    (addr (make-inet-socket-addr (make-inet-addr srv) 53)))
;; 	(send-to    sock m/s 0 (len m/s) #f addr)
;; 	(recv-from* sock buf 0 1024 #f #f <object>)
;; 	(fd-close sock)
;; 	(parse-message buf)))))

;; Moved the buf into the lambda, so that it get's created with each
;; invocation. In the previous version, the buf was common to all
;; invocations. This combined with the multiple threads is
;; problematic...

(def message-transaction/udp
  (fn (srv msg)
    (let ((m/s  (msg->bs msg))
	  (sock (udp-sock))
	  (addr (make-inet-socket-addr (make-inet-addr srv) 53)))
      (dbg ">>> ask ~s\n" srv)
      (send-packet sock m/s addr)
      (let ((buf (receive-packet sock)))
        (dbg "<<< got reply\n")
        (close sock)
        (parse-message buf)))))
