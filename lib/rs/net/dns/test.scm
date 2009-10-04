
(load "all.scm")

(def buf (make-string 1024))

(def q (make-query "www.google.com." 'A 'IN))

(def m (query->message q))

(def m/s (msg->bs m))

(def m/v (list->vector (string->list m/s)))

(def sock (udp-sock))

;; (def addr (make-inet-socket-addr (make-inet-addr "24.26.193.62") 53))

(def addr (make-inet-socket-addr (make-inet-addr "127.0.0.1") 53))

(send-to sock m/s 0 (string-length m/s) #f addr)

(def fd-set (make-fd-set (list sock) (list sock) (list sock)))

;; (recv-from sock buf 0 1024 #f #f <object>)