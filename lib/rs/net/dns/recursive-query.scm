
;; srvl : Server List (nameservers)
;; q    : Query

(def (recursive-query srvl q)
  (let* ((srv  (list-ref-random srvl))
	 (qmsg (query->message/no-rd q))
	 (rmsg (message-transaction/udp srv qmsg)))
    (cache-message rmsg)
    (case (classify-message rmsg)
      ((NXDOMAIN)
       rmsg)
      ((SERVER-FAILURE)
       (let ((new-srvl (remove srv srvl)))
	 (if (null? new-srvl)
	     rmsg
	     (recursive-query new-srvl q))))
      ((ANSWERED)
       rmsg)
      ((CNAME)
       rmsg)
      ((NO-NAME-SERVERS)
       rmsg)
      (else
       (recursive-query (extract-ns-ips rmsg) q)))))

;; Given a message, get the NS records from the authority section and
;; translate the corresponding server names to IP addresses.
       
(def (extract-ns-ips msg)
  (filter identity
	  (map name->ip
	       (map rdata
		    (filter ns-rr? (authority-section msg))))))

(def (classify-message msg)
  (let ((query (car (question-section msg))))
    (cond ((eq? 'NAME-ERROR (rcode msg))
	   'NXDOMAIN)
	  ((eq? 'SERVER-FAILURE (rcode msg))
	   'SERVER-FAILURE)
	  ((rr-find query (answer-section msg))
	   'ANSWERED)
	  ((rr-find (make-query (name query) 'CNAME (class query))
		    (answer-section msg))
	   'CNAME)
	  ((null? (filter ns-rr? (authority-section msg)))
	   'NO-NAME-SERVERS)
	  (else
	   'UNCLASSIFIED))))

(def (name->ip name)
  (or (name->ip/cache   name)
      (name->ip/servers name)))

(def (name->ip/cache name)
  (let ((name-hits  (cache-get (make-query name 'A 'IN)))
	(cname-hits (cache-get (make-query name 'CNAME 'IN))))
    (cond (name-hits
	   (rdata (list-ref-random name-hits)))
	  (cname-hits
	   (name->ip (rdata (list-ref-random cname-hits))))
	  (else
	   #f))))

(def (name->ip/servers name)
  (let* ((rmsg (recursive-query *root-nameservers*
				(make-query name 'A 'IN)))
	 (name-hits (rr-filter (make-query name 'A 'IN)
			       (answer-section rmsg)))
	 (cname-hits (rr-filter (make-query name 'CNAME 'IN)
				(answer-section rmsg))))
    (cond ((not-null? name-hits)
	   (rdata (list-ref-random name-hits)))
	  ((not-null? cname-hits)
	   (name->ip (rdata (list-ref-random cname-hits))))
	  (else
	   #f))))

(def (names->ip-list names)
  (filter identity (map name->ip names)))

;; Add resource records from a message to the cache. Also, cache
;; negative results (NXDOMAIN) if applicable.

(def (cache-message msg)
  (let ((q (car (question-section msg))))
    ;; Cache a negative response
    (let ((soa-rr (find (fn (rr) (soa-rr? rr)) (authority-section msg))))
      (if (and (eq? 'NAME-ERROR (rcode msg))
	       soa-rr)
	  (cache-nx q (minimum (rdata soa-rr)))))
    ;; Cache the resource records
    (for-each cache-add-rr (answer-section msg))
    (for-each cache-add-rr (authority-section msg))
    (for-each cache-add-rr (additional-section msg))))

;; A new version of cache-message which uses the new cache
;; implementation.

;; (def (cache-message msg)

;;   ;; Handle NX and null responses
;;   (let ((q      (car (question-section msg)))
;; 	(soa-rr (find (rn (rr) (soa-rr? rr)) (authority-section msg)))
;; 	(code   (classify-message msg)))

;;     (if soa-rr
;; 	(case code
;; 	  ((NAME-ERROR)
;; 	   ;; Cache a negative response
;; 	   (cache-add-nx (name q) (minimum (rdata soa-rr))))
;; 	  ((NO-NAME-SERVERS)
;; 	   ;; Cache a null response
;; 	   (cache-add-null q (minimum (rdata soa-rr)))))))

;;   ;; Cache the resource records
;;   (for-each cache-add-rr (answer-section msg))
;;   (for-each cache-add-rr (authority-section msg))
;;   (for-each cache-add-rr (additional-section msg)))

