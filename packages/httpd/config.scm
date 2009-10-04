
#|
   This http daemon is designed to simultaneously provide multiple
   multiple services.  This configuration provides:

      port
      ----
      8080   filtered pages from cmvc:sev1fam/web.1
             (cached)
      8081   filtered data from clone tree over the backing tree
             from donovan's `web' world
      8082   unfiltered data from donovan's `web' world
      8083   echo handler
      8084   AWS (Alternative Web Space)
|#

(define (main)
  (chdir "/u/donovan/work/d/web")
  ;;
  ;; store this vpath proc in a variable so that 8081 and 8082
  ;; share the same cache
  ;;
  (let ((work (vpath (file-tree "clone")
		     (file-tree ".backing"))))
    (run (cons (open-service 8080)
	       (http-protocol
		(cache-pages
		 (filter-pages
		  dumb-filter
		  (vpath (file-tree ".cache")
			 (cmvc-server "sev1fam" "web.1"))))))
	 (cons (open-service 8081) 
	       (http-protocol
		(filter-pages dumb-filter work)))
	 (cons (open-service 8082) 
	       (http-protocol work))
	 (cons (open-service 8083) 
	       (http-protocol echo-pages))
	 (cons (open-service 8084) 
	       (http-protocol (alternative-web-space ".aws"))))))

(define (dumb-filter writer)
  (let ((o (open-output-file "temp")))
    (writer o)
    (close-output-port o)
    (
;;
;;

(define (invoke-service-handler fd peer svc)
  (let ((inp (open-fd-input-port fd))
	(outp (open-fd-output-port fd)))
    (svc inp outp peer)
    (flush-output-port outp)
    (fd-close fd)))

(define (run-service ports)
  (let ((fds (make-fd-set (map car ports) '() '()))
	(handle-1 (lambda (service-fd)
		    (bind ((client-fd at-address (socket-accept service-fd)))
		      (if client-fd
			  (invoke-service-handler
			   client-fd
			   at-address
			   (cdr (assq service-fd ports))))))))
    (let loop ((ready-fds (fd-select 999 fds)))
      ;;
      ;; process a request from each ready port
      ;;
      (for-each handle-1 ready-fds)
      ;;
      ;; go and get some more
      ;;
      (loop))))

  (bind ((client-fd at-address (socket-accept fd)))
    (if client-fd
