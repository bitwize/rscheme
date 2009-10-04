
(defmacro with-rr-fields (r . body)
  `(let ((name (rr-name r))
	 (type (rr-type r))
	 (class (rr-class r))
	 (ttl   (rr-ttl r))
	 (rdata (rr-rdata r)))
     ,@body))
