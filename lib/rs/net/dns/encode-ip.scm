
(def (ip->bs ip)
  (apply bs-append
	 (map uint8->bs
	      (map string->number
		   (string-split ip ".")))))