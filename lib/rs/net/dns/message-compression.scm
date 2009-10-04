
(def (quux m/o i t)
  (if (null? m/o)
      '()
      (let ((new-rr (append
		     (compress-name (get-name/o m/o) t)
		     (skip-name (get-rr/o m/o)))))
	(append new-rr
		(quux (skip-rr/o m/o)
		      (+ i (len new-rr))
		      (digest-name (get-name/o m/o) i t))))))
