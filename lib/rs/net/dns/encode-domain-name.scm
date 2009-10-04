
(def (dn->bs dn)
  (apply bs-append
	 (map label->bs
	      (dn->labels dn))))

(def (dn->labels dn)
  (string-split dn "."))

(def (domain-name-null? dn)
  (string=? "" dn))

(def (domain-name-cdr dn)
  (string-join "."
	       (cdr (string-split dn "."))))