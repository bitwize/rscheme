
(def (query->bs q)
  (bs-append (dn->bs (name q))
	     (uint16->bs (type->num (type q)))
	     (uint16->bs (class->num (class q)))))
