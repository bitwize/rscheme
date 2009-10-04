
;; DNS message header layout :
;;
;;                                     1  1  1  1  1  1
;;       0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     |                      ID                       |
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     |                    QDCOUNT                    |
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     |                    ANCOUNT                    |
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     |                    NSCOUNT                    |
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     |                    ARCOUNT                    |
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

(def header-formatter (make-bs-formatter '(1 4 1 1 1 1 3 4)))

(def (msg->bs m)
  (let ((qdcount (len (question-section   m)))
	(ancount (len (answer-section     m)))
	(nscount (len (authority-section  m)))
	(arcount (len (additional-section m))))
    (bs-append (uint16->bs (id m))
	       (uint16->bs
		(header-formatter (list (bool->num   (qr m))
					(opcode->num (opcode m))
					(bool->num   (aa m))
					(bool->num   (tc m))
					(bool->num   (rd m))
					(bool->num   (ra m))
					             (z m)
					(rcode->num  (rcode m)))))
	       (uint16->bs qdcount)
	       (uint16->bs ancount)
	       (uint16->bs nscount)
	       (uint16->bs arcount)
	       (apply bs-append (map query->bs (question-section   m)))
	       (apply bs-append (map rr->bs    (answer-section     m)))
	       (apply bs-append (map rr->bs    (authority-section  m)))
	       (apply bs-append (map rr->bs    (additional-section m))))))


      
