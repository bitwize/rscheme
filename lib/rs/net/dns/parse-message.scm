
(def (subvec v i j)
  (subvector v i (+ 1 j)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Single, Double, and Quad functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def (get-single m/v i)
  (char->integer (vref m/v i)))

(def (get-double m/v i)
  (+ (<< (get-single m/v i) 8)
     (get-single m/v (+ i 1))))

(def (get-quad m/v i)
  (+ (<< (get-single m/v i)       24)
     (<< (get-single m/v (+ i 1)) 16)
     (<< (get-single m/v (+ i 2)) 8)
     (get-single m/v (+ i 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bit functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define get-bit
  (let ((mask #b0001))
    (lambda (message/vector j)
      (bitwise-and
       mask
       (arithmetic-shift (get-single message/vector (quotient j 8))
                         (- (- 7 (modulo j 8))))))))

(define get-bits
  (lambda (message/vector j k)
    (let ((temp
           (list->number
            (map char->integer
                 (vector->list
                  (subvec message/vector
			      (quotient j 8) (quotient k 8)))))))
      (bitwise-and (bit-mask (+ (- k j) 1))
                   (arithmetic-shift temp
                                     (- (- 7 (modulo k 8))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Label functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def (get-label m/v i)
  (vector->string
   (subvec m/v
	   (+ i 1)
	   (+ i (label-length m/v i)))))

(def (skip-label m/v i)
  (+ i (label-length m/v i) 1))

(def (label-length m/v i)
  (get-single m/v i))

(def (null-label? m/v i)
  (= 0 (get-single m/v i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Name functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def (get-name m/v i)
  (cond ((null-label? m/v i)
	 "")
	((pointer? m/v i)
	 (get-name m/v (pointer m/v i)))
	(else
	 (string-append (get-label m/v i)
			"."
			(get-name m/v (skip-label m/v i))))))

(def (skip-name m/v i)
  (cond ((null-label? m/v i)
	 (+ i 1))
	((pointer? m/v i)
	 (+ i 2))
	(else
	 (skip-name m/v (skip-label m/v i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Query functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Diagram of a query, from RFC 1035.
;;
;;                                     1  1  1  1  1  1
;;       0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     |                                               |
;;     /                     QNAME                     /
;;     /                                               /
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     |                     QTYPE                     |
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     |                     QCLASS                    |
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

(def (get-query m/v i)
  (let ((j (skip-name m/v i)))
    (make <query>
      name:  (get-name m/v i)
      type:  (num->type  (get-double m/v j))
      class: (num->class (get-double m/v (+ j 2))))))

(def (skip-query m/v i)
  (+ (skip-name m/v i) 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RR functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Diagram of a resource record, from RFC 1035.
;;
;;                                     1  1  1  1  1  1
;;       0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     |                                               |
;;     /                                               /
;;     /                      NAME                     /
;;     |                                               |
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     |                      TYPE                     |
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     |                     CLASS                     |
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     |                      TTL                      |
;;     |                                               |
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     |                   RDLENGTH                    |
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--|
;;     /                     RDATA                     /
;;     /                                               /
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

(def (get-rr m/v i)
  (let ((j (skip-name m/v i)))
    (let ((type  (num->type  (get-double m/v j)))
	  (class (num->class (get-double m/v (+ j 2)))))
      (make <rr>
	name:  (get-name m/v i)
	type:  type
	class: class
	ttl:   (get-quad m/v (+ j 4))
	rdata: (get-rdata type m/v (+ j 10))))))

;; This version of get-rdata is passed a symbol indicating what type
;; of RR to read. Of course, new RR types may be introduced into the
;; world, and since this version get-rdata has hard coded knowledge
;; about the standard RR types, extensibility might be
;; inconvenient. In the future, a table indexed by type-code could be
;; used to point to the various functions used to handle the different
;; types.

(def (get-rdata type m/v i)
  (case type
    ((CNAME NS PTR)
     (get-name m/v i))
    ((MX)
     (make <mx>
       preference: (get-double m/v i)
       exchange:   (get-name m/v (+ i 2))))
    ((SOA)
     (let ((j (skip-name m/v (skip-name m/v i))))
       (make <soa>
	 mname:   (get-name m/v i)
	 rname:   (get-name m/v (skip-name m/v i))
	 serial:  (get-quad m/v j)
	 refresh: (get-quad m/v (+ j 4))
	 retry:   (get-quad m/v (+ j 8))
	 expire:  (get-quad m/v (+ j 12))
	 minimum: (get-quad m/v (+ j 16)))))
    ((A)
     (get-ip m/v i))
    ((AAAA)
     (get-ipv6 m/v i))))

(def (skip-rr m/v i)
  (let ((j (skip-name m/v i)))
    (let ((rdlength (get-double m/v (+ j 8))))
      (+ j 10 rdlength))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def (get-ip m/v i)
  (format "~a.~a.~a.~a"
	  (get-single m/v i)
	  (get-single m/v (+ i 1))
	  (get-single m/v (+ i 2))
	  (get-single m/v (+ i 3))))

(def (get-ipv6 m/v i)
  (format "~a:~a:~a:~a:~a:~a:~a:~a"
	  (get-double m/v i)
	  (get-double m/v (+ i 2))
	  (get-double m/v (+ i 4))
	  (get-double m/v (+ i 6))
	  (get-double m/v (+ i 8))
	  (get-double m/v (+ i 10))
	  (get-double m/v (+ i 12))
	  (get-double m/v (+ i 14))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pointer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def pointer?
  (let ((mask #b11000000))
    (fn (m/v i)
      (= mask (bitwise-and mask (get-single m/v i))))))

(def pointer
  (let ((mask #b0011111111111111))
    (fn (m/v i)
      (bitwise-and mask (get-double m/v i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Query section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def (get-query-section c m/v i)
  (if (zero? c)
      '()
      (cons (get-query m/v i)
	    (get-query-section (- c 1) m/v (skip-query m/v i)))))

(def (skip-query-section c m/v i)
  (if (zero? c)
      i
      (skip-query-section (- c 1) m/v (skip-query m/v i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RR section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def (get-rr-section c m/v i)
  (if (zero? c)
      '()
      (cons (get-rr m/v i)
	    (get-rr-section (- c 1) m/v (skip-rr m/v i)))))

(def (skip-rr-section c m/v i)
  (if (zero? c)
      i
      (skip-rr-section (- c 1) m/v (skip-rr m/v i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse-message
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Diagram of a header, from RFC 1035.
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

(def header-length 12)

(def (parse-message* m/v)
  (let ((query-count      (get-double m/v 4))
	(answer-count     (get-double m/v 6))
	(authority-count  (get-double m/v 8))
	(additional-count (get-double m/v 10)))
    ;; Offset indeces
    (let* ((i header-length)
	   (j (skip-query-section query-count     m/v i))
	   (k (skip-rr-section    answer-count    m/v j))
	   (l (skip-rr-section    authority-count m/v k)))
      (make <message>
	id:     (get-double m/v 0)
	qr:     (num->bool   (get-bit  m/v (+ 16 0)))
	opcode: (num->opcode (get-bits m/v (+ 16 1) (+ 16 4)))
	aa:     (num->bool   (get-bit  m/v (+ 16 5)))
	tc:     (num->bool   (get-bit  m/v (+ 16 6)))
	rd:     (num->bool   (get-bit  m/v (+ 16 7)))
	ra:     (num->bool   (get-bit  m/v (+ 16 8)))
	z:      (get-bits   m/v (+ 16 9)  (+ 16 11))
	rcode:  (num->rcode (get-bits   m/v (+ 16 12) (+ 16 15)))
	question-section:   (get-query-section query-count      m/v i)
	answer-section:     (get-rr-section    answer-count     m/v j)
	authority-section:  (get-rr-section    authority-count  m/v k)
	additional-section: (get-rr-section    additional-count m/v l)))))

;; m/s : message as a string

(def (parse-message m/s)
  (parse-message* (list->vector (string->list m/s))))
