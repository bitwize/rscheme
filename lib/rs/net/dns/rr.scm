
(define-class <rr> (<object>) name type class ttl rdata)

(def (make-rr name type class ttl rdata)
  (make <rr>
    name:  name
    type:  type
    class: class
    ttl:   ttl
    rdata: rdata))

(define-class <hinfo> (<object>) cpu os)
(define-class <mx>    (<object>) preference exchange)
(define-class <soa>   (<object>)
  mname rname serial refresh retry expire minimum)

(define-method write-object ((self <rr>) port)
  (format port "#[<rr> ~s ~s ~s ~s ~s]"
	  (name self)
	  (type self)
	  (class self)
	  (ttl self)
	  (rdata self)))

(define-method write-object ((self <hinfo>) port)
  (format port "#[<hinfo> ~s ~s]"
	  (cpu self)
	  (os  self)))

(define-method write-object ((self <mx>) port)
  (format port "#[<mx> ~s ~s]"
	  (preference self)
	  (exchange self)))

(define-method write-object ((self <soa>) port)
  (format port "#[<soa> ~s ~s ~s ~s ~s ~s ~s]"
	  (mname   self)
	  (rname   self)
	  (serial  self)
	  (refresh self)
	  (retry   self)
	  (expire  self)
	  (minimum self)))

(def (ns-rr? rr)  (eq? 'NS  (type rr)))
(def (soa-rr? rr) (eq? 'SOA (type rr)))