
,(use rs.db.oodb)

(define-class <person> (<entity>)
  ssn
  first-name
  middle-name
  last-name
  address)

(define-class <school> (<entity>)
  name
  address)

;  ,(use debugger) ,(use compiler) ,(trace compile)


(define-world-schema test
  <person>
  <school>)
 
(define (create-records)
  (pmake <person>
    ssn: "551-71-9670"
    first-name: "Donovan"
    middle-name: "Michael"
    last-name: "Kolbly"
    address: "8710 Mosquero Circle")
  (pmake <school>
    name: "UT"
    address: "Austin, TX"))

(define (t)
  (bind ((w ps (new-world test "/tmp/t1.sto")))
    (with-world w (lambda () (do-transaction create-records)))
    (commit-world w ps)))
