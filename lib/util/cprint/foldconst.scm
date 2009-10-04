
(define-patterns fold-const
  ((expt ?qu ?qv)                ?x   where: ((?x (expt ?qu ?qv))))
  ((arithmetic-shift ?x 0)       ?x)
  ((arithmetic-shift ?qu ?qv)    ?x   where: ((?x (if (< ?qv 0)
						      (arithmetic-shift-right 
						       ?qu 
						       (- ?qv))
						      (shift-left ?qu ?qv)))))
  ;
  ((* ?x 1)                      ?x)
  ((+ ?x 0)                      ?x)
  ((- ?x 0)                      ?x)
  ((/ ?x 1)                      ?x)
  ;
  ((* ?qu ?qv)                   ?x   where: ((?x (* ?qu ?qv))))
  ((+ ?qu ?qv)                   ?x   where: ((?x (+ ?qu ?qv))))
  ((- ?qu ?qv)                   ?x   where: ((?x (- ?qu ?qv))))
  ((- ?qu)                       ?x   where: ((?x (- ?qu))))
  ((/ ?qu ?qv)                   ?x   where: ((?x (/ ?qu ?qv))))
  ((quotient ?k ?l)              ?x   where: ((?x (quotient ?k ?l))))
  ((modulo ?k ?l)                ?x   where: ((?x (modulo ?k ?l))))
  ((remainder ?k ?l)             ?x   where: ((?x (remainder ?k ?l))))
  ;
  ((< ?qu ?qv)                   ?x   where: ((?x (< ?qu ?qv))))
  ((<= ?qu ?qv)                  ?x   where: ((?x (<= ?qu ?qv))))
  ((> ?qu ?qv)                   ?x   where: ((?x (> ?qu ?qv))))
  ((>= ?qu ?qv)                  ?x   where: ((?x (>= ?qu ?qv))))
  ((= ?qu ?qv)                   ?x   where: ((?x (= ?qu ?qv))))
  ;
  ((if (if ?p ?q ?r) ?s ?t)      (if ?p (if ?q ?s ?t) (if ?r ?s ?t)))
  ((if #t ?p ?q)                 ?p)
  ((if #f ?p ?q)                 ?q)
  ;
  ((+ (+ ?x ?y) ?z)              (+ ?x (+ ?y ?z)))
  ((+ ?qu ?x)                    (+ ?x ?qu))
  ;
  ((- (- ?x ?y) ?z)              (- ?x (+ ?y ?z)))
  ((- ?qu ?x)                    (- ?x ?qu))
  ;
  ((- (+ ?x ?y) ?z)              (+ ?x (- ?y ?z)))
  ((+ (- ?x ?y) ?z)              (+ ?x (- ?z ?y)))
  ;
  ((* (* ?x ?y) ?z)              (* ?x (* ?y ?z)))
  ((* ?qu ?x)                    (* ?x ?qu))
  ;
  ((* (+ ?x ?qu) ?qv)            (+ (* ?x ?qv) (* ?qu ?qv)))
  ;
  ; decompose multi-arg versions
  ;
  ((+ ?a ?b ?c . ?r)             (+ (+ ?a ?b) ?c . ?r))
  ((* ?a ?b ?c . ?r)             (* (* ?a ?b) ?c . ?r))
  ((/ ?a ?b ?c . ?r)             (/ (/ ?a ?b) ?c . ?r))
  ((- ?a ?b ?c . ?r)             (- (- ?a ?b) ?c . ?r))
  ;
  ((and ?x ?y ?z . ?r)           (and ?x (and ?y ?z . ?r)))
  ((or ?x ?y ?z . ?r)            (or ?x (or ?y ?z . ?r)))
)

