,(use rs.util.relation
      rs.util.properties)

(define-class <person> (<object>)
  (properties init-value: '())
  (parent init-value: #f)
  (name type: <string>))

(define-index **person/name** (name <person>) :unique)

(define-method write-object ((self <person>) port)
  (format port "#[<person> ~a]" (name self)))

(define-relation <-parent-child->
  ;;
  candidate-keys: '((child))
  ;;
  (mother type: <person>
          use-slot: #t)
  (father type: <person>
          use-slot: 'properties)
  (child type: <person>
         use-slot: 'parent))


(define donovan (make <person> name: "Donovan"))
(define sue (make <person> name: "Sue"))
(define lane (make <person> name: "Lane"))
(define ellen (make <person> name: "Ellen"))
(define jason (make <person> name: "Jason"))


(establish <-parent-child-> mother: sue father: donovan child: lane)
(establish <-parent-child-> mother: sue father: donovan child: ellen)
(establish <-parent-child-> mother: sue father: donovan child: jason)

;;;

(define-relation <-married->
  ;;
  candidate-keys: '((husband wife))
  ;;
  (husband type: <person>)
  (wife type: <person> use-slot: 'properties))
  

(establish <-married-> husband: donovan wife: sue)

(define-method children ((self <person>))
  (map child (append (query <-parent-child-> mother: self)
                     (query <-parent-child-> father: self))))

;;

(define alice (make <person> name: "Alice"))
(define bob (make <person> name: "Bob"))
(define sally (make <person> name: "Sally"))
(define zac (make <person> name: "Zac")) 

(define john (make <person> name: "John"))
(define jen (make <person> name: "Jen"))
(define david (make <person> name: "David"))

(establish <-parent-child-> mother: alice father: bob child: sally)
(establish <-parent-child-> mother: alice father: bob child: zac)

(establish <-parent-child-> mother: jen father: john child: david)

(children alice)
(retract (car (query <-parent-child-> child: zac)))

;;;  Testing a post-hoc relation...
;;;  (uses properties, but has no extent and no private slots)

(define-relation <-friend->
  ;;
  candidate-keys: '((peer1 peer2))
  keep-extent?: #f
  ;;
  (peer1 type: <person> use-slot: 'properties)
  (peer2 type: <person> use-slot: 'properties))

;;
(establish <-friend-> peer1: donovan peer2: john)
