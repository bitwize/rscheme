
(define-class <parse-node> (<object>) :abstract
  start-token
  end-token)

(define-class <leaf-node> (<parse-node>))

(define-method contents ((self <leaf-node>))
  (data (start-token self)))

(define-class <list-node> (<parse-node>)
  contents)

(define-class <dotted-list-node> (<list-node>)
  tail)

(define-class <vector-node> (<parse-node>)
  contents)

(define-class <sqlist-node> (<parse-node>)
  contents)

(define-class <modifier-node> (<parse-node>)
  (modifier type: <symbol>)
  (content type: <parse-node>))

;;;

(&module
 (export <parse-node> start-token end-token
	 <leaf-node> contents
	 <list-node>
	 <dotted-list-node> tail
	 <vector-node>
	 <sqlist-node>
	 <modifier-node>))

;;;

(define (list-node-empty? (node <list-node>))
  (eq? (vector-length (contents node)) 0))

(define (list-node-head (node <list-node>))
  (vector-ref (contents node) 0))

(define-method list-node-last-tail ((node <list-node>))
  (make <list-node>
	start-token: (end-token node)
	end-token: (end-token node)
	contents: '#()))

(define-method list-node-last-tail ((node <dotted-list-node>))
  (tail node))

(define (list-node-tail (node <list-node>))
  (if (= (vector-length (contents node)) 1)
      (list-node-last-tail node)
      (let ((v (subvector (contents node) 1)))
	(make <list-node>
	      start-token: (start-token (vector-ref v 0))
	      end-token: (end-token node)
	      contents: v))))

(define (list-node-length (node <list-node>))
  (vector-length (contents node)))

(define (list-node-nth (node <list-node>) n)
  (vector-ref (contents node) n))

(&module
 (export list-node-empty?
	 list-node-head
	 list-node-tail
         list-node-nth
         list-node-length))

