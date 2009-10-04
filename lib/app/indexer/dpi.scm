
;;;  Data/Program Interface (as opposed to API)
;;;
;;;  defines the interface to the persistent object store

(define (setup-indirects ps)
  (register-indirect-page ps
			  100
			  (vector <document-index>
				  <keyword-index>
				  <bit-set>
				  <bit-cluster>
				  <email-archive>
				  <extern-email-message>
				  <inline-email-message>)))


;;;
