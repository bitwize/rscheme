
;; Query / Resource Record / Message - Utility Functions

(def (query-rr=? q r)
  (and (string-ci=? (name q)  (name r))
       (eq?         (type q)  (type r))
       (eq?         (class q) (class r))))

(def (rr-filter q rrl)
  (filter (fn (r) (query-rr=? q r)) rrl))

(def (rr-find q rrl)
  (find (fn (r) (query-rr=? q r)) rrl))

(def (query->message q)
  (make <message>
    qr: #f
    rd: #t
    question-section: (list q)))

(def (query->message/no-rd q)
  (make <message>
    qr: #f
    rd: #f
    question-section: (list q)))
