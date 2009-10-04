
(define *sd1* '((global x-spacing: 100
			y-spacing: 50)
		(free label: "free"
		      

(load "arrow.scm")

(define-interactive (tsd view)
  (interactive (owner))
  (let ((p (eval-state-diagram *sd1*)))
    (pp p)
    (paste-from-extern p
		       (page-contents (view-page (underlying-object view)))
		       (make-size 100 100))
    (clear-all-areas (in-document view))))
