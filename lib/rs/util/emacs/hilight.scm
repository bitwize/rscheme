
(define (emacs-hilite (node <parse-node>))
  (let ((start (+ 1 (input-offset (location (start-token node)))))
	(end (+ 1
		(input-offset (location (end-token node)))
		(lexeme-length (end-token node)))))
    (write `(put-text-property ,start ,end 'face 'undef-var))
    (newline)))
