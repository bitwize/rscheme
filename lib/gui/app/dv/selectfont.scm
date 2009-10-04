
(define-interactive (set-font-name (name <string>))
  (interactive (minibuffer <string> "Font name: "))
  (let* ((client (current-client))
	 (old-font (active-font client)))
    (set-active-font! client
		      (make <text-font>
			    font-name: name
			    font-member: (font-member old-font)
			    font-size: (font-size old-font)))))


(define-interactive (set-font-member (style <string>))
  (interactive (minibuffer <string> "Font style: "))
  (let* ((client (current-client))
	 (old-font (active-font client)))
    (set-active-font! client
		      (make <text-font>
			    font-name: (font-name old-font)
			    font-member: style
			    font-size: (font-size old-font)))))


(define-interactive (set-font-size (size <real>))
  (interactive (minibuffer <number> "Font size: "))
  (let* ((client (current-client))
	 (old-font (active-font client)))
    (set-active-font! client
		      (make <text-font>
			    font-name: (font-name old-font)
			    font-member: (font-member old-font)
			    font-size: size))))
