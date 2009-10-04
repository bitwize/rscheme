
(define-method parse->datum ((self <leaf-node>))
  (assert (eq? (start-token self) (end-token self)))
  (data (start-token self)))

(define-method parse->datum ((self <list-node>))
  (vector->list (vector-map parse->datum (contents self))))

(define-method parse->datum ((self <dotted-list-node>))
  (append (next-method) (parse->datum (tail self))))

(define-method parse->datum ((self <vector-node>))
  (vector-map parse->datum (contents self)))

(define-method parse->datum ((self <sqlist-node>))
  (cons 'sqlist (vector->list (vector-map parse->datum (contents self)))))
#|
  (signal (make <no-datum-for-sqlist>
		sqlist: self))
|#
(define-method parse->datum ((self <modifier-node>))
  (list (modifier self) (parse->datum (content self))))

;;;

(define-method parse-item ((self <leaf-token>) port)
  (make <leaf-node>
	start-token: self
	end-token: self))

(define-method parse-item ((self <close-paren-token>) port)
  (signal (make <mismatched-delimiter>
		source: port
		token: self)))

(define-method parse-item ((self <close-sqbracket-token>) port)
  (signal (make <mismatched-delimiter>
		source: port
		token: self)))

(define-method parse-item ((self <dot-token>) port)
  (signal (make <misplaced-dot>
		source: port
		token: self)))


(define-method parse-item ((self <backquote-token>) port)
  (parse/modif self port 'quasiquote))

(define-method parse-item ((self <quote-token>) port)
  (parse/modif self port 'quote))

(define-method parse-item ((self <unquote-token>) port)
  (parse/modif self port 'unquote))

(define-method parse-item ((self <unquote-splicing-token>) port)
  (parse/modif self port 'unquote-splicing))

(define (parse/modif (start <token>) (port <input-port>) (modif <symbol>))
  (let ((sub (parse-item (scan port) port)))
    (if (eof-object? sub)
	(signal (make <missing-modified-item>
		      source: port
		      token: start
		      modifier: modif))
	(make <modifier-node>
	      start-token: start
	      end-token: (end-token sub)
	      modifier: modif
	      content: sub))))

(define-method parse-item ((self <open-paren-token>) port)
  (let loop ((r '()))
    (bind ((tok (scan port)))
      (if (instance? tok <close-paren-token>)
	  (make <list-node>
		start-token: self
		end-token: tok
		contents: (list->vector (reverse r)))
	  (if (instance? tok <dot-token>)
	      (let* ((after-dot (parse-item (scan port) port))
		     (close-p (scan port)))
		(if (instance? close-p <close-paren-token>)
		    (make <dotted-list-node>
			  start-token: self
			  end-token: close-p
			  contents: (list->vector (reverse r))
			  tail: after-dot)
		    ;; note that if `after-dot' is EOF, then `close-p' is too
		    (if (eof-object? close-p)
			(signal (make <unterminated-list>
				      source: port
				      token: self))
			(signal (make <expected-close-paren-after-tail>
				      source: port
				      token: close-p)))))
	      (if (eof-object? tok)
		  (signal (make <unterminated-list>
				source: port
				token: self))
		  (loop (cons (parse-item tok port) r))))))))

(define-method parse-item ((self <open-vector-token>) port)
  (let loop ((r '()))
    (bind ((tok (scan port)))
      (if (instance? tok <close-paren-token>)
	  (make <vector-node>
		start-token: self
		end-token: tok
		contents: (list->vector (reverse r)))
	  (if (eof-object? tok)
	      (signal (make <unterminated-vector>
			    source: port
			    token: self))
	      (loop (cons (parse-item tok self) r)))))))

(define-method parse-item ((self <open-sqbracket-token>) port)
  (let loop ((r '()))
    (bind ((tok (scan port)))
      (if (instance? tok <close-sqbracket-token>)
	  (make <sqlist-node>
		start-token: self
		end-token: tok
		contents: (list->vector (reverse r)))
	  (if (eof-object? tok)
	      (signal (make <unterminated-sqlist>
			    source: port
			    token: self))
	      (loop (cons (parse-item tok self) r)))))))

(define-method parse-item ((self <eof>) port)
  self)

(define-method input-port-parse ((self <input-port>))
  (parse-item (scan self) self))

;;;
