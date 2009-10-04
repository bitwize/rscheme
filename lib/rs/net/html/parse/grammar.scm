;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;-*-Scheme-*-;;;;

(define (tagged stag content)
  (cons (cadr stag) 
	(cons (cddr stag)
	      content)))

(define $basic-html-tags '(b i strong em td font img tr center body html
			     title head map area sup table a))
(define $opt-end-html-tags '(p))
(define $empty-html-tags '(hr br basefont))

(define $html-grammar
  `(
    ;; start
    (rule start html
	  (lambda (body) body))
    ;;
    (rule html
	  (repeat html-elem) 
	  ;; `lst' comes back with an extra level of lists, due to
	  ;; the fact that the operand to `repeat' is a sequence which
	  ;; generates a list of meanings for each element in the list
	  ;; probably that should be fixed in `gen-repeat-node' to better
	  ;; factor functionality (ie, no implicit `sequence' operators)
	  (lambda (lst) (map car lst)))
    ;;
    (rule html-elem (string) (lambda (str) str))
    ;;
    ,@(map (lambda (t)
	     `(rule html-elem (sequence (stag ,t) 
					html
					(etag ,t))
		    (lambda (s b e) (tagged s b))))
	   $basic-html-tags)

    ,@(map (lambda (t)
	     `(rule html-elem (sequence (stag ,t)
					html
					(optional (etag ,t)))
		    (lambda (s b e) (tagged s b))))
	   $opt-end-html-tags)

    ,@(map (lambda (t)
	     `(rule html-elem (sequence (stag ,t)
					(optional (etag ,t)))
		    (lambda (s b e) (tagged s b))))
	   $empty-html-tags)
    ))

(define *grammar* (eval-gram $html-grammar))
