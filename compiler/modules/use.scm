#|------------------------------------------------------------*-Scheme-*--|
 | File:    compiler/modules/use.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.2
 | File mod date:    1997-11-29 23:10:28
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  (rsc)
 |
 `------------------------------------------------------------------------|#


;;
;; returns a table mapping target (current) module binding names
;; to source (imported) module binding names
;;

(define (inval-val kwd rqd val)
  (error/syntax
   "`~s' value is invalid (not ~a)\n==> ~s" kwd rqd val))

(define (rename-form? f)
  (and (list? f)
       (eq? (length f) 3)
       (eq? (cadr f) '=>)
       (symbol? (car f))
       (symbol? (caddr f))))

(define (build-use-name-map from-module forms)
  (let ((seen '())
	(import 'all)
	(exclude '())
	(prefix "")
	(rename '()))
    (for-each-keyword
     (lambda (keyword value)
       ;;
       (if (memq keyword seen)
	   (error/syntax "`~s' keyword given more than once" keyword))
       (set! seen (cons keyword seen))
       ;;
       (case keyword
	 ((import:)
	  (if (and (not (eq? value 'all))
		   (or (not (list? value))
		       (not (every? (lambda (f)
				      (or (symbol? f)
					  (rename-form? f)))
				    value))))
	      (inval-val keyword "`all' or a valid list" value))
	  (set! import value)
	  #t)
	 ;;
	 ((exclude:)
	  (if (or (not (list? value))
		  (not (every? symbol? value)))
	      (inval-val keyword "a list of symbols" value))
	  (set! exclude value)
	  #t)
	 ;;
	 ((prefix:)
	  (if (not (string? value))
	      (inval-val keyword "a string" value))
	  (set! prefix value)
	  #t)
	 ;;
	 ((rename:)
	  (if (or (not (list? value))
		  (not (every? rename-form? value)))
	      (inval-val keyword "a list of renames" value))
	  (set! rename value)
	  #t)
	 ;;
	 ((export:)
	  (error/syntax "`export:' in a module use is not yet supported"))))
     forms)
    ;;
    (if (and (pair? exclude)
	     (not (eq? import 'all)))
	(error/syntax "`export:' specified w/o and `import:' of all"))
    ;;
    (let ((name-map (make-symbol-table)))
      ;;
      (if (eq? import 'all)
	  (table-for-each 
	   (module-exports from-module)
	   (if (string=? prefix "")
	       (lambda (h k v)
		 (table-insert! name-map 
				k
				k))
	       (lambda (h k v)
		 (table-insert! name-map
				k 
				(string->symbol
				 (string-append prefix 
						(symbol->string k)))))))
	  (for-each 
	   (lambda (i)
	     (if (pair? i)
		 (table-insert! name-map
				(car i)
				(caddr i))
		     (table-insert! name-map i i)))
	   import))
      ;;
      (for-each (lambda (x)
		  (table-remove! name-map x))
		exclude)
      ;;
      (for-each (lambda (r)
		  (table-insert! name-map
				 (car r)
				 (caddr r)))
		rename)
      ;;
      name-map)))
