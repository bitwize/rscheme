(define-module srfi.1 ()
  (&module
   (implements SRFI-1 srfi-1)
   (import usual-inlines rs.sys.undefine))
  ;;
  ;; adapter code
  ;;
  (undefine error)
  (undefine set-car!)
  (undefine set-cdr!)
  ;;
  ;; suppress `primop used in tail posn' message
  (define-syntax (set-car! x v)
    (begin (with-module usual-inlines (set-car! x v)) (values)))
  (define-syntax (set-cdr! x v)
    (begin (with-module usual-inlines (set-cdr! x v)) (values)))
 
  (define error
    (let ((error error))
      (lambda (msg . args)
	(apply
	 error (apply string-append "~a" (map (lambda (s)
						"\n   ~s")
					      args))
	 msg
	 args))))
  
  (define-macro (receive args init . body)
    `(bind ((,@args ,init)) ,@body))
  ;;
  (&module
   (load "ref.scm"))
  ;;

  (&module
   (export xcons tree-copy make-list list-tabulate list* list-copy
	   iota. circular-list zip first second third fourth fifth
	   sixth seventh eighth ninth tenth take drop take! drop!
	   unzip2 unzip3 unzip4 unzip5
	   reverse-append reverse-append!  unfold/tail unfold foldl
	   foldr pair-foldr pair-foldl reducel reducer append-map
	   append-map! pair-for-each map! filter-map map-in-order
	   filter filter! partition partition! remove remove!  del
	   del! delv delv! delete delete! mem
	   del-duplicates del-duplicates! delq-duplicates
	   delv-duplicates delete-duplicates delq-duplicates!
	   delv-duplicates! delete-duplicates! ass acons alist-copy
	   alist-delete alist-delete! del-ass del-ass! del-assq
	   del-assq! del-assv del-assv! del-assoc del-assoc! find
	   find-tail any every list-index)))
