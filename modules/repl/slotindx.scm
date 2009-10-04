
(define (compile-slot-special sf form lex-envt dyn-envt mode what)
  ;;
  (define (find-slot-by-name type slot-name)
    (let loop ((at type))
      (if at
          (let ((pick (select (lambda (sd)
                                (eq? (name sd) slot-name))
                              (direct-slots at))))
            (if (pair? pick)
                (car pick)
                (if (null? (superclasses at))
                    (error
                     "No slot named ~s could be found in ~s or any ancestor"
                     slot-name
                     (name type))
                    (loop (car (superclasses at)))))))))
  ;;
  (let ((type (parse-type-expr (cadr form) lex-envt dyn-envt))
        (slot-name (caddr form)))
    ;;
    (if (not (symbol? slot-name))
        (error "Slot name ~s is not a <symbol>" slot-name))
    ;;
    (let ((sd (find-slot-by-name type slot-name)))
      ;;
      (make <ic-const>
            value: (case what
                     ((index) (index sd))
                     (else (error "bad slot-special: ~s" what)))))))
  
(define (compile/slot-index sf form lex-envt dyn-envt mode)
  (compile-slot-special sf form lex-envt dyn-envt mode 'index))
