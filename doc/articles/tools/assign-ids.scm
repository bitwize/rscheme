(define (sxml:document? x)
  (and (pair? x) (eq? (car x) '*TOP*)))

(define *want-uid* (make-symbol-table))
(table-insert! *want-uid* 'part #t)
(table-insert! *want-uid* 'chapter #t)
(table-insert! *want-uid* 'sect1 #t)
(table-insert! *want-uid* 'sect2 #t)
(table-insert! *want-uid* 'sect3 #t)
(table-insert! *want-uid* 'formalpara #t)
(table-insert! *want-uid* 'figure #t)
(table-insert! *want-uid* 'table #t)
(table-insert! *want-uid* 'example #t)
(table-insert! *want-uid* 'co #t)

(define (ID n)
  (cond
   ((assq '*ID* (sxml:attributes n))
    => cadr)
   (else 
    #f)))

(define (assign-ids src)
  (cond
   ((and (pair? src) (memq (car src) '(@ *XML* *DECL* *PI*)))
    src)
   ;;
   ((sxml:entityref? src)
    (format #t "Unexpanded entity: ~s\n" src)
    src)
   ;;
   ((sxml:document? src)
    (cons (car src) (map assign-ids src)))
   ;;
   ((sxml:element? src)
    (let ((attrs (sxml:attributes src))
          (rest (map assign-ids (sxml:children src))))
      ;;
      (if (table-key-present? *want-uid* (car src))
          (set! attrs (append attrs (list 
                                     (list 
                                      '*ID* 
                                      (string-append
                                       "*"
                                       (machine-bits->string src)))))))
      ;;
      (cons* (car src) (cons '@ attrs) rest)))
   ;;
   ((sxml:text? src)
    src)
   ;;
   ((sxml:comment? src)
    src)
   ;;
   (else
    (format #t "not munging: ~s\n" src)
    src)))
