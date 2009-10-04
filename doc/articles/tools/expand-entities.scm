

;;;
(error "--- This is for the old util.sxml ---")

(define-class <xml-entity> (<object>)
  parent
  source)

(define-thread-var *xml-entity* #f)

;;; Pass 1 : Traverse entities and build up article-node tree

(define (expand-entity ref)
  (let ((q (make-dequeue))
        (e (make <xml-entity>
                 parent: *xml-entity*
                 source: (append-path (current-directory)
                                      (string->file (cadr ref))))))
    ;;
    (format #t "-- PROCESSING ~a\n" (source e))
    ;;
    (within-directory
     (file-directory (source e))
     (lambda ()
       (thread-let ((*xml-entity* e))
         (for-each-sxml-element-from-file
          (pathname->os-path (source e))
          (lambda (item)
            (dequeue-push-back! q item)
            (values))))))
    ;;
    (vector->list (dequeue-state q))))

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

(define (ID n)
  (cond
   ((assq '*ID* (sxml:attributes n))
    => cadr)
   (else 
    #f)))

(define (expand-entities src)
  (cond
   ((and (pair? src) (memq (car src) '(@ *XML* *DECL* *PI*)))
    (list src))
   ;;
   ((sxml:entityref? src)
    (expand-entity src))
   ;;
   ((sxml:document? src)
    (list (cons (car src) (apply append (map expand-entities (cdr src))))))
   ;;
   ((sxml:element? src)
    (let ((attrs (sxml:attributes src))
          (rest (apply append (map expand-entities (sxml:children src)))))
      ;;
      (set! attrs (append attrs (list (list '*SOURCE* (source *xml-entity*)))))
      ;;
      (if (table-key-present? *want-uid* (car src))
          (set! attrs (append attrs (list 
                                     (list 
                                      '*ID* 
                                      (string-append
                                       "*"
                                       (machine-bits->string src)))))))
      ;;
      (list (cons* (car src) (cons '@ attrs) rest))))
   ;;
   ((sxml:text? src)
    (list src))
   ;;
   (else
    (format #t "not munging: ~s\n" src)
    (list src))))


(define *application-entities*
  ;; these would be declared as a DOCTYPE subset in the container
  (let ((tbl (make-symbol-table)))
    ;;
    (table-insert! tbl 'ie '(foreignphrase "i.e."))
    (table-insert! tbl 'eg '(foreignphrase "e.g."))
    (table-insert! tbl 'file.bibliography '(*ENTITY* "bibliography.xml"))
    (table-insert! tbl 'file.refentries '(*ENTITY* "ref.xml"))
    (table-insert! tbl 'file.ref-open '(*ENTITY* "ref-open.xml"))
    (table-insert! tbl 'file.ref-create '(*ENTITY* "ref-create.xml"))
    tbl))

(define (load-xml-document (src <file-name>))
  (thread-let ((*xml-entity* (make <xml-entity>
                                   parent: #f
                                   source: src)))
    (with-xml-refs
     (lambda (key)
       (table-lookup *application-entities* key))
     (lambda ()
       (car (expand-entities (call-with-input-file src port->sxml)))))))

#|
(load-xml-document (string->file "persistence.xml"))
|#
