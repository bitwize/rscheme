,(use regex tables paths)

;;;
;;;  read Element Structure Information Set in nsgmls output notation
;;;

(define-class <sgml-entity-decl> (<object>)
  origin
  name
  type
  subtype
  f-record
  s-record)

;;; Obtained 4.0 stuff from:
;;;    http://www.w3.org/TR/REC-html40/sgml/intro.html


(define *html-cat-file* (append-path (current-directory) 
                                     (string->file "4.0/HTML4.cat")))
(define *html-decl-file* (append-path (current-directory) 
                                      (string->file "4.0/HTML4.decl")))

(define (chunked-copy-between-ports src dst #optional visit)
  (let loop ((n 0))
    (let ((chunk (input-port-read-max src 8192)))
      (if (eof-object? chunk)
          n
          (begin
            (write-string dst chunk)
            (if visit (visit chunk))
            (loop (+ n (string-length chunk))))))))

(define (html-port->sxml (src <input-port>) name)
  (bind ((out in proc (port->run->port "nsgmls" "-m" 
                                       (pathname->os-path *html-cat-file*)
                                       (pathname->os-path *html-decl-file*)
                                       "-")))
    (thread-resume
     (make-thread
      (lambda ()
        (chunked-copy-between-ports src out)
        (flush-output-port out)
        (close-output-port out))
      "nsgmls-fill"))
    ;;
    (let ((data (read-sgml-element in name)))
      (check-exit-status proc)
      data)))

(define (load-html file name)
  (let ((p (open-input-process (~ "nsgmls -m html.cat html.decl ~a" file))))
    (let ((x (read-sgml-element p name)))
      (close-input-port p)
      x)))

(define (add-content! node item)
  (set-cdr! (cdr node) (cons item (cddr node))))


(define (read-sgml-element port name)
  (let ((entity-dict (make-string-table))
        (finfo #f)
        (sinfo #f)
        (apending '())
        (id-ix (make-string-table)))
    ;;
    (let loop ((stack `((*TOP* (@ (src ,name)))))
               (location #f))
      (let ((l (read-line port)))
        (if (string? l)
            (case (string-ref l 0)
              ((#\A) 
               (if (not (implied-attribute l))
                   (set! apending (cons l apending)))
               (loop stack location))
              ((#\() (let* ((name (downcased-symbol (substring l 1)))
                            (q (make-dequeue)))
                       ;;
                       (for-each
                        (lambda (p)
                          (parse-attribute q p))
                        apending)
                       (set! apending '())
                       ;;
                       (loop (cons (list name
                                         ;; not well-formed SXML yet; we'll
                                         ;; fix it up in the ")" handler
                                         (vector->list (dequeue-state q)))
                                   stack)
                             location)))
              ((#\)) 
               (let ((name (downcased-symbol (substring l 1)))
                     (top (car stack)))
                 (assert (eq? name (car top)))
                 ;;
                 (let ((e (if (null? (cadr top))
                              (cons (car top) (reverse! (cddr top)))
                              (cons* (car top)
                                     (cons '@ (cadr top))
                                     (reverse! (cddr top))))))
                   (add-content! (cadr stack) e)
                   (loop (cdr stack) location))))
#|
              ((#\L) (let ((x (string-split (substring l 1) #\space)))
                       (if (pair? (cdr x))
                           (loop stack (list (string->number (car x)) (cadr x)))
                           (loop stack (cons (string->number (car x)) (cdr location))))))
              |#
              ((#\-) 
               (add-content! (car stack) (parse-sgml-value (substring l 1)))
               (loop stack location))
              #|
              ((#\?)
               (add-content! (car stack)
                             (make <sgml-pi>
                                   parent: (car stack)
                                   origin: location
                                   content: (substring l 1)))
               (loop stack location))
               |#
              ((#\N #\p)          ; these have something to do with notation decls
               (loop stack location))
              ((#\f)
               (set! finfo (substring l 1))
               (loop stack location))
              ((#\s)
               (set! sinfo (substring l 1))
               (loop stack location))
              ((#\E)
               (bind ((name type subtype (list->values (string-split
                                                        (substring l 1)
                                                        #\space))))
                 (table-insert! entity-dict
                                name
                                (make <sgml-entity-decl>
                                      parent: #f
                                      origin: location
                                      name: name
                                      type: type
                                      subtype: subtype
                                      f-record: finfo
                                      s-record: sinfo))
                 (set! sinfo #f)
                 (set! finfo #f)
                 (loop stack location)))
              ((#\C)
               (assert (null? (cdr stack)))
               (car stack))
              (else
               (error "What? ~s" l)))
            (if (pair? (cdr stack))
                (error "Too much left on stack: ~s" stack)
                (car stack)))))))

(define (downcased-symbol str)
  (string->symbol (list->string (map char-downcase (string->list str)))))

(define implied-attribute (reg-expr->proc '(seq #\A
                                                (save (+ (or alpha #\-)))
                                                #\space
                                                "IMPLIED")))

(define specified-attribute (reg-expr->proc '(seq #\A
                                                  (save (+ (or alpha #\-)))
                                                  #\space
                                                  (save (or "TOKEN"
                                                            "CDATA"
                                                            "NOTATION"
                                                            "ENTITY"))
                                                  #\space
                                                  (save (* any)))))

(define (parse-attribute q l)
  (bind ((s e name (implied-attribute l)))
    (if s
        (values)
        (bind ((s e name mode value (specified-attribute l)))
          (if s
              (let ((atype (cond
                            ((string=? mode "TOKEN") 'token)
                            ((string=? mode "CDATA") 'cdata)
                            ((string=? mode "NOTATION") 'notation)
                            ((string=? mode "ENTITY") 'entity)
                            (else (error "unknown mode ~s" mode)))))
                (dequeue-push-back! q (list (downcased-symbol name) value))
                (values))
              (error "cannot parse attribute line ~s" l))))))

(define (parse-sgml-value str)
  (let loop ((i 0)
             (prev '()))
    (let ((k (string-search str #\\ i)))
      (if k
          (let ((f (string-ref str (+ k 1))))
            (case f
              ((#\n)
               (loop (+ k 2) (cons* "\n" (substring str i k) prev)))
              ((#\\)
               (loop (+ k 2) (cons* "\\" (substring str i k) prev)))
              ((#\|)
               (bind ((s e label (internal-sdata str k)))
                 (loop e (cons* (cond
                                 ((string=? label "amp") "&")
                                 ((string=? label "lt") "<")
                                 ((string=? label "gt") ">")
                                 ((string=? label "quot") "\"")
                                 ((string=? label "rArr") "\210")
                                 ((string=? label "ndash") "\261")
                                 ((string=? label "mdash") "\320")
                                 (else
                                  (error "unknown internal SDATA entity &~a;" label)))
                                (substring str i k) 
                                prev))))
              ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
               (loop (+ k 4)
                     (cons* (string (integer->char
                                     (string->number
                                      (substring str (+ k 1) (+ k 4))
                                      8)))
                            (substring str i k)
                            prev)))
              ((#\# #\%)
               (error "unhandled escape sequence at ~s" (substring str k)))
              (else
               (error "unknown escape sequence at ~s" (substring str k)))))
          (if (null? prev)
              str
              (string-join "" (reverse! (cons (substring str i) prev))))))))

(define internal-sdata (reg-expr->proc 
                        '(seq #\\
                              #\|
                              #\[
                              (save (+ (not space)))
                              (* space)
                              #\]
                              #\\
                              #\|)))
