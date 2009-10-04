
(define-class <email-archive> (<object>)
  (tail-offset type: <fixnum> init-value: 0)
  (file-name type: <string>))

(define-class <email-message> (<document>) :abstract
  (subject-line #|type: (union <string> #f)|# init-value: #f)
  (time-sent #|type: (union <time> #f)|# init-value: #f))

(define-class <extern-email-message> (<email-message>)
  (archive type: <email-archive>)
  (msg-offset type: <fixnum>)
  (msg-length type: <fixnum>))

(define-class <inline-email-message> (<email-message>)
  (content type: <vector>))

;;;

(define *open-stores* (make-string-table))

(define-method get-repository ((self <email-archive>))
  (or (table-lookup *open-stores* (file-name self))
      (let ((l (open-document-index (file-name self))))
	(table-insert! *open-stores* (file-name self) l)
	l)))

(define-method content ((self <extern-email-message>))
  (lookup (get-repository (archive self))
          (document-id self)))

;;;

(define-method display-object ((self <email-message>) port)
  (format port "[~d]" (id self))
  (if (time-sent self)
      (format port " (~a)" (time->string (time-sent self) "%m-%d")))
  (if (subject-line self)
      (format port " ~#*@50a" (subject-line self))))

;;

(define-method render ((self <email-message>) #optional port)
  (let ((port (or port (current-output-port))))
    ;(format #t "|------ ~a ------|\n" self)
    (vector-for-each
     (lambda (l)
       (format #t "~a\n" l))
     (content self))))

  

