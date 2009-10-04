
;; //user/new/post

(define (new-user-post remains query data)
  (if (and data
	   (eq? 'POST (cdr (assq 'request-type query))))
      (html
       (title (display "New User Response"))
       (header-1 (display "New User Response"))
       (format #t "~d bytes were received\n" (string-length data))
       (preformatted (print data)))))
  
;; //user/new

(define (new-user-form query)
  (html
   (title (display "New User Request"))
   (header-1 (display "New User Request"))
   (newline)
   (input-form 
    (POST "post")
    
    (header-2 (display "1. Login Id"))
    (display "Choose a login id.  This should be an alphabetic\n")
    (display "string of up to 15 characters that you will use\n")
    (display "to identify yourself to the system, and that others\n")
    (display "will know you by.\n")
    (par)
    (display "For example, my login id is ")
    (code (display "donovan"))
    (par)
    (display "system login")
    (display ": ")
    (input-field name 12)
    (submit-button 'submit "Submit New User Request")
    (par))))
