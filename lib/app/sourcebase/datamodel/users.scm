
(define-class <user> (<object>)
   (name type: <string>)
   (id type: <integer>)		;; this id is for "tar" and VFS's
   (full-name type: <string>)
   (email-addr type: <string>)
   (remote-hosts type: <list> init-value: '())
   
    ;; assq list group => list of <authority-domain>
   (authorities type: <list> init-value: '())   
   (properties type: <list> init-value: '())
   (audit-log type: <list> init-value: '())
   (active-items type: <list> init-value: '())
   (check-outs type: <list> init-value: '()))

(define (super-user? (u <user>))
   (if (assq 'super-user (properties u))
       #t
       #f))

(define-method write-object ((self <user>) port)
   (format port "#[<user> ~a]" (name self)))

(define-method display-object ((self <user>) port)
  (write-string port (name self)))
