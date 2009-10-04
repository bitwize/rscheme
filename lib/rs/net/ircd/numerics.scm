(define-macro (define-numeric name . frags)
  (let* ((p (parse-format-string
             (string-append (string-join "" frags) "\r\n")))
         (args (with-module
                   sets
                 (union (map (lambda (x)
                               (string->symbol (vector-ref x 9)))
                             (select vector? p))
                        '()))))
    ;;
    `(define-macro (,name %port #key ,@args)
       (,'quasiquote
        (writev+ 
         ,(list 'unquote '%port)
         (vector
          ,@(map (lambda (x)
                   (if (string? x)
                       x
                       (list 'unquote (string->symbol (vector-ref x 9)))))
                 p)))))))

(define-numeric RPL_WHOREPLY
  ":~{me}a 352 ~{you}a ~{channel}a "
  "~{username}a ~{hostname}a ~{servername}a ~{nickname}a "
  "~{status}a :~{hopcount}d ~{realname}a")

(define-numeric RPL_ENDOFWHO
  ":~{me}a 315 ~{name}a ~{mask}a :End of /WHO list.")


(define-numeric RPL_MOTDSTART
  ":~{me}a 375 :- ~{me}a Message of the Day -")

(define-numeric RPL_ENDOFMOTD
  ":~{me}a 376 :End of /MOTD command.")

(define-numeric RPL_MOTD
  ":~{me}a 372 :- ~{line}a")

(define-numeric ERR_NICKCOLLISION
  ":~{me}a 436 ~{nickname}a :Nickname collision KILL")

(define-numeric RPL_NAMREPLY
  ":~{me}a 353 ~{nickname}a ~{pub-or-secret}a ~{channel}a :~{members}a")

(define-numeric RPL_ENDOFNAMES
  ":~{me}a 366 ~{nickname}a ~{channel}a :End of /NAMES list.")

(define-numeric MODE:SET
  ":~{me}a MODE ~{subject}a +~{set}a")
