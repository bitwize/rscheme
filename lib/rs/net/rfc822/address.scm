(define-class <rfc822-address> (<object>)
  hash-code
  user-name
  domain)

(define-method equal? ((a <rfc822-address>) b)
  (and (instance? b <rfc822-address>)
       (eq? (hash-code a) (hash-code b))
       (string-ci=? (user-name a) (user-name b))
       (string-ci=? (string-join #\. (domain a))
                    (string-join #\. (domain b)))))

(define-class <rfc822-error> (<condition>)
  address
  message)

(define-method display-object ((self <rfc822-error>) port)
  (format port "~s: ~a\n"
          (address self)
          (message self)))

(define-method to-string ((self <rfc822-address>))
  (string-append (user-name self)
                 "@"
                 (string-join #\. (domain self))))

(define-method domain-name ((self <rfc822-address>))
  (string-join #\. (domain self)))

#|
(define-method write-object ((self <rfc822-address>) port)
  (format port "#[~a ~a]" (object-class self) (to-string self)))
|#

(define (parse-email-address addr)
  (handler-case
   (bind ((v (parse-using-grammar (string->list addr)
                                  *email-address-grammar*)))
     (if v
         (let ((user (cadr (assq 'user (car v))))
               (domain (cdr (assq 'domain (car v)))))
           ;;
           (if (every? string? domain)
               (if (< (length domain) 2)
                   (signal
                    (make <rfc822-error>
                          address: addr
                          message: "Invalid use of top-level domain"))
                   (make <rfc822-address>
                         hash-code: (with-module
                                        primops
                                      (tuple->hash
                                       (string-ci->hash user)
                                       (string-ci->hash
                                        (string-join #\. domain))))
                         user-name: user
                         domain: domain))
               (signal
                (make <rfc822-error>
                      address: addr
                      message: "Unsupported domain notation"))))
         (signal (make <rfc822-error>
                       address: addr
                       message: "Bad address syntax"))))
   ((<rfc822-error> condition: e)
    (signal e))
   ((<condition>)
    (signal (make <rfc822-error>
                  address: addr
                  message: "Bad address syntax")))))


(define *email-address-grammar*
  (grammar
   ;;
   (start (x:mailbox) x)
   ;;
   (terminal #\" #\@ #\. #\[ #\] #\\)
   (charset DIGIT (#\0 #\9))
   ;; technically, CHAR should be:  (charset CHAR (#\x00 #\x7F))
   ;; but we don't want crazy stuff going on here...
   (charset CHAR (#\space #\~))
   ;;
   (charset QTEXT CHAR - #\" #\cr)
   (charset DTEXT CHAR - #\[ #\] #\\ #\cr)
   ;;
   (charset SPECIALS
            #\( #\) #\< #\> #\@ 
            #\, #\; #\: #\\ #\"
            #\. #\[ #\])
   ;;
   (charset CTLS (#\x00 #\x1F) #\x7F)
   (charset ATOM CHAR - SPECIALS #\space CTLS)
   (atom ((t: (+ ATOM))) 
         (list->string t))
   ;;
   (quoted-string (#\" (l: (* QTEXT)) #\") 
                  (list->string l))
   ;;
   (word (x:atom) x)
   (word (x:quoted-string) x)
   ;;
   (mailbox (x:addr-spec) x)
   (addr-spec (u:local-part #\@ d:domain) (list u d))
   (local-part (first:word (rest: (* #\. word)))
               (list 'user (string-join #\. (cons first (map cadr rest)))))
   ;;
   (domain (first:sub-domain (rest: (* #\. sub-domain)))
           (cons* 'domain first (map cadr rest)))
   (sub-domain (domain-ref))
   (sub-domain (domain-literal))
   ;;
   (domain-ref (atom))
   (domain-literal (#\[ (a: (* (or DTEXT quoted-pair))) #\])
                   (list 'literal (list->string a)))
   ;;
   (quoted-pair (#\\ x:CHAR) x)
   ))


(define (valid-email-address? str)
  (handler-case
   (begin
     (parse-email-address str)
     #t)
   ((<condition> condition: c)
    #f)))

