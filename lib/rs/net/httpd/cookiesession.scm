
(define-class <cookie-session> (<object>)
  (properties type: <vector> init-value: '#())
  start-time
  last-time
  id)

(define-method web-session-cookie ((self <cookie-session>))
  (id self))

;;;
  

(define-class <cookie-session-area> (<uri-node>)
  (protected type: <uri-node>)
  cookie-manager
  (cookie-key type: <string> init-value: "session")
  (cookie-scope type: <string> init-value: "/"))

(define (setcookie rsp (self <cookie-session-area>) cookie)
  (set-header-field! rsp
                     'set-cookie
                     (~ "~a=~a; path=~a"
                        (cookie-key self) 
                        cookie 
                        (cookie-scope self))))

(define-class <cookie-manager> (<object>) :abstract)

(define-class <incore-cookie-manager> (<cookie-manager>)
  database
  prefix)

(define-method web-session-create ((self <incore-cookie-manager>) req)
  (let* ((t (time))
         (n (make <cookie-session>
                  id: (format #f "ax.~a.~d" 
                              (prefix self)
                              (table-size (database self)))
                  start-time: t
                  last-time: t)))
    (table-insert! (database self) (id n) n)
    n))
  
(define-method web-session-access ((self <incore-cookie-manager>) req key)
  (if (table-key-present? (database self) key)
      (let ((s (table-lookup (database self) key)))
        (set-last-time! s (time))
        s)
      (let* ((t (time))
             (n (make <cookie-session>
                      id: key
                      start-time: t
                      last-time: t)))
        (table-insert! (database self) key n)
        n)))
            

(define-method write-object ((self <cookie-session>) port)
  (format port "#[<cookie-session> ~a]" (id self)))

(define-method directory? ((self <cookie-session-area>))
  (directory? (protected self)))

(define-thread-var *current-cookie-session* #f)

(define (with-cookie-reset (rsp <http-response>) 
                           (area <cookie-session-area>)
                           session-object 
                           thunk)
  (set-property! (request rsp) '%session session-object)
  (setcookie rsp area (web-session-cookie session-object))
  (thread-let ((*current-cookie-session* session-object))
    (thunk)))
  
(define-method dispatch-uri ((self <cookie-session-area>) path rsp)
  ;;
  (define (go-with session-object)
    (set-property! (request rsp) '%session session-object)
    (thread-let ((*current-cookie-session* session-object))
      (dispatch-uri (protected self) path rsp)))
  ;;
  (let ((c (get-property (request rsp) 'cookie #f))
        (mgr (cookie-manager self)))
    (cond
     ((and c (assoc (cookie-key self) (parse-request-cookies c)))
      => (lambda (p)
           (let ((s (web-session-access mgr (request rsp) (cdr p))))
             (if s
                 (go-with s)
                 ;; we don't like what they gave us; reset their cookie!
                 (let ((s (web-session-create mgr (request rsp))))
                   (setcookie rsp self (web-session-cookie s))
                   (go-with s))))))
                 
     (else
      (let ((s (web-session-create mgr (request rsp))))
        (setcookie rsp self (web-session-cookie s))
        (go-with s))))))

(define (current-cookie-session)
  *current-cookie-session*)

(define (make-cookie-session-area prot #key 
                                  (manager default: #f)
                                  (key default: #f)
                                  (scope default: #f))
  (let* ((mgr (or manager
                  (make <incore-cookie-manager>
                        database: (make-string-table)
                        prefix: (time->string (time) "%y%m%d%H%M%S"))))
         (a (make <cookie-session-area>
                  cookie-manager: mgr
                  protected: prot)))
    (if key
        (set-cookie-key! a key))
    (if scope
        (set-cookie-scope! a scope))
    a))
    

  
(define *cookie-split* (reg-expr->proc '(seq #\; (+ space))))
(define *supplied-cookie* (reg-expr->proc 
                           '(seq
                             (save (+ (not (or #\; #\, #\space #\=))))
                             #\=
                             (save (* any)))))
                                                
(define (parse-request-cookies str)
  (map (lambda (c)
         (bind ((s e n v (*supplied-cookie* c)))
           (cons n v)))
       (string-split str *cookie-split*)))
