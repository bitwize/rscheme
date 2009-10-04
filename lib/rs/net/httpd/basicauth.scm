
(define-class <uri-auth-area> (<uri-node>)
  (realm type: <auth-realm>)
  (protected type: <uri-node>))

(define-method directory? ((self <uri-auth-area>))
  (directory? (protected self)))

(define (auth-failure realm rsp type)
  (set-result! rsp 401 type)
  (set-header-field! rsp 'content-type "text/html")
  (set-content! rsp (format #f "<html><body>~a</body></html>" type))
  (set-header-field! rsp 'www-authenticate 
                     (format #f "Basic realm=\"~a\"" (name realm))))

(define parse-basic-authorization
  (reg-expr->proc '(seq "Basic" (+ space) (save (* (not space))))))

(define-method dispatch-uri ((self <uri-auth-area>) path rsp)
  (check-authentication (realm self)
                        rsp
                        (lambda ()
                          (dispatch-uri (protected self) path rsp))))

(define (make-auth-area (realm <auth-realm>) protected)
  (make <uri-auth-area>
        realm: realm
        protected: protected))

;;;

(define (check-authentication (realm <auth-realm>) 
                              (rsp <http-response>)
                              (pass-thunk <function>))
  (let ((a (get-property (request rsp) 'authorization #f)))
    (if a
        (bind ((s e info (parse-basic-authorization a))
               (auth (if info
                         (string-split (pem-decode info) #\:)
                         '())))
          (note* 408 "authorization ~s" auth)
          (if (not (= (length auth) 2))
              (generate-error-response/client-error
               rsp
               "Erroneous basic authorization encoding")
              (let ((token (authenticate realm
                                         (car auth)
                                         (cadr auth))))
                (if token
                    (begin
                      (set-property! (request rsp) '%user token)
                      (pass-thunk))
                    (auth-failure realm
                                  rsp
                                  "Authentication failed")))))
        (auth-failure realm
                      rsp
                      "Authentication required"))))

                              
