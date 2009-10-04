#|
(define-module-extend iolib ()
(define-method input-port-read ((self <input-port>) 
                                #key (location-table default: #f))
  ;; Quick hack to capture the ENDING line number of forms
  (thread-let ((*read-port* self))
    (let ((meth (find-method input-port-scan-token (list self)))
          (n (if (name self)
                 (list (name self))
                 '())))
      (read:parse-object 
       (lambda ()
         (meth self))
       (and location-table
            (lambda (datum line)
              (table-insert! location-table 
                             datum
                             (cons* (input-port-line-number self) 
                                    line
                                    n))))))))
)
|#

(define-class <xml-test-runner> (<test-runner>))

(define (xml-test-runner-create)
  (make <xml-test-runner>
        test-runner-on-test: (lambda (self info)
                               (values))))

(define-method print-test-summary ((self <xml-test-runner>))
  (result->sxml (car (group-stack self)) 
                (make-vector $num-result-types 0)))

(define-method result->sxml ((self <test-group>) summary)
  (let* ((a (make-vector $num-result-types 0))
         (body (map (lambda (sub)
                      (result->sxml sub a))
                    (reverse (results self)))))
    ;;
    (for-each (lambda (k)
                (vector-set! summary k (+ (vector-ref summary k)
                                          (vector-ref a k))))
              (range $num-result-types))
    ;;
    `(testsuite
      ,@(if (name self)
            `((title ,(name self)))
            '())
      ,@(location-as-sxml (location self))
      (summary (@ (pass ,(to-string (vector-ref a 0)))
                  (fail ,(to-string (vector-ref a 1)))
                  (xpass ,(to-string (vector-ref a 2)))
                  (xfail ,(to-string (vector-ref a 3)))
                  (skip ,(to-string (vector-ref a 4)))))
      ,@body)))

(define (location-as-sxml loc)
  (if loc
      (case (length loc)
        ((1)
         ;; only know the file...
         `((source (file ,(as-sxml (car loc))))))
        ((2)
         ;; usual form
         `((source (file ,(as-sxml (car loc)))
                   (line ,(to-string (cadr loc))))))
        ((3)
         ;; new, improved form that includes end line#
         `((source (file ,(as-sxml (car loc)))
                   (line ,(to-string (cadr loc)))
                   (lastline  ,(to-string (caddr loc)))))))
      '()))

(define-method as-sxml ((self <file-name>))
  (pathname->os-path self))

(define-method as-sxml ((self <string>))
  self)

(define-method result->sxml ((self <test-result>) (accum <vector>))
  (let ((k (case (status self)
             ((pass) 0)
             ((fail) 1)
             ((xpass) 2)
             ((xfail) 3)
             ((skip) 4))))
    (vector-set! accum k (+ 1 (vector-ref accum k))))
  ;;
  `(test
    ,@(if (name self)
          `((title ,(name self))) 
          '())
    ,@(location-as-sxml (location self))
    (result ,(to-string (status self)))))

