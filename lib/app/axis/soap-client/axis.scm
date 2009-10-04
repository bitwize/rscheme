(define-macro (define-axis-soap-stub name args req rsp)
  `(define (,name ,@(map (lambda (a)
                           (list (car a) (cadr a)))
                         args))
     (let ((r (soap-rpc (list ',req
                              ,@(map (lambda (a)
                                       (let ((enc (cond
                                                   ((memq 'encode-with: a)
                                                    => cadr)
                                                   (else 'soap-encode))))
                                         `(cons
                                           ',(symbol-append "axis:" (car a))
                                           (,enc ,(car a)))))
                                     args)))))
       (if (eq? (car r) ',rsp)
           (apply ,(symbol-append rsp "-decode")
                  (select sxml:element? (sxml:children r)))
           (error "Expected ~s reply, got ~s"
                  ',rsp
                  (car r))))))

(define (axis:connect user #optional password)  ; password needed if `user' isn't the GUEST user
  ;;
  (let* ((d (soap-rpc `(axis:PreAuthenticateRequest (xsd:string ,user))))
         (usalt (xpath-str d "axis:usersalt"))
         (ssalt (xpath-str d "axis:sessionsalt"))
         (csalt (md5-digest (~ "~d_~a...~a~a"
                               (getpid)
                               (machine-bits->string (bvec->hash (time) 0 8))
                               (random)
                               (machine-bits->string d))))
         (tmp (and password (md5-digest (~ "~a|~a" usalt password))))
         (h (and password
                 (md5-digest (string-join #\| (list csalt ssalt user tmp))))))
    ;;
    (soap-rpc `(axis:AuthenticateRequest 
                (xsd:string ,user)
                (xsd:string ,csalt)
                (xsd:string ,(or h ""))))))

(define (axis:whoami)
  (let ((r (soap-rpc `(axis:WhoAmIRequest))))
    (values (delay (xpath-str r "axis:user"))
            (delay (xpath-str r "axis:fullname")))))

(define-class <axis-Path> (<object>)
  steps)

(define-method equal? ((a <axis-Path>) b)
  (and (instance? b <axis-Path>)
       (equal? (steps a) (steps b))))
  
  
(define-method hash-code ((self <axis-Path>))
  (hash-code (steps self)))

(define-method soap-encode ((self <object>))
  (list (soap-encode-value self)))

(define-method soap-encode ((self <axis-Path>))
  (map (lambda ((s <string>))
         `(axis:step (enc:string ,s)))
       (steps self)))

(define-class <axis-FileRef> (<object>)
  fileset
  snapshot
  (path type: <axis-Path>))

(define-method equal? ((a <axis-FileRef>) b)
  (and (instance? b <axis-FileRef>)
       (string=? (fileset a) (fileset b))
       (string=? (snapshot a) (snapshot b))
       (equal? (path a) (path b))))

(define-method hash-code ((self <axis-FileRef>))
  (tuple->hash
   (string->hash (fileset self))
   (tuple->hash
    (string->hash (snapshot self))
    (hash-code (path self)))))

(define-method soap-encode ((self <axis-FileRef>))
  `((axis:fileset ,(fileset self))
    (axis:snapshot ,(snapshot self))
    (axis:path ,@(soap-encode (path self)))))

(define (soap-decode-Path node)
  (make <axis-Path>
        steps: (map xpath:node->string (xpath () node "axis:step"))))

(define (soap-decode-FileRef node)
  (make <axis-FileRef>
        fileset: (xpath-str-unique node "axis:fileset")
        snapshot: (xpath-str-unique node "axis:snapshot")
        path: (soap-decode-Path (xpath-unique () node "axis:path"))))
        

(define (axis:GetHistoryReply-decode file currentversion history)
  (values file
          (xpath:node->string currentversion)
          (map (lambda (e)
                 (vector 
                  (xpath-str e "axis:version")
                  (xpath-str e "axis:comment")
                  (xpath-str e "axis:author")
                  (xpath-str e "axis:mtime")
                  (map (lambda (r)
                         (list (xpath-str r "axis:name")
                               (xpath-str r "axis:title")))
                       (xpath () e "axis:reasons/axis:cr"))))
               (xpath () history "axis:entry"))))

     
(define-axis-soap-stub axis:GetHistory ((file <axis-FileRef>))
  axis:GetHistoryRequest
  axis:GetHistoryReply)



(define (axis:RepositoryTimestampReply-decode x y)
  (values (string->number (xpath:node->string x))
          (xpath:node->string y)))

(define-axis-soap-stub axis:RepositoryTimestamp ()
  axis:RepositoryTimestampRequest
  axis:RepositoryTimestampReply)

(define (axis:SnapshotDifferenceReply-decode difflist reasons)
  (values (map (lambda (d)
                 (list (map xpath:node->string (xpath () d "axis:path/axis:step"))
                       (string-ref (xpath-str d "axis:nodetype") 0)
                       (string->symbol (xpath-str d "axis:difftype"))
                       (xpath-str d "axis:newversion")
                       (xpath-str d "axis:oldversion")
                       (string->number (xpath-str d "axis:nodeid"))))
               (xpath () difflist "axis:diff"))
          (map (lambda (cr)
                 (list (xpath-str cr "axis:name")
                       (xpath-str cr "axis:title")))
               (xpath () reasons "axis:cr"))))

(define-axis-soap-stub axis:SnapshotDifference ((fileset <string>)
                                                (fromsnap <string>)
                                                (tosnap <string>))
  axis:SnapshotDifferenceRequest
  axis:SnapshotDifferenceReply)


(define (axis:CreateTagSnapshotReply-decode)
  (values))

(define-axis-soap-stub axis:CreateTagSnapshot ((fileset <string>)
                                               (snapshot <string>)
                                               (basis <string>))
  axis:CreateTagSnapshotRequest
  axis:CreateTagSnapshotReply)

(define (axis:ConfigureKeywordBySnapshotReply-decode)
  (values))

(define-axis-soap-stub axis:ConfigureKeywordBySnapshot ((fileset <string>)
                                                        (snapshot <string>)
                                                        (keyword <string>)
                                                        (value <string>))
  axis:ConfigureKeywordBySnapshotRequest
  axis:ConfigureKeywordBySnapshotReply)

(define (axis:ChangeRequestLookupReply-decode title)
  (values
   (xpath:node->string title)))

(define-axis-soap-stub axis:ChangeRequestLookup ((cr <string>))
  axis:ChangeRequestLookupRequest
  axis:ChangeRequestLookupReply)

(define (axis:ListDirectoryReply-decode file history content)
  (values
   (map xpath:node->string (xpath () content "axis:item"))
   (let ((e (xpath-unique () history "axis:entry")))
     (vector
      (xpath-str-unique e "axis:version")
      (xpath-str-unique e "axis:comment")
      (xpath-str-unique e "axis:author")
      (xpath-str-unique e "axis:mtime")
      (map (lambda (r)
             (list (xpath-str-unique r "axis:name")
                   (xpath-str-unique r "axis:title")))
           (xpath () e "axis:reasons/axis:cr"))))
   (soap-decode-FileRef file)))


(define (axis:RetrieveReply-decode file history content)
  (values
   (base64-gzip-decode (xpath:node->string content))
   (let ((e (xpath-unique () history "axis:entry")))
     (vector
      (xpath-str-unique e "axis:version")
      (xpath-str-unique e "axis:comment")
      (xpath-str-unique e "axis:author")
      (xpath-str-unique e "axis:mtime")
      (map (lambda (r)
             (list (xpath-str-unique r "axis:name")
                   (xpath-str-unique r "axis:title")))
           (xpath () e "axis:reasons/axis:cr"))))
   (soap-decode-FileRef file)))



(define-axis-soap-stub axis:Retrieve ((file <axis-FileRef>))
  axis:RetrieveRequest
  axis:RetrieveReply)

(define-axis-soap-stub axis:ListDirectory ((file <axis-FileRef>))
  axis:ListDirectoryRequest
  axis:ListDirectoryReply)


(define (cenc str)
  (list (base64-gzip-encode str)))

(define (yenc lst)
  (map (lambda ((item <string>))
         `(axis:cr (axis:name ,item)))
       lst))

(define (axis:UpdateReply-decode ver) 
  (cadr (soap-decode-value ver)))

(define (axis:CreateFileReply-decode) 
  (values))

(define (axis:CreateDirectoryReply-decode)
  (values))

(define-axis-soap-stub axis:Update ((file <axis-FileRef>)
                                    (baseversion <string>)
                                    (mode <integer>)
                                    (reasons <list> encode-with: yenc)
                                    (comment <string>)
                                    (mimetype <string>)
                                    (kwdstyle <string>)
                                    (content <string> encode-with: cenc))
  axis:UpdateRequest
  axis:UpdateReply)

(define-axis-soap-stub axis:CreateFile ((file <axis-FileRef>)
                                        (domain <string>)
                                        (mode <integer>)
                                        (reasons <list> encode-with: yenc)
                                        (comment <string>)
                                        (mimetype <string>)
                                        (kwdstyle <string>)
                                        (content <string> encode-with: cenc))
  axis:CreateFileRequest
  axis:CreateFileReply)

(define-axis-soap-stub axis:CreateDirectory ((file <axis-FileRef>)
                                             (domain <string>)
                                             (mode <integer>)
                                             (reasons <list> encode-with: yenc)
                                             (comment <string>))
  axis:CreateDirectoryRequest
  axis:CreateDirectoryReply)



#|
(set! *soap-session* (open-soap-session (t0)))
;; test...
(axis:connect "alice" "foo")
(axis:SnapshotDifference "os1" "A01-2004-02-17:1" "HEAD")

;; rscheme...
(axis:connect "guest")
(axis:SnapshotDifference "rs-0.7" "v0.7.3.3-b21u" "3.3")

|#
