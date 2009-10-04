
,(use rs.net.pserver
      rs.util.realm
      app.sourcebase
      syscalls)


;;;
;;;  For creating the test family
;;;

(define (init-testfam)
  (make-database repository: "/tmp/testps/sto"
                 database-name: "testfam"
                 su-id: "boss"
                 su-fullname: "The Boss"
                 su-email-addr: "boss@some.org"
                 su-host-user: "boss@localhost")
  ;;
  (make-filesystem "foo" (string->user "boss") (string->group "world"))
  (let ((foo (string->filesystem "foo"))
        (boss (string->user "boss"))
        (world (string->group "world")))
    ;;
    (node-lock foo '#,(path "/") boss)
    (make-directory foo '#,(path "/a") boss world '())
    (make-directory foo '#,(path "/b") boss world '())
    (directory-delta foo '#,(path "/") boss '() "Added /a and /b")
    ;;
    (node-lock foo '#,(path "/a") boss)
    (make-directory foo '#,(path "/a/x") boss world '())
    (make-directory foo '#,(path "/a/y") boss world '())
    (directory-delta foo '#,(path "/a") boss '() "Added /a/x and /a/y")
    ;;
    (node-lock foo '#,(path "/b") boss)
    (make-directory foo '#,(path "/b/z") boss world '())
    (directory-delta foo '#,(path "/b") boss '() "Added /b/z")
    ;;
    (node-lock foo '#,(path "/b/z") boss)
    (make-directory foo '#,(path "/b/z/p") boss world '())
    (directory-delta foo '#,(path "/b/z") boss '() "Added /b/z/p")
    ;;
    (for-each
     (lambda (t)
       (let* ((p (car t))
              (d (fs-parent-path p)))
         (node-lock foo d boss)
         (make-file foo p boss world
                    (string->content (cadr t))
                    '()
                    "Initial checkin"
                    (time))
         (directory-delta foo d boss '() (format #f "Added ~a" p))))
     *test-files*)
    ;;
    (commit-sourcebase)))

(define *test-files*
  '((#,(path "/a/x/one.txt")
     "1\nThis is some file content\nWhat do you think?\n")
    (#,(path "/a/x/two.txt")
     "2\nHere is some more stuff\nHow is it?\n")
    (#,(path "/a/y/three.txt")
     "3\nThis is a Q-file\n")
    (#,(path "/b/z/p/four.txt")
     "4\nThis is a Q4-file\n")
    (#,(path "/b/z/p/five.txt")
     "5\nThis is a Q5-file\n")
    (#,(path "/b/z/six.txt")
     "6\nThis is a Q6-file\n")))

;;;

(define (t)
  (database-connect "/tmp/testps/sto" 'update)
  (let ((r (make-basic-auth-realm "pserver")))
    (add-realm-user! r "donovan" "foobar" '("rsfam" "boss"))
    (start-cvs-server family: "testfam"
                      realm: r)))
