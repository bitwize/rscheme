
;;;
;;;   A message is comprised of various flags, fields, and sections,
;;;   including a question section.  The first question
;;;   is the "query"
;;;

;;;  NOTE that this procedure can probably be merged
;;;       with `handle-request' in caching-nameserver.scm
;;;

(define (local-inquiry (msg <message>))
  (let ((query (car (question-section msg))))
    ;;
    (set-qr! msg #t)
    (set-aa! msg #f)
    (set-tc! msg #f)
    (set-ra! msg #t)
    ;;
    (cond
     ((eq? 'NX (cache-get query))
      (dbg "Query matches an NXDOMAIN entry in cache~%")
      (set-rcode! msg 'NAME-ERROR)
      (set-authority-section! msg '())
      (set-additional-section! msg '())
      msg)
     ;;
     ((answers-from-cache query) =>
      (lambda (answers)
        (dbg "Sending answers from cache~%")
        (if-debug (print answers))
        ;;
        ;; not allows to make zone queries internally
        (assert (not (instance? answers <zone>)))
        ;;
        (set-rcode! msg 'NO-ERROR)
        (set-answer-section! msg answers)
        msg))
     ;;
     (else
      ;; Try internet servers
      (dbg "Trying Internet nameservers...~%")

      (let* ((nameservers (find-closest-nameservers (name query)))
             (results     (answers-from-servers nameservers query)))

        (cond
         ;;
         ((not (message? results))

          (dbg "Sending answer from Internet servers...~%")

          ;; Not a message, so we got an answer.
          
          (set-rcode! msg 'NO-ERROR)
          (set-answer-section!     msg results)
          (set-authority-section!  msg '())
          (set-additional-section! msg '())

          msg)

         ((message-nxdomain? results)

          (dbg "Received NXDOMAIN response...~%")
          
          (set-rcode! msg 'NAME-ERROR)
          (set-answer-section! msg '())
          (set-authority-section! msg '())
          (set-additional-section! msg '())
          
          msg)
         
         (else ;; No answer - weird. Just return...

          (dbg "No records match query...~%")

          (set-rcode! msg 'NO-ERROR)
          (set-answer-section! msg '())
          (set-authority-section! msg '())
          (set-additional-section! msg '())

          msg)))))))

(define (dns-query #key 
                   (domain type: <string> parameter: the-domain)
                   (type type: <symbol> default: 'A parameter: the-type)
                   (class type: <symbol> default: 'IN parameter: the-class))
  (let* ((q (make <query>
                  name: the-domain
                  type: the-type
                  class: the-class))
         (m (local-inquiry
             (make <message>
                   id: 12345
                   qr: #f
                   opcode: 'QUERY
                   aa: #f
                   tc: #f
                   rd: #t
                   ra: #f
                   z: 0
                   rcode: 'NO-ERROR
                   question-section: (list q)
                   answer-section: '()
                   authority-section: '()
                   additional-section: '()))))
    (if (instance? m <message>)
        (case (rcode m)
          ((NAME-ERROR) #f)
          ((NO-ERROR)
           ;; (1) will there ever be an answer in the list that
           ;;     is not referenced by a CNAME?
           ;; (2) will there ever be an answer to a different
           ;;     question?
           ;; if either answer to the above is YES, then this
           ;; implementation is broken...
           (select
            (lambda (rr)
              (and (eq? (type rr) the-type)
                   (eq? (class rr) the-class)))
            (answer-section m))))
        ;(rr-filter q (answer-section m))
        #f)))
