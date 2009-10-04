;;;
;;;  Notification engine
;;;

(define *apparently-from* "IssueBase <ibis@localhost.localdomain>")

(define (set-notify-from! sender)
  (note 2231 "Set notification from ~s" sender)
  (set! *apparently-from* sender))

(define (send-notification rcpts subj text)
  (let ((p (open-output-string)))
    (format p "From: ~a\n" *apparently-from*)
    (format p "To: ")
    (let ((subseq? #f))
      (for-each (lambda ((u <user>))
                  (if subseq? (format p ",\n     "))
                  (set! subseq? #t)
                  (if (email u)
                      (format p "~a <~a>" (fullname u) (email u))
                      (format p "(~a)" (fullname u))))
                rcpts)
      (format p "\n"))
    (format p "Subject: ~a\n" subj)
    (format p "Date: ~a\n" (time->string (time) "%a, %d %b %Y %H:%M:%S %z"))
    (format p "\n")
    (write-string p text)
    ;;
    (if (not (string=? text ""))
        (let ((out (close-output-port p)))
          (note 2290 "------------------------[Notification]-----------")
          (for-each
           (lambda (l)
             (note 2291 "~a" l))
           (string-split out #\newline))
          (note 2292 "-------------------------------------------------")
          (let ((r (select identity (map email rcpts))))
            (if (null? r)
                (note 2293 "No recipients with e-mail addresses")
                (let ((p (apply port->run "/usr/sbin/sendmail" r)))
                  (write-string p out)
                  (let ((rc (close-output-port p)))
                    (note 2294 "/usr/sbin/sendmail exited with ~s" rc)))))))))

;;;
;;;  A notification event being built up
;;;

(define-thread-var *the-notification* #f)

(define-class <notification> (<object>)
  (recipients init-value: '())
  (subject init-value: "")
  (content))
  
(define (current-notification)
  (or *the-notification*
      (error "No notification context")))

(define (with-notification thunk)
  (let ((n (make <notification>
                 content: (open-output-string))))
    (thread-let ((*the-notification* n))
      (thunk))
    (set-recipients! n (delq! (user (current-transaction))
                              (recipients n)))
    (if (and *prune-sending-notify* (null? (recipients n)))
        (if (not (string=? (subject n) ""))
            (note 2209 "No recipients for ~s" (subject n)))
        (begin
         (note 2201 "~d recipients for ~s" 
               (length (recipients n))
               (subject n))
         (send-notification (recipients n)
                            (subject n)
                            (close-output-port (content n)))))))

(define *prune-sending-notify* #f)

;;;
;;;  add someone to the distribution list
;;;

(define-method notify ((who <user>))
  (let (((n <notification>) (current-notification)))
    (if (memq who (recipients n))
        #f
        (begin
          (set-recipients! n (cons who (recipients n)))
          #t))))

;;;
;;;
;;;

(define-method notify ((on <domain>) (event <symbol>))
  ;;
  (let ((interests (cdr (or (assq event *event->interest-index*)
                            '(#f))))
        (domains (domain-heritage on))
        (new 0))
    (for-each
     (lambda ((u <user>))
       (let ((user-interests (get-property u 'interest '())))
         (if (any? (lambda (i)
                     (let ((domain-list (assq i user-interests)))
                       (if domain-list
                           (any? (lambda (d)
                                   (memq d (cdr domain-list)))
                                 domains)
                           #f)))
                   interests)
             (if (notify u)
                 (set! new (+ new 1))))))
     (value-sequence (user-index (current-information-base))))
    (note 2231 "Notify ~s on domain ~s: ~d new parties" event (name on) new)))

;;;

(define (notify-subject msg . args)
  (let ((m (apply format #f msg args)))
    (note 2241 "Subject: ~s" m)
    (set-subject! (current-notification) m)))

(define (notify-print thunk)
  (with-output-to-port (content (current-notification)) thunk))

;;;

(define *event->interest-index*
  '((item-create manager)
    (item-delete manager)
    (item-recreate manager)
    (item-assign manager)
    (position-taken manager release tester)
    (position-future manager release)
    (position-declined manager)))

(define *interests* 
  (with-module sets
    (union (apply append (map cdr *event->interest-index*))
           '())))

#|
Subject:  Defect Assigned To New Owner

Defect 3588 has been assigned to wzhao by wzhao.

Severity    4
Prefix      d
Release     papp12
Reference   
Abstract    change mode of xxml for cgi calls
Remarks:

Old Owner: rbuckman
New Owner: wzhao
---
Subject:  Defect Opened

Defect 3588 has been opened by wzhao.

Component   munger
Severity    4
Prefix      d
Release     papp12
Reference   
Abstract    change mode of xxml for cgi calls
Remarks:

change mode of xxml for cgi calls

---
Subject:  Defect Returned


Defect 3489 has been returned by gadbois with
an answer code of as_designed.

Severity    1
Prefix      d
Release     
Reference   
Abstract    Multiple mail recipients do not receive mail.
Remarks:

The mail server requires postmaster intervention to send mail with
more than 10 recipients in order to trap some types of spam, and the
postmaster was not in during the testing period.

The 1.01 release meeting members decided this limit should be
increased to 25.  This requirement is notes in defect 3489.

---
Subject:  Defect Modified

Defect 72 was modified by amos.

Severity    1
Prefix      d
Release     
Reference   
Abstract    The current planned result would be to send a message 

These changes were made:

Old Severity: 1
New Severity: 2


Additional Remarks:



|#
