

(define (work-item-open (owner <user>)
			(based-on <change-request>)
			class
			. options)
   (let ((item (apply make-instance class owner: owner
   				 base-request: based-on
				 %alloc-area: (area-of based-on)
				 options)))
     (set-waiting-items! based-on (cons item (waiting-items based-on)))
    (work-item-state-changed item #f 'open)
     item))

(define (work-item-activate (item <work-item>))
   (kassert (not (close-audit-entry item)))
   (set-activate-audit-entry! item *audit-entry*)
   (let (((u <user>) (owner item))
         ((cr <change-request>) (base-request item)))
     (set-waiting-items! cr (delq! item (waiting-items cr)))
     (set-active-items! cr (cons item (active-items cr)))
     (set-active-items! u (cons item (active-items u)))
     (work-item-state-changed item 'open 'active)
     item))

(define (work-item-close (item <work-item>))
   (kassert (not (close-audit-entry item)))
   (kassert (activate-audit-entry item))
   ;;
   (set-close-audit-entry! item *audit-entry*)
   ;;
   (let (((u <user>) (owner item))
         ((cr <change-request>) (base-request item)))
    (set-active-items! cr (delq! item (active-items cr)))
    (set-active-items! u (delq! item (active-items u)))
    (work-item-state-changed item 'active 'closed)
    (set-history! cr (cons item (history cr)))
    (if (null? (active-items cr))
	(next-change-request-state cr item))
    item))

;;
;; work item notification hook
;;

(define (work-item-state-changed (item <work-item>) old-state new-state)
  (format #t "~s: state changed ~a => ~a\n" item old-state new-state)
  (if (eq? new-state 'active)
      (set-mail-queue! *application*
		       (cons item (mail-queue *application*)))))

;;
;; takes a list of REASONS (<change-request>'s)
;; that explain a given USER's changes to a given FS (<file-system>)
;; and turns it into the corresponding list of <fs-change>'s

(define (reasons->fs-changes (user <user>) (fs <file-system>) (reasons <list>))
   (map (lambda ((r <change-request>))
   	   (let ((c (user-fs-change user fs r)))
	     (if c
	         c
	         (work-item-activate
		   (work-item-open user r <fs-change>
		 		 file-system: fs)))))
	reasons))

(define (user-fs-change (user <user>) fs req)
;    (phash "user-fs-change.user" user)
;    (phash "user-fs-change.fs" fs)
;    (phash "user-fs-change.req" req)
  (let loop ((a (active-items user)))
    (if (null? a)
        #f
	(begin
	 (phash "user-fs-change.node" (car a))
	(if (and (eq? (base-request (car a)) req)
	         (instance? (car a) <fs-change>)
		 (eq? (file-system (car a)) fs))
            (car a)
	    (loop (cdr a)))))))
