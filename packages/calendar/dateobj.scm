;;

(define-class <date> (<object>) :immob)

(define-method write-object ((self <date>) port)
  (write-string port (date->string self)))

(define-method to-string ((self <date>))
  (date->string self))

(define (ymd->date y m d)
  (day->date (ymd->day y m d)))

(define (date->ymd (date <date>))
  (day->ymd (date->day date)))

(define (date->string (self <date>))
  (bind ((year month day (day->ymd (date->day self))))
    (string-append (number->string year)
		   "-"
		   (if (fixnum<? month 10)
		       "0"
		       "")
		   (number->string month)
		   "-"
		   (if (fixnum<? day 10)
		       "0"
		       "")
		   (number->string day))))


;; idea:
;;
;; <immob>
;;    |
;;   <date>  (secondary tag DATE_TAG)
;;
;;  27 bits can handle from 1.1.1 to 367476.08.27
;;



;;  operations on <date> objects

(define (date->day (d <date>))
  (get-immob-value d))

(define (day->date (day <integer>))
  (make-immob 7 day))

(define (date+ (a <date>) (b <integer>))
  (make-immob 7 (fixnum+ (get-immob-value a) b)))

(define (date- (a <date>) b)
  (if (integer? b)
      (make-immob 7 (fixnum- (get-immob-value a) b))
      (if (instance? b <date>)
	  (fixnum- (get-immob-value a) (get-immob-value b))
	  (error "~s: invalid args" (list 'date- a b)))))

(define (date=? (a <date>) (b <date>)) (eq? a b))
(define (date>? (a <date>) (b <date>)) (fixnum>? (date->day a) (date->day b)))
(define (date>=? (a <date>) (b <date>))(fixnum>=? (date->day a) (date->day b)))
(define (date<? (a <date>) (b <date>)) (fixnum<? (date->day a) (date->day b)))
(define (date<=? (a <date>) (b <date>))(fixnum<=? (date->day a) (date->day b)))

;;

(define *date-patterns* '())

(define (date-pattern str)
  (let loop ((p *date-patterns*))
    (if (null? p)
        (values)
        (bind ((s e #rest r ((caar p) str)))
          (if s
              (bind ((y m d (if (cdar p)
                                (apply (cdar p) r)
                                (list->values r))))
                (if y
                    (values y m d)
                    (loop (cdr p))))
              (loop (cdr p)))))))
  
(define (string->date (n <string>))
  (bind ((yy mm dd (date-pattern n)))
    (if yy
	(make-immob 7 (ymd->day (if (string? yy) (string->number yy) yy)
                                (if (string? mm) (string->number mm) mm)
                                (if (string? dd) (string->number dd) dd)))
	#f)))

(define (date->time (date <date>) (time <fixnum>))
  (day->time (date->day date) time))

(%early-once-only
  (rscheme-global-set! 26 <date>)
  (if (null? *date-patterns*)
      (set! *date-patterns*
            (list
             (cons (reg-expr->proc     ; ISO format 2002-04-13
                    '(entire (seq (let year (+ digit))
                                  #\-
                                  (let month (+ digit))
                                  #\-
                                  (let day (+ digit)))))
                   #f)
             (cons (reg-expr->proc     ; old RScheme format 2002.04.13
                    '(entire (seq (let year (+ digit))
                                  #\.
                                  (let month (+ digit))
                                  #\.
                                  (let day (+ digit)))))
                   #f)
             (cons (reg-expr->proc     ; American format 4/13/02
                    '(entire (seq (let month (+ digit))
                                  #\/
                                  (let day (+ digit))
                                  #\/
                                  (let year (+ digit)))))
                   (lambda (m d y)
                     (let ((y (string->number y)))
                       (values (if (< y 50)
                                   (+ y 2000)
                                   (+ y 1900))
                               m
                               d)))))))
  (add-alternate-number-parser! string->date))

;;;

(define (today)
  (string->date (time->string (time) "%Y-%m-%d")))
