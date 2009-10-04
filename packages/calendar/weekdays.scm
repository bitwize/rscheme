
(define (date->weekday date)
  (vector-ref '#(sunday 
		 monday 
		 tuesday 
		 wednesday 
		 thursday 
		 friday 
		 saturday)
	      (remainder (date->day date) 7)))

(define (date->week (date <date>))
  (let ((day (date->day date)))
    (values (quotient day 7)
	    (remainder day 7))))

(define (week->date (week <fixnum>) (weekday <fixnum>))
  (if (and (fixnum>=? weekday 0)
	   (fixnum<? weekday 7))
      (day->date (+ (* week 7) weekday))
      (error "weekday `~d' out of range 0..6" weekday)))


(define (weekday->day-of-week weekday)
  (case weekday
    ((sunday) 0)
    ((monday) 1)
    ((tuesday) 2)
    ((wednesday) 3)
    ((thursday) 4)
    ((friday) 5)
    ((saturday) 6)
    (else "Unknown weekday spec: ~s" weekday)))

(define $short-weekday-names '#("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
(define $short-month-names '#(#f 
                              "Jan" "Feb" "Mar" "Apr"
                              "May" "Jun" "Jul" "Aug"
                              "Sep" "Oct" "Nov" "Dec"))

(define $long-weekday-names '#("Sunday" "Monday" "Tuesday" "Wednesday" 
                               "Thursday" "Friday" "Saturday"))
(define $long-month-names '#(#f 
                              "January" "February" "March" "April"
                              "May" "June" "July" "August"
                              "September" "October" "November" "December"))

(define (short-weekday-name weekday)
  (vector-ref $short-weekday-names
              (if (fixnum? weekday)
                  weekday
                  (weekday->day-of-week weekday))))
    
(define (short-month-name month)
  (vector-ref $short-month-names month))


(define (long-weekday-name weekday)
  (vector-ref $long-weekday-names
              (if (fixnum? weekday)
                  weekday
                  (weekday->day-of-week weekday))))
    
(define (long-month-name month)
  (vector-ref $long-month-names month))

;;;
;;;  Return the Nth weekday after (or before, if N<0) the given date
;;;
;;;  For example, in the U.S., Thanksgiving falls on the 4th Thursday
;;;  of November, so you can call:
;;;
;;;       (ordinal-weekday '2004-11-01 4 4)
;;;                                    ^4=Thursday
;;;
;;;  to find that in 2004, that it is on 2004-11-25
;;;
;;;  As another example, daylight savings time ends on the last Sunday
;;;  in October, so you can call:
;;;
;;;     (ordinal-weekday '2004-10-31 0 -1)
;;;
;;;  to find that in 2004, it ends on, 2004-10-31
;;;
;;;  A trickier example would be Administrative Assistant's Day,
;;;  which is the Wednesday of the last full week of April.  Since
;;;  the last full week ends on a Saturday, this amounts to:
;;;
;;;     (date+ (ordinal-weekday '2004-04-30 6 -1) -3)
;;;
;;;  (since Wednesday is always 3 days before Saturday)
;;;

(define (ordinal-weekday (first-candidate <date>) 
                         (weekday <fixnum>) 
                         (ordinal <fixnum>))
  (assert (not (= ordinal 0)))
  ;;
  (bind ((week dow (date->week first-candidate)))
    (date+
     first-candidate
     (if (> ordinal 0)
         (+ (modulo (- weekday dow) 7) (* 7 (- ordinal 1)))
         (- (+ (modulo (- dow weekday) 7) (* 7 (- (- ordinal) 1))))))))


(define (last-day-of-month (self <date>))
  (bind ((y m d (date->ymd self)))
    (date+
     (if (= m 12)
         (ymd->date (+ y 1) 1 1)
         (ymd->date y (+ m 1) 1))
     -1)))
