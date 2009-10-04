;;;
;;;  See <http://www.tondering.dk/claus/calendar.html> (Calendar FAQ)
;;;  for alternative calculations in both ymd->day and day->ymd
;;;  directions

(define-syntax (julian-firsts leap?)
  (if leap?
      '#(1 1 32 61 92 122 153 183 214 245 275 306 336 367)
      '#(1 1 32 60 91 121 152 182 213 244 274 305 335 366)))


;;
;;  ymd->day
;;  convert a y/m/d date to it's calendar day number
;;
#|
146097  number of days in 400 years
{36524, 36524, 36524, 36525} number of days in each 100 years thereof
{1461, ..., 1460, ..., 
|#

;;
;;  this returns the same value as
;;    DateToNumber[{year,month,day},Gregorian]
;;  from Mathematica Package Miscellaneous/Calendar.m

(define (ymd->day (year <fixnum>) (month <fixnum>) (day <fixnum>))
  (if (and (fixnum>? month 0)
	   (fixnum<=? month 12))
      (bind ((first-day leap? (year->first-day year))
	     (firsts-vec (julian-firsts leap?))
	     (jul (fixnum+ day (vector-ref firsts-vec month))))
	(if (and (fixnum>? day 0)
		 (fixnum<=? jul (vector-ref firsts-vec (add1 month))))
	    (fixnum+ jul (fixnum- first-day 2))
	    (error "day `~d' of month `~d' is out of range 1..~d"
		   day month (- (vector-ref firsts-vec (add1 month))
				(vector-ref firsts-vec month)))))
      (error "month `~d' is out of range 1..12" month)))
;;
;;  this returns the same value as DateToNumber[{year,1,1},Gregorian]
;;

(define (year->first-day (year <fixnum>))
  (bind ((four-centuries
	  century
	  four-years
	  year
	  leap? (year-parts year)))
    (values (+ (fixnum* 146097 four-centuries)
	       (fixnum* 36524 century)
	       (fixnum* 1461 four-years)
	       (fixnum* 365 year)
	       1)
	    leap?)))

;;

(define (decompose-date (day <fixnum>))
  (let-syntax ((qm (syntax-form (num div . rest)
                     (values (fixnum-quotient num div)
                             (int-modulo num div)
                             . rest)))
               (qma (syntax-form (num div1 divrest rest1 restrest)
                      (if (< num div1)
                          (values 0 num . rest1)
                          (let ((n (fixnum- num div1)))
                            (values (add1 (fixnum-quotient n divrest))
                                    (int-modulo n divrest)
                                    . restrest))))))
    ;;
    ;; the +365 offset adjusts so that the internal date
    ;; representation, +365, will consider 2000-01-01 (=730120),
    ;; to be exactly   (* 5 146097)
    ;;
    (bind (((n <fixnum>) (fixnum+ day 365))
           ((y400 <fixnum>) (n <fixnum>) (qm n 146097))
           ((y100 <fixnum>) (n <fixnum>) (qma n 36525 36524 () ()))
           ((y4 <fixnum>) (n <fixnum>) (if (eq? y100 0)
                     (qm n 1461)
                     (qma n 1460 1461 () ())))
           ((y1 <fixnum>) (n <fixnum>) leap? (if (and (not (eq? y100 0))
                                                      (eq? y4 0))
                                                 ;; it's a four-year block 
                                                 ;; that *does not* include
                                                 ;; a leap year
                                                 (qm n 365 #f)
                                                 (qma n 366 365 (#t) (#f)))))
      (values (fixnum+ (fixnum+ (fixnum* 400 y400)
                                (fixnum* 100 y100))
                       (fixnum+ (fixnum* 4 y4)
                                y1))
              (add1 n)
              leap?))))

(define (day->ymd day)
  (bind ((yy julian leap? (decompose-date day))
         (mm dd (my-month-quotient julian leap?)))
    (values yy mm dd)))
