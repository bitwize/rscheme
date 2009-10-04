
(define *timezones* #f)

(%early-once-only
 (set! *timezones* (make-table string-ci=? string-ci->hash))
 (for-each (lambda (a)
	     (table-insert! *timezones*
			    (car a)
			    (+ (* (quotient (cadr a) 100) 3600)
			       (* (remainder (cadr a) 100) 60))))
	   ;;
	   ;; this table is from RFC 822 for mail dates, plus "UTC"
	   ;;
	   '(("CST" -600)  ("CDT" -500)
	     ("EST" -500)  ("EDT" -400)
	     ("MST" -700)  ("MDT" -600)
	     ("PST" -800)  ("PDT" -700)
	     ("A" -100) ("B" -200) ("C" -300) ("D" -400)
	     ("E" -500) ("F" -600) ("G" -700) ("H" -800)
	     ("I" -900) ("K" -1000) ("L" -1100) ("M" -1200)
	     ("N" +100) ("O" +200) ("P" +300) ("Q" +400)
	     ("R" +500) ("S" +600) ("T" +700) ("U" +800)
	     ("V" +900) ("W" +1000) ("X" +1100) ("Y" +1200)
	     ("Z" 0)
	     ("GMT" 0)
	     ("UT" 0)
	     ("UTC" 0)
             ;;
             ;; and some more to handle PSQL time
             ("+01" +0100) ("+02" +0200) ("+03" +0300) ("+04" +0400)
             ("+05" +0500) ("+06" +0600) ("+07" +0700) ("+08" +0800)
             ("+09" +0900) ("+10" +1000) ("+11" +1100) ("+12" +1200)
             ("-01" -0100) ("-02" -0200) ("-03" -0300) ("-04" -0400)
             ("-05" -0500) ("-06" -0600) ("-07" -0700) ("-08" -0800)
             ("-09" -0900) ("-10" -1000) ("-11" -1100) ("-12" -1200))))
            

(%early-once-only
 (define splittz (reg-expr->proc '(seq (save (or #\+ #\-))
                                       (save (seq digit digit))
                                       #\:
                                       (save (seq digit digit))))))

(define (string->timezone tz)
  (or (table-lookup *timezones* tz)
      (let ((n (string->number tz)))
        (if n
            (+ (* (quotient n 100) 3600)
               (* (remainder n 100) 60))
            (bind ((s e sgn hh mm (splittz tz)))
              (if s
                  (* (if (string=? sgn "-") -1 1)
                     (+ (* 3600 (string->number hh))
                        (* 60 (string->number mm))))
                  #f))))))

(define (string->month month-name)
  (let ((m (assoc month-name
		  '(("Jan" . 1)
		    ("Feb" . 2)
		    ("Mar" . 3)
		    ("Apr" . 4)
		    ("May" . 5)
		    ("Jun" . 6)
		    ("Jul" . 7)
		    ("Aug" . 8)
		    ("Sep" . 9)
		    ("Oct" . 10)
		    ("Nov" . 11)
		    ("Dec" . 12)))))
    (if m
	(cdr m)
	(string->number month-name))))

(define *time-patterns* '())

(define (add-time-pattern! converter pattern-proc)
  (set! *time-patterns* 
	(cons (cons pattern-proc converter)
	      *time-patterns*)))

(define (iso-time year month day hh mm ss frac tz)
  (let ((t (day->time (ymd->day (string->number year)
                                (string->number month)
                                (string->number day))
                      (+
                       (if hh
                           (+ (* (string->number hh) 3600)
                              (* (string->number mm) 60))
                           0)
                       (if ss
                           (string->number ss)
                           0)
                       (if tz
                           (- (string->timezone tz))
                           0)))))
    (if frac
        (time+interval t
                       (seconds->interval
                        (string->number (string-append "0." frac))))
        t)))

(%early-once-only
 ;;
 ;; RFC 822 time
 ;;
 ;; e.g., "Thu, 16 Feb 1995 09:49:25 -0600"
 ;;
 (add-time-pattern!
       rfc-822-time
       (reg-expr->proc 
	'(entire
	  (seq (? (seq (or "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
		       #\,
		       (+ #\space)))
	       (let day (+ digit))
	       (+ #\space)
	       (let month (seq (range #\A #\Z)
			       (range #\a #\z)
			       (range #\a #\z)))
	       (+ #\space)
	       (let year (+ digit))
	       (+ #\space)
	       (let hh (+ digit))
	       #\:
	       (let mm (+ digit))
	       (? (seq #\:
		       (let ss (+ digit))))
	       (+ #\space)
	       (let tz (or (+ (range #\A #\Z))
			   ;; technically, the +/- is required
			   ;; but lets be slightly more forgiving...
			   (seq (? (or #\+ #\-))
				digit digit digit digit)))))))
 ;;
 ;; PSQL time
 ;;   e.g., "2004-04-09 17:03:38.302461-05"
 ;;     or  "2004-04-09 17:03:38-05"
 ;;
 (add-time-pattern! iso-time
                    (reg-expr->proc
                     '(entire
                       (seq
                        (save (seq digit digit digit digit))
                        #\-
                        (save (seq digit digit))
                        #\-
                        (save (seq digit digit))
                        (?
                         (seq
                          (+ space)
                          (save (seq digit digit))
                          #\:
                          (save (seq digit digit))
                          (? (seq
                              #\:
                              (save (seq digit digit))
                              (? (seq
                                  #\.
                                  (save (+ digit))))))
                          (? (save
                              (seq (or #\+ #\-)
                                   (seq (+ digit)))))))))))
 ;;
 ;; ISO time (with timezone added)
 ;;
 ;; e.g., "1995-04-28 03:45:00.000 CST"
 ;;   or, "1995-04-28 03:45:00"
 ;;
 (add-time-pattern! iso-time
                    (reg-expr->proc
                     '(entire
                       (seq
                        (save (seq digit digit digit digit))
                        #\-
                        (save (seq digit digit))
                        #\-
                        (save (seq digit digit))
                        (?
                         (seq
                          (+ space)
                          (save (seq digit digit))
                          #\:
                          (save (seq digit digit))
                          (? (seq
                              #\:
                              (save (seq digit digit))
                              (? (seq
                                  #\.
                                  (save (+ digit))))))
                          (? (seq
                              (+ space)
                              (save
                               (or (+ (range #\A #\Z))
                                   (seq (? (or #\+ #\-))
                                        digit digit digit digit)))))))))))
 ;;
 ;;  XML (SOAP/datetime?) time
 ;;  "%Y-%m-%dT%H:%M:%SZ"
 ;;  <http://www.w3.org/TR/NOTE-datetime>
 ;;  (note that the arguments are the same as ISO time, but the
;;   text is formatted slightly differently)
 ;; 
 (add-time-pattern! iso-time
                    (reg-expr->proc
                     '(entire
                       (seq
                        (save (seq digit digit digit digit))
                        #\-
                        (save (seq digit digit))
                        #\-
                        (save (seq digit digit))
                        #\T
                        (save (seq digit digit))
                        #\:
                        (save (seq digit digit))
                        (?
                         (seq
                          #\:
                          (save (seq digit digit))
                          (? (seq
                              #\. (save (+ digit))))))
                        (save (or "Z"
                                  (seq (or #\+ #\-)
                                       digit 
                                       digit
                                       (? (seq #\: digit digit)))))))))
 )

#|
-- envelopes are based on ctime(), which is very hard
-- to reverse (mktime() might do it with isdst=-1, but is it standard?)
-- because TZ info is not present
 ;;
 ;; the times I see on my envelope
 ;; e.g., "Thu Feb 16 09:49:25 1995"
 ;;
 (add-time-pattern!
  envelope-time
  (reg-expr->proc '(entire
		    (seq (or "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
			 #\space
			 (let month (seq (range #\A #\Z)
				      (range #\a #\z)
				      (range #\a #\z)))
			 #\space
			 (let day (+ digit))
			 (+ #\space)
			 (let hh digit (? digit))
			 #\:
			 (let mm (seq digit digit))
			 #\:
			 (let ss (seq digit digit))
			 #\space
			 (let yy (+ digit))))))
|#

(define (string->time str)
  (let loop ((patterns *time-patterns*))
    (if (null? patterns)
	#f
	(let (((trial <pair>) (car patterns)))
	  (bind ((s e #rest args ((car trial) str)))
	    (if s
		(let ((t (apply* args (cdr trial))))
		  (or t (loop (cdr patterns))))
		(loop (cdr patterns))))))))

(define (rfc-822-time day month year hh mm ss tz)
  (let ((month (string->month month))
	(tzdelta (string->timezone tz))
	(year (if (> (string-length year) 2)
		  (string->number year)
		  (+ 1900 (string->number year))))
	(day (string->number day)))
    (and month
	 tzdelta
	 (>= year 1969)
	 (< year 2038)
	 (day->time (date->day (ymd->date year month day))
		    (+ (* (string->number hh) 3600)
		       (* (string->number mm) 60)
		       (if ss (string->number ss) 0)
		       (- tzdelta))))))
  
