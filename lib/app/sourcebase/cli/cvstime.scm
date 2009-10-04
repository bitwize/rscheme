(define (cvs-time year month day hh mm ss frac tz)
  (day->time (ymd->day (string->number year)
                       (string->number month)
                       (string->number day))
             (if hh
                 (+ (* (string->number hh) 3600)
                    (* (string->number mm) 60)
                    (if ss
                        (string->number ss)
                        0)
                    (if frac
                        (string->number (string-append "0." frac))
                        0)
                    (if tz
                        (- (string->timezone tz))
                        0)))))

(add-time-pattern! 
 cvs-time
 (reg-expr->proc
  '(entire
    (seq
     (save (seq digit digit digit digit))
     #\/
     (save (seq digit digit))
     #\/
     (save (seq digit digit))
     (?
      (seq
       (+ (or space #\_))	; the `_' is because 'cleint/ci' is lame
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
