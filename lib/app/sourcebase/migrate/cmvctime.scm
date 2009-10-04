;; note:  this will be an hour off for CST times
 
(define *cdt-shift* (seconds->interval (* 5 3600)))
(define *cst-shift* (seconds->interval (* 6 3600)))

;; eg: 96/04/11 18:08:19


(define cmvc-time-pattern
 (reg-expr->proc 
  '(entire
    (seq (let yy (seq digit digit))
	 #\/
	 (let mm (seq digit digit))
	 #\/
	 (let dd (seq digit digit))
	 #\space
	 (let HH (seq digit digit))
	 #\:
	 (let MM (seq digit digit))
	 #\:
	 (let SS (seq digit digit))))))

;;  the timezone crap is way hacked and hard-coded for the rsfam conversion
#|
(define (cmvc->time str)
  (bind ((s e yy mm dd HH MM SS (cmvc-time-pattern str)))
   (and s
        (let* ((d (ymd->date (+ 1900 (string->number yy)) 
				    (string->number mm)
				    (string->number dd)))
		(tz (cond
			((date<? d 1995.4.2) *cst-shift*)
			((date<? d 1995.10.29) *cdt-shift*)
			((date<? d 1996.4.7) *cst-shift*)
			((date<? d 1996.9.30) *cdt-shift*)
			(else (error "TZ is hacked... ~s not supported" d)))))
          (time+interval (day->time (date->day d)
				    (+ (* (string->number HH) 3600)
					(* (string->number MM) 60)
					(string->number SS)))
			 tz)))))
|#