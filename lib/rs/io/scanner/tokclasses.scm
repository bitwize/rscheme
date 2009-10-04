
(define *num-initial* 
  (char-set-union
   *digit*
   (members->char-set '#(#\+ #\- #\.))))
   
(define *num-continued* 
  (char-set-union
   *num-initial*
   (members->char-set '#(#\e #\E #\s #\S #\f #\F #\d #\D #\l #\L
			 #\i #\I #\# #\/ #\@))))

(for-each
 (lambda (ch)
   (table-insert! *num-continued* ch #t))
 '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\. #\e #\E #\+ #\- #\/))

(define *special-initial*
  (members->char-set
   '#(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\~ #\_ #\^)))

(define *special-continued*
  (members->char-set
   '#(#\. #\+ #\-)))

(define *id-initial* 
  (char-set-union *letter* *special-initial*))

(define *id-continued* 
  (char-set-union *id-initial* *digit* *special-continued*))
