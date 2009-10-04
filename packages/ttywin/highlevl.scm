
(define-fluid *screen*)

(define (with-curses thunk)
  (save-tty)
  (let ((s (init-curses)))
    (fluid-let ((*screen* s))
;      (mode-cbreak)
      (mode-cr)
      (mode-raw)
      (mode-no-echo)
      (bind ((#rest x (thunk)))
	(end-curses)
	(reset-tty)
	(list->values x)))))

(define (call-with-field-output y x proc)
  (let ((p (open-output-string))
	(s (fluid-ref *screen*)))
    (proc p)
    (move s y x)
    (add-str s (close-output-port p))))

(define format-at #f)

(define-class <screen-output-port> (<output-port>)
  underlying-screen
  x-margin
  y-posn)


(define (format/standout-begin port info)
  (standout-begin (underlying-screen port)))

(define (format/standout-end port info)
  (standout-end (underlying-screen port)))

(define-method output-port-write-char ((self <screen-output-port>) ch)
  (add-char (underlying-screen self) ch))

(define-method write-string ((self <screen-output-port>) (str <string>))
  (let ((s (underlying-screen self)))
    (let loop ((i 0))
      (let ((x (string-search str #\newline i)))
	(if x
	    (begin
	      (add-str s (substring str i x))
	      (set-y-posn! self (+ (y-posn self) 1))
	      (move s (y-posn self) (x-margin self))
	      (loop (+ x 1)))
	    (add-str s (if (eq? i 0)
			   str
			   (substring str i))))))))

;;
;; format-at provides two additional format codes:  ~< and ~> which
;; respectively turn on and off standout mode
;;

(%early-once-only
 (set! format-at
       (let ((format (make-formatter
		      (list (cons* #\< format/standout-begin 0)
			    (cons* #\> format/standout-end 0)))))
       (lambda (y x fmt . args)
	 (let ((s (fluid-ref *screen*)))
	   (move s y x)
	   (apply* (make <screen-output-port>
			 underlying-screen: s
			 x-margin: x
			 y-posn: y)
		   fmt 
		   args
		   format))))))

;; chars ==> UL TOP UR LEFT FILL RIGHT LL BOT LR

(define (paint-line y x h w chars offset)
  (let (((left-ch <ascii-char>) (string-ref chars (+ offset 0)))
	((mid-str <string>) (make-string w (string-ref chars (+ offset 1))))
	((right-ch <ascii-char>) (string-ref chars (+ offset 2)))
	(base (fluid-ref *screen*)))
    (let loop (((i <fixnum>) y) 
	       ((n <fixnum>) h))
      (if (not (eq? n 0))
	  (begin
	    (move base i x)
	    (add-char base left-ch)
	    (add-str base mid-str)
	    (add-char base right-ch)
	    (loop (+ i 1) (- n 1)))))))

(define (paint-border y x h w (chars <string>))
  (let ((x0 (- x 1)))
    (paint-line (- y 1) x0 1 w chars 0)
    (paint-line y x0 h w chars 3)
    (paint-line (+ y h) x0 1 w chars 6)))
  
(define (with-subwindow y x h w thunk)
  (let ((sub (sub-window (fluid-ref *screen*) h w y x))
	(results '()))
    (paint-border y x h w "+-+| |+-+")
    (refresh *screen*)
    (fluid-let ((*screen* sub))
      (bind ((#rest x (thunk)))
	(set! results x)))
    (paint-border y x h w "         ")
    (list->values results)))


(define (get-edited-line-from-subwin y x h w prompt check-input do-completion)
  (with-subwindow y x h w
		  (lambda ()
		    (format-at 0 0 prompt)
		    (refresh *screen*)
		    (get-edited-line (+ y 1) x (- h 1) w check-input do-completion))))

(define (getch y x)
  (move *screen* y x)
  (refresh *screen*)
  (get-char *screen*))

(define (get-edited-line y x h w check-input do-completion)
   (let ((sub (sub-window (fluid-ref *screen*) h w y x)))
     (assert (not (eq? sub 0)))
     (let ((str (fluid-let ((*screen* sub))
		  (edit-buffer "" check-input do-completion))))
       ;; so how do you delete windows?
       str)))

(define (draw-buffer str)
  (move *screen* 0 0)
  (clear-to-eol *screen*)
  (add-str *screen* str))


;;  look-for-error is a procedure of one argument, a proposed
;;  return string.  If it returns #f, the string is acceptable
;;  and is returned.  Otherwise, it returns the index of the
;;  location of the error and an error message (two values)
;;
;;  do-completion-on is a procedure of two arguments, a current
;;  buffer state and the cursor position.   It should return two
;;  values, the new buffer and the new cursor position.
;; 

(define (edit-buffer str look-for-error do-completion-on)
  (let ((scr *screen*)
	(ix (string-length str))
	(buf str))
    (let loop ()
      (draw-buffer buf)
      (let ((ch (getch 0 ix)))
	(move scr 1 0)  (clear-to-eol scr) ;; clear error line
	(if (or (char<? ch #\space)
		(char>? ch #\~))
	    ;;
	    ;; control character
	    ;;
	    (case (char->integer ch)
	      ((8 127)
	       (if (> ix 0)
		   (begin
		     (set! buf (string-append (substring buf 0 (- ix 1))
					      (substring buf ix)))
		     (set! ix (- ix 1))))
	       (loop))
	      ((4)
	       (if (< ix (string-length buf))
		   (set! buf (string-append (substring buf 0 ix)
					    (substring buf (+ ix 1)))))
	       (loop))
	      ((10 13)
	       (bind ((err-at err-msg (look-for-error buf)))
		 (if err-at
		     (begin
		       (set! ix err-at)
		       (format-at 1 0 "~<Error:~> ~a" err-msg)
		       (clear-to-eol *screen*)
		       (loop))
		     buf)))
	      ((1)
	       (set! ix 0)
	       (loop))
	      ((11)  ;; ^K
	       (set! buf (substring buf 0 ix))
	       (loop))
	      ((9)
	       (bind ((b i (do-completion-on buf ix)))
		 (set! buf b)
		 (set! ix i)
		 (loop)))
	      ((2)
	       (if (> ix 0)
		   (set! ix (- ix 1)))
	       (loop))
	      ((6)
	       (if (< ix (string-length buf))
		   (set! ix (+ ix 1)))
	       (loop))
	      ((5)
	       (set! ix (string-length buf))
	       (loop))
	      (else
	       (loop)))
	    ;;
	    ;; normal character
	    ;;
	    (begin
	      (set! buf (string-append 
			 (substring buf 0 ix)
			 (string ch)
			 (substring buf ix)))
	      (set! ix (+ ix 1))
	      (loop)))))))
