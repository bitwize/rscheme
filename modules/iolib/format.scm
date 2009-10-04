#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/iolib/format.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.17
 | File mod date:    2004-07-02 07:50:15
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  iolib
 |
 | Purpose:          Provide the `format' function
 `------------------------------------------------------------------------|#

(define-glue (parse-format-string str)
{
extern obj parse_format_string( obj string );

   REG0 = parse_format_string(str);
   RETURN1();
})

;; 
;;
;;option  mnemonic: description
;;------  ------------------------
;;    ~a  any: display the argument (as for humans).
;;    ~s  slashified: write the argument (as for parsers).
;;    ~j  joined: display over space-seperated elements of list
;;    ~d  decimal: the numeric argument is output in decimal format.
;;    ~x  hexadecimal: the integer argument is output in hexadecimal format.
;;    ~b  binary: the integer argument is output in binary format.
;;    ~o  octal: the integer argument is output in octal format.
;;    ~f  float: the numeric argument is output in fixed-format floating point
;;    ~c  character: the next argument is displayed as a character.
;;    ~_  space: output a space character.
;;    ~%  newline: output a newline character.
;;    ~~  tilde: output a tilde.
;;    ~t  tab: output a tab character.
;;    ~>  tab: the next arg is an integer # of tab stops.
;;    ~p  plural: if the argument is greater than 1, print a lower case 's'.
;;    ~h  hash: create a user hash number for the arg object
;;    ~r  raw: the 32 bits of the value are printed in hex
;;    ~C  Capitalize: like ~a, but the first letter is cap'd
;
; future:
;;    ~g  glorify: pretty print the argument (typically an s-expression).

(define-syntax (format-character v) (vector-ref v 0))
(define-syntax (sharp-flag v) (vector-ref v 1))
(define-syntax (star-flag v) (vector-ref v 2))
(define-syntax (at-flag v) (vector-ref v 3))
(define-syntax (negative-flag v) (vector-ref v 4))
(define-syntax (pre-dot-leading-zero-flag v) (vector-ref v 5))
(define-syntax (pre-dot-number v) (vector-ref v 6))
(define-syntax (post-dot-digits v) (vector-ref v 7))
(define-syntax (post-dot-number v) (vector-ref v 8))
(define-syntax (braced-modifier v) (vector-ref v 9))

(define $default-info
  '#(#f   ;; [0] control char -- not used inside of formatters
          ;;                     (used to dispatch to a formatter)
     #f   ;; [1] `#' flag
     #f   ;; [2] `*' flag
     #f   ;; [3] `@' flag
     #f   ;; [4] `-' flag
     #f   ;; [5] pre-`.' leading zero flag
     #f   ;; [6] pre-`.' number
     #f   ;; [7] post-`.' digits
     #f   ;; [8] post-`.' number
     #f)) ;; [9] braced modifier

(define (format/a port arg)
  (display-object arg port))

(define (format/s port arg)
  (write-object arg port))

(define (format/p port arg)
  (if (not (= arg 1))
      (output-port-write-char port #\s)))

(define (format/d port (arg <number>))
  (write-string port (number->string arg)))

(define (format/x port arg)
  (write-string port (number->string arg 16)))

(define (format/r port arg)
  (write-string port (machine-bits->string arg)))

(define (format/b port arg)
  (write-string port (number->string arg 2)))

(define (format/o port arg)
  (write-string port (number->string arg 8)))

(define (format/c port arg)
  (output-port-write-char port arg))

#|
(define (format/h port info arg)
  (write-string port "#*")
  (write-int port (get-user-hash arg)))

(define (format/> port info arg)
  (write-tab4 arg port))
|#

(define (format/_ port)
  (output-port-write-char port #\space))

(define (format/% port)
  (output-port-write-char port #\newline))

(define (format/~ port)
  (output-port-write-char port #\~))

(define (format/t port)
  (output-port-write-char port #\tab))

(define (format/C port arg)
  (display-object 
   arg 
   (make-output-filter 
    port
    (let ((first #t))
      (lambda ((str <string>))
	(if (and first (fixnum>? (string-length str) 1))
	    (begin
	      (set! first #f)
	      (string-append
	       (string (char-upcase (string-ref str 0)))
	       (substring str 1)))
	    str))))))

(define (format/j port (arg <list>))
  (if (pair? arg)
      (begin
	(display-object (car arg) port)
	(let loop (((r <list>) (cdr arg)))
	  (if (pair? r)
	      (begin
		(output-port-write-char port #\space)
		(display-object (car r) port)
		(loop (cdr r))))))))
      

; Note:  this is arranged so that users
; can create their own formatter functions (a la NeXTSTEP)
; Since you want scope control, a global access point is not
; desired.  Instead, there is an alternate procedure form,
; `format-using', which takes a formatter list.
; Furthermore, *global-formatters* is the default list,
; so you can just prepend onto that and get the standard stuff.
;

(define (string-split-evenly str n)
  (let loop ((r '())
             (i (string-length str)))
    (if (> i 0)
        (let ((j (max 0 (- i n))))
          (loop (cons (substring str j i) r) j))
        r)))

(define (format-gen/d info)
  (values
   1
   (if (eq? info $default-info)
       format/d
       (if (pre-dot-number info)
           (if (star-flag info)
               (lambda (port (arg <number>))
                 (let ((a (do-align (number->string arg) info)))
                   (write-string port 
                                 (string-join #\, (string-split-evenly a 3)))))
               (lambda (port (arg <number>))
                 (write-string port (do-align (number->string arg) info))))
           (if (star-flag info)
               (lambda (port (arg <number>))
                 (let ((a (number->string arg)))
                   (write-string port 
                                 (string-join #\, (string-split-evenly a 3)))))
               (lambda (port (arg <number>))
                 (write-string port (number->string arg))))))))

       

(define (format-gen/f info)
  (values 
   1
   (let* ((prec (post-dot-number info))
          (c-fmt (string-append "%"
                                (if (negative-flag info) "-" "")
                                (if (pre-dot-leading-zero-flag info)
                                    "0"
                                    "")
                                (let ((q (pre-dot-number info)))
                                  (if q
                                      (number->string q)
                                      ""))
                                (if prec "." "")
                                (if prec (number->string prec) "")
                                "f")))
     (lambda (port (arg <number>))
       (write-string port (sprintf-float c-fmt 99 (exact->inexact arg)))))))
             
             

(define (std-format-gen-1 underlying-format-proc)
  (lambda (info)
    (values
     1
     (if (eq? info $default-info)
	 underlying-format-proc
	 (standard-formatting-options info underlying-format-proc)))))

(define (std-format-gen-0 underlying-format-proc)
  (lambda (info)
    (values
     0
     (if (eq? info $default-info)
	 underlying-format-proc
	 (standard-formatting-options info underlying-format-proc)))))

(%early-once-only
(define *global-formatters*
  (list (cons #\a (std-format-gen-1 format/a))
	(cons #\s (std-format-gen-1 format/s))
	(cons #\d format-gen/d)
	(cons #\f format-gen/f)
	(cons #\x (std-format-gen-1 format/x))
	(cons #\b (std-format-gen-1 format/b))
	(cons #\o (std-format-gen-1 format/o))
	(cons #\c (std-format-gen-1 format/c))
	(cons #\_ (std-format-gen-0 format/_))
	(cons #\% (std-format-gen-0 format/%))
	(cons #\~ (std-format-gen-0 format/~))
	(cons #\t (std-format-gen-0 format/t))
	(cons #\j (std-format-gen-1 format/j))
	(cons #\C (std-format-gen-1 format/C))
	(cons #\p (std-format-gen-1 format/p))
	(cons #\r (std-format-gen-1 format/r)))))

(define (pop-format-args args num req)
  (case num
    ((0)
     (values '() args))
    ((1)
     (if (pair? args)
         (values (cons (car args) '())
                 (cdr args))
         (error "format: missing argument for `~~~a'" req)))
    (else
     (let ((first (cons (car args) '())))
       (let loop ((prev first)
                  (r (cdr args))
                  (n (- num 1)))
         (if (eq? n 0)
             (values first r)
             (if (pair? r)
                 (let ((cell (cons (car r) '())))
                   (set-cdr! prev cell)
                   (loop cell (cdr r) (- n 1)))
                 (error "format: missing arguments for `~~~a'" req))))))))

#|
(define (rendition-function (info <vector>))
  (if (pre-dot-number info)
      ;; field width specified...
      (if (sharp-flag info)
	  ;; `#' ==> truncate
	  (truncation-filter
	   ;; within-bounds filter
	   (if (at-flag info)
	       ;; `@' ==> don't align
	       identity
	       ;; no `@' ==> align
	       (lambda 'rendition-function/align (str)
		 (do-align str info)))
	   ;; outside bounds filter
	   (if (star-flag info)
	       ;; `*' ==> mark overlow
	       mark-overflow
	       ;; no `*' ==> don't mark overflow
	       identity))
	  ;; no `#' ==> don't truncate
	  no-truncate-filter)
      null-filter))

(define (null-filter fmt-proc
		     (port <output-port>)
		     (info <vector>))
  (fmt-proc port))

|#

(define (do-format dest (format-str <string>) arg-list formatters)
  (let ((port (if dest (if (eq? dest #t)
			   (current-output-port) 
			   dest)
		  (open-output-string))))
    (let loop ((control (parse-format-string format-str)) 
	       (args arg-list))
      (if (pair? control)
	  (let ((x (car control)))
	    (if (string? x)
		(begin
		  (write-string port x)
		  (loop (cdr control) args))
		(let* ((fmt-ch (if (ascii-char? x)
				   x
				   (format-character x)))
		       (fmtr (assq fmt-ch formatters)))
		  (if fmtr
		      (bind ((num-args proc ((cdr fmtr) 
					     (if (ascii-char? x)
						 $default-info
						 x)))
			     (these-args rest (pop-format-args args
                                                               num-args
                                                               fmt-ch)))
			(apply* port these-args proc)
			(loop (cdr control) rest))
		      (error "format: unrecognized format code `~~~a'"
			     fmt-ch)))))))
    (if dest
	(values)
	(close-output-port port))))

;;;  `proc' is a procedure of 1+N arguments;
;;;    a port,
;;;    and the arguments that were passed to the procedure
;;;    which `standard-formatting-options' returns

(define (standard-formatting-options info proc)
  (if (pre-dot-number info)
      ;; field width specified...
      (if (sharp-flag info)
	  ;; `#' ==> truncate
	  (truncation-filter
	   proc
	   info
	   ;; within-bounds filter
	   (if (at-flag info)
	       ;; `@' ==> don't align
	       identity
	       ;; no `@' ==> align
	       (lambda 'rendition-function/align (str)
		 (do-align str info)))
	   ;; outside bounds filter
	   (if (star-flag info)
	       ;; `*' ==> mark overlow
	       mark-overflow
	       ;; no `*' ==> don't mark overflow
	       identity))
	  ;; no `#' ==> don't truncate
	  (no-truncate-filter proc info))
      proc))

(define (do-align str info)
  (let ((deficit (- (pre-dot-number info) (string-length str))))
    (if (< deficit 1)
	str
	(if (negative-flag info)
	    ;; `-' specified -- right justify
	    (if (pre-dot-leading-zero-flag info)
		;; `-0' specified -- justify center
		(let ((l (quotient deficit 2)))
		  (string-append (make-string l #\space)
				 str
				 (make-string (- deficit l) #\space)))
		;; `-' only -- justify right
		(string-append (make-string deficit #\space) str))
	    ;; `-' not specified
	    (if (pre-dot-leading-zero-flag info)
		;; `0' specified -- justify right with 0 padding
		(string-append (make-string deficit #\0) str)
		;; nothing specified -- justify left
		(string-append str (make-string deficit #\space)))))))

(define (mark-overflow str)
  (string-append str "..."))

(define (no-truncate-filter proc info)
  (lambda (port . args)
    (let ((p (open-output-string)))
      (apply proc p args)
      (write-string port (do-align (close-output-port p) info)))))

(define (truncation-filter proc info within-proc overflow-proc)
  (lambda (port . args)
    (write-string
     port
     (with-bounded-string-port* (pre-dot-number info)
				(lambda (p)
				  (apply proc p args))
				within-proc
				overflow-proc))))


(define-macro (~ format-str . arg-list)
  (if (string? format-str)
      (let ((t (gensym)))
        (let loop ((control (parse-format-string format-str))
                   (r '())
                   (a arg-list))
          (cond
           ((null? control)
            (if (not (null? a))
                (error "Leftover format argument exprs: ~s" a))
            `(let ((,t (open-output-string)))
               ,@(reverse r)
               (close-output-port ,t)))
           ((string? (car control))
            (loop (cdr control)
                  (cons `(write-string ,t ,(car control)) r)
                  a))
           (else
            (bind ((x (car control))
                   (fmt-ch (if (ascii-char? x)
                               x
                               (format-character x)))
                   (num-args proc ((cdr (assq fmt-ch *global-formatters*))
                                   (if (ascii-char? x)
                                       $default-info
                                       x)))
                   (these rest (pop-format-args a num-args fmt-ch)))
              (loop (cdr control)
                    (cons `(',proc ,t ,@these) r)
                    rest))))))
      `(format #f ,format-str ,@arg-list)))

  
;;;
;;;  (format DEST FMT-STR ARG ...)
;;;  (format-using FORMATTERS DEST FMT-STR ARG ...)
;;;

(define (format dest . args)
  (if (string? dest)
      (do-format #f dest args *global-formatters*)
      (do-format dest (car args) (cdr args) *global-formatters*)))

(define (format-using formatters dest format-str . arg-list)
  (do-format dest format-str arg-list formatters))

(define (make-formatter new-formatters)
  (let ((formatters (append new-formatters *global-formatters*)))
    (lambda (dest format-str . arg-list)
      (do-format dest format-str arg-list formatters))))

(%early-once-only (set__format format))
