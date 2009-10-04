#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/iolib/read.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.13
 | File mod date:    2005-03-16 18:43:13
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  iolib
 |
 | Purpose:          scheme reader
 `------------------------------------------------------------------------|#

;;;----------------------------------------------------------------------
;;;   read error objects and handling
;;;----------------------------------------------------------------------

(define-thread-var *read-port* #f)

(define-class <read-error> (<condition>)
  (reading-port type: <input-port>)
  (input-port-line-number type: <fixnum> init-value: -1) ;; -1=>missing
  (read-error-message type: <string>)
  (read-error-args type: <list>))

(define-method display-object ((self <read-error>) port)
  (if (name (reading-port self))
      (format port "~a:" (name (reading-port self))))
  (let ((ln (input-port-line-number self)))
    (if (eq? ln -1)
	(set! ln (input-port-line-number (reading-port self))))
    (if (not (eq? ln -1))
	(format port "~d:" ln)))
  (format port "read: ")
  (apply format port 
	 (read-error-message self)
	 (read-error-args self))
  (newline port))


(define (read:internal-error code info)
  (error (make <read-error>
	       reading-port: *read-port*
	       read-error-message: "Internal error (code ~d: ~s)"
	       read-error-args: (list code info))))

(define (read:error line msg . info-list)
  (signal (make <read-error>
		reading-port: *read-port*
		input-port-line-number: (or line -1)
		read-error-message: msg
		read-error-args: info-list)))

;;;----------------------------------------------------------------------
;;;   the reader utility procedures
;;;----------------------------------------------------------------------

(define (read:parse-vector scanner first last start-line ltab)
  (bind ((class item elem-line (read:parse-object* scanner ltab)))
    (if (eof-object? item)
	(read:error start-line
		    "<open-vector> missing <close-paren>")
	(if (eq? class 'end)
	    (case item
	      ((<close-paren>) (values 'result 
				       (list->vector first)
				       start-line))
	      ((<dot>)
	       (read:error elem-line
			   "<dot> not understood in vector datum"))
	      (else (read:internal-error 1 item)))
	    (let ((i (cons item '())))
	      (if (null? first)
		  (read:parse-vector scanner i i start-line ltab)
		  (begin
		    (set-cdr! last i)
		    (read:parse-vector scanner first i start-line ltab))))))))

(define (read:parse-list scanner first last start-line ltab)
  (bind ((class item elem-line (read:parse-object* scanner ltab)))
    (if (eof-object? item)
	(read:error start-line "<open-paren> missing <close-paren>")
	(if (eq? class 'end)
	    (case item
	      ((<close-paren>) (values 'result
				       first
				       start-line))
	      ((<dot>)
	       (if (null? first)
		   (read:error elem-line "<dot> before any items")
		   (bind ((class item tail-line (read:parse-object* scanner ltab)))
		     (if (eq? class 'result)
			 (if (eof-object? item)
			     (read:error elem-line
					 "Unexpected <eof> after <dot>")
			     (begin
			       (set-cdr! last item)
			       (bind ((class i l (read:parse-object* scanner ltab)))
				 (if (and (eq? class 'end)
					  (eq? i '<close-paren>))
				     (values 'result
					     first
					     start-line)
				     (read:error
				      l
				      "Unexpected ~s inside at end of ~s"
				      i 
				      first)))))
			 (read:error tail-line
				     "Unexpected ~s after <dot>" 
				     item)))))
	      (else (read:internal-error 2 item)))
	    (let ((i (cons item '())))
	      (if (null? first)
		  (read:parse-list scanner i i start-line ltab)
		  (begin
		    (set-cdr! last i)
		    (read:parse-list scanner first i start-line ltab))))))))

(define (read:parse-object* scanner ltab)
  (bind ((token-type token-data token-line (scanner)))
    (case token-type
      ((<literal>)
       (if ltab
           (ltab token-data token-line))
       (values 'result token-data token-line))
      ((<number> <symbol>)
       (values 'result token-data token-line))
      ((<open-paren>)
       (if ltab
           (bind ((mode data line (read:parse-list scanner '() '() token-line ltab)))
             (ltab data token-line)
             (values mode data line))
           (read:parse-list scanner '() '() token-line ltab)))
      ((<hash-comma-token>)
       (bind ((mode data line (read:parse-object* scanner ltab)))
         (if (and (eq? mode 'result)
                  (list? data))
             (let ((x (eval-hash-comma-datum data token-line)))
               (if ltab
                   (ltab x line))
               (values 'result x token-line))
             (read:error token-line "Invalid #, form"))))
      ((<close-paren> <dot>)
       (values 'end token-type token-line))
      ((<open-vector>)
       (if ltab
           (bind ((mode data line (read:parse-vector scanner '() '() token-line ltab)))
             (ltab data token-line)
             (values mode data line))
           (read:parse-vector scanner '() '() token-line ltab)))
      ((<curly-braced>)
       (let ((cb (make <curly-braced>
                       line-number: token-line
                       input-port-name: (name *read-port*)
                       text: token-data)))
         (if ltab
             (ltab cb token-line))
         (values 'result cb token-line)))
      ((quote unquote unquote-splicing quasiquote)
       (bind ((meta datum line (read:parse-object* scanner ltab)))
	 (if (eq? meta 'result)
	     (if (eof-object? datum)
		 (read:error token-line
			     "Unexpected <eof> after ~s" token-type)
                 (let ((x (list token-type datum)))
                   (if ltab
                       (ltab x token-line))
                   (values 'result x token-line)))
	     (read:error token-line
			 "Unexpected ~s after ~s" datum token-type))))
      (else
       (if (eof-object? token-type)
	   (values 'result token-type token-line)
	   (read:error token-line
		       "Unrecognized token in input: ~s ~s" 
		       token-type
		       token-data))))))

;;;----------------------------------------------------------------------
;;;   the reader main procedures and entry point
;;;----------------------------------------------------------------------

(define (read:parse-object scanner ltab)
  (bind ((type result line (read:parse-object* scanner ltab)))
    (if (eq? type 'end)
	(case result
	  ((<close-paren>) 
	   (read:error line "Unmatched <close-paren>"))
	  ((<dot>)
	   (read:error line "Misplaced <dot>"))
	  (else
	   (read:internal-error 3 result)))
	(values result line))))

;;; the `read' procedure can access the thread variable `*read-port*'
;;; to find out what port is being read from -- this is only (normally) 
;;; used in case of an error

(define-method input-port-read ((self <input-port>) #key (location-table default: #f))
  (thread-let ((*read-port* self))
    (let ((meth (find-method input-port-scan-token (list self)))
          (n (if (name self)
                 (list (name self))
                 '())))
      (read:parse-object 
       (lambda ()
         (meth self))
       (and location-table
            (lambda (datum line)
              (table-insert! location-table 
                             datum
                             (cons* (input-port-line-number self) 
                                    line
                                    n))))))))
