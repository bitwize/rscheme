#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/iolib/bstrout.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.8
 | File mod date:    2003-03-03 01:20:10
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  iolib
 |
 | Purpose:          Bounded-output strings
 `------------------------------------------------------------------------|#

(define-class <bounded-string-output-port> (<output-port>)
  current-buffer
  current-buffer-index
  overflow-procedure)

(define (bounded-string-port-overflowed port)
 ((overflow-procedure port) (current-buffer port)))

(define-glue (bounded-string-output-port-flush port closeq)
  literals: ((& signal-port-is-closed) 
             'bounded-string-output-port-flush)
{
  if (NOT( gvec_ref( port, BSOP_BUFFER ))) {
    REG0 = port;
    REG1 = LITERAL(1);
    APPLYF( 2, TLREFB(0) );
  } else {
    REG0 = BSOP_flush( port, truish(closeq) );
    RETURN1();
  }
})

(define-method get-output-string ((self <bounded-string-output-port>))
  (string-output-port-flush self #f))

(define-method close-output-port ((self <bounded-string-output-port>))
  (string-output-port-flush self #t))

(define (open-output-bounded-string proc (n <fixnum>))
  (make <bounded-string-output-port>
	current-buffer: (bvec-alloc <string> (add1 n))
	current-buffer-index: 0
	overflow-procedure: proc))

(define-glue (bounded-string-output-port-write-char port the_char)
 literals: ((& bounded-string-port-overflowed)
            (& signal-port-is-closed) 
            'bounded-string-output-port-write-char)
{
char ch;

  if (NOT( gvec_ref( port, BSOP_BUFFER ))) {
    REG0 = port;
    REG1 = LITERAL(2);
    APPLYF( 2, TLREFB(1) );
  } else {
    ch = GET_IMMEDIATE_VALUE(the_char);
    if (!BSOP_write( port, &ch, 1 ))
      APPLYF(1,TLREFB(0));
    else
      RETURN0();
  }
})

(define-method output-port-write-char ((self <bounded-string-output-port>) 
				       (ch <ascii-char>))
  (bounded-string-output-port-write-char self ch))


(define-glue (bounded-string-output-port-write-string port the_str)
 literals: ((& bounded-string-port-overflowed)
            (& signal-port-is-closed) 
            'bounded-string-output-port-write-string)
{
  if (NOT( gvec_ref( port, BSOP_BUFFER ))) {
    REG0 = port;
    REG1 = LITERAL(2);
    APPLYF( 2, TLREFB(1) );
  } else {
    if (!BSOP_write( port, string_text(the_str), string_length(the_str) ))
      APPLYF(1,TLREFB(0));
    else
      RETURN0();
  }
})

(define-method write-string ((self <bounded-string-output-port>) 
			     (str <string>))
  (bounded-string-output-port-write-string self str))


(define-glue (bounded-string-output-port-write-int port the_int)
 literals: ((& bounded-string-port-overflowed)
            (& signal-port-is-closed) 
            'bounded-string-output-port-write-int)
{
char temp[40];

  if (NOT( gvec_ref( port, BSOP_BUFFER ))) {
    REG0 = port;
    REG1 = LITERAL(2);
    APPLYF( 2, TLREFB(1) );
  } else {
    sprintf( temp, "%ld", (long)fx2int(the_int) );
    if (!BSOP_write( port, temp, strlen(temp) ))
      APPLY(1,TLREF(0));
    else
      RETURN0();
  }
})

(define-method write-int ((self <bounded-string-output-port>) (int <fixnum>))
  (bounded-string-output-port-write-int self int))

;;
;;  friendly functions...
;;

(define (with-bounded-string-port* size proc finish-proc1 finish-proc2)
  (call-with-current-continuation
   (lambda (exit)
     (let ((port (open-output-bounded-string (lambda (result)
					       (exit (finish-proc2 result)))
					     size)))
       (proc port)
       (finish-proc1 (bounded-string-output-port-flush port #t))))))

(define (bstr-negarg fn len)
  (error "~s: arg `len' must be non-negative, not ~d"
	 fn
	 len))

(define (object->bounded-string (len <fixnum>) thing)
  (if (fixnum<? len 0)
      (bstr-negarg 'object->bounded-string len)
      (with-bounded-string-port* 
       len
       (lambda (p) (write-object thing p))
       identity
       (lambda (str) (string-append str "...")))))

(define (display->bounded-string (len <fixnum>) thing)
  (if (fixnum<? len 0)
      (bstr-negarg 'display->bounded-string len)
      (with-bounded-string-port* 
       len
       (lambda (p) (display-object thing p))
       identity
       identity)))
