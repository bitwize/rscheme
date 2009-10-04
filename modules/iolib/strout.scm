#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/iolib/strout.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.8
 | File mod date:    2006-01-28 16:50:07
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  iolib
 |
 | Purpose:          <string-output-port> implementation
 `------------------------------------------------------------------------|#

(define-class <string-output-port> (<output-port>)
  current-buffer
  current-buffer-index
  ;; [see op_str.c...]
  ;; buffer-overflows is a <list> of full buffers
  buffer-overflows)

(define-glue (string-output-port-flush port closeq)
  literals: ((& signal-port-is-closed) 
             'string-output-port-flush)
{
  if (NOT( gvec_ref( port, SOP_BUFFER ))) {
    REG0 = port;
    REG1 = LITERAL(1);
    APPLYF( 2, TLREFB(0) );
  } else {
    REG0 = SOP_flush( port, truish(closeq) );
    RETURN1();
  }
})

(define-method get-output-string ((self <string-output-port>))
  (string-output-port-flush self #f))
  
(define-method close-output-port ((self <string-output-port>))
  (string-output-port-flush self #t))

(define (open-output-string)
  (make <string-output-port>
	current-buffer: (bvec-alloc <byte-vector> 100)
	current-buffer-index: 0
	buffer-overflows: '()))


(define-glue (string-output-port-write-char port the_char)
  literals: ((& signal-port-is-closed) 
             'string-output-port-write-char)
{
char ch;

  if (NOT( gvec_ref( port, SOP_BUFFER ))) {
    REG0 = port;
    REG1 = LITERAL(1);
    APPLYF( 2, TLREFB(0) );
  } else {
    ch = GET_IMMEDIATE_VALUE(the_char);
    SOP_write( port, &ch, 1 );
    RETURN0();
  }
})

(define-method output-port-write-char ((self <string-output-port>)
				       (ch <ascii-char>))
  (string-output-port-write-char self ch))


(define-glue (string-output-port-write-string port the_str)
  literals: ((& signal-port-is-closed) 
             'string-output-port-write-string)
{
  if (NOT( gvec_ref( port, SOP_BUFFER ))) {
    REG0 = port;
    REG1 = LITERAL(1);
    APPLYF( 2, TLREFB(0) );
  } else {
    SOP_write( port, string_text(the_str), string_length(the_str) );
    RETURN0();
  }
})

(define-method write-string ((self <string-output-port>) (str <string>))
  (string-output-port-write-string self str))


(define-glue (string-output-port-write-int port the_int)
  literals: ((& signal-port-is-closed) 
             'string-output-port-write-int)
{
char temp[40];
int n;

  if (NOT( gvec_ref( port, SOP_BUFFER ))) {
    REG0 = port;
    REG1 = LITERAL(1);
    APPLYF( 2, TLREFB(0) );
  } else {
    n = sprintf( temp, "%ld", (long)fx2int(the_int) );
    SOP_write( port, temp, n );
    RETURN0();
  }
})

(define-method write-int ((self <string-output-port>) (int <fixnum>))
  (string-output-port-write-int self int))

;;;

(define-method port-position ((self <string-output-port>))
  (let loop ((l (buffer-overflows self))
             ((n <fixnum>) (current-buffer-index self)))
    (if (null? l)
        n
        (loop (cdr l) (fixnum+ n (bvec-length (car l)))))))

