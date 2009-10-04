#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/corelib/process.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.12
 | File mod date:    2003-10-22 18:03:44
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  corelib
 |
 | Purpose:          Standard this-process control functions
 `------------------------------------------------------------------------|#

;;;  
;;;  Baseline RScheme interface to the currently running process
;;;

(define *verbose* #t) ;; set to `#f' by start function if "-q" or "-script"

;;; This is really filled in a boot (makeinit) time 
;;; see </compiler/boot/makeinit.scm>, and will typically
;;; look like (0 7 3 1 30)
(define *rscheme-version-list* '(0 7))
;;;

(define (set-verbose! f)
  (set! *verbose* f))

(define *args* '())  ;; application-level args (NOT process-level args)

(define (set-app-args! lst)
  (set! *args* lst))

(define-glue (process-abort)
{
unsigned i;

    for (i=0; i<arg_count_reg; i++)
      {
	fprintf( stderr, " process-abort[%u] := ", i );
	fprinto( stderr, reg_ref(i) );
	fprintf( stderr, "\n" );
      }
    abort();
})

(%early-once-only
 (define *at-exit-thunks* (make-dequeue)))

(define (on-exit (thunk <function>))
  (dequeue-push-back! *at-exit-thunks* thunk)
  (values))

(define (process-exit (code <fixnum>))
  (let (((v <vector>) (dequeue-state *at-exit-thunks*)))
    (let loop (((i <fixnum>) 0))
      (if (eq? i (gvec-length v))
	  (process-exit* code)
	  (begin
	    ((gvec-ref v i))
	    (loop (add1 i)))))))

(define-glue (process-exit* code)
{
   exit( fx2int( code ) );
   RETURN0();
})

(define-glue (os-type)
{
   REG0 = os_type();
   RETURN1();
})

;; note: if word size is N bits, then (memq 'bits-N (os-type)) is non-#f

(define-glue (word-size-bits)
{
   REG0 = int2fx( WORD_SIZE_BITS );
   RETURN1();
})

(define-safe-glue (getenv (str <raw-string>))
{
  const char *r = os_getenv( str );
  REG0 = r ? make_string( r ) : FALSE_OBJ;
  RETURN1();
})

;; return the OS-level current working directory
;;
;; normal applications will only read this at startup
;; and not usually chdir around, although chdir'ing is
;; fine as long as you know what you're doing and you
;; update *current-dir* appropriately
;;
;; a minimal implementation can return "./", although
;; doing so (or returning any relative path) will make
;; dir-from-to signal an error if an attempt is made
;; to go from an absolute path to a relative path

(define-glue (os-getwd)
{
   REG0 = os_getwd();
   RETURN1();
})

(define-safe-glue (os-setwd! (path <raw-string>))
{
  os_setwd(path);
  RETURN0();
})

(define-glue (get-compile-options)
  literals: ('bci-trace  'apply-backtrace)
{
extern int bci_trace_flag;
obj opts = NIL_OBJ;

  if (bci_trace_flag >= 0)
    opts = cons( LITERAL(0), opts );

#ifdef RECORD_CALL_CHAIN
  opts = cons( LITERAL(1), opts );
#endif
  REG0 = opts;
  RETURN1();
})

;; check or set the bytecode interpreter's trace flag
;; returns the previous state of the trace flag
;;  the trace flag is not set unless `to' is exactly #t or #f

(define-glue (set-bci-trace-flag! to)
{
extern int bci_trace_flag;
int old;

 if (bci_trace_flag >= 0)
   {
     old = bci_trace_flag;

     if (EQ(to,TRUE_OBJ))
       bci_trace_flag = 1;
     else if (EQ(to,FALSE_OBJ))
       bci_trace_flag = 0;

     REG0 = rb_to_bo(old);
   }
  else
    {
      /* this should never happen, as nobody should call 
	 this function if get-compile-options indicates
	 that this functionality is not available 
	 */
      REG0 = FALSE_OBJ;
    }
   RETURN1();
})

(define-glue (set-apply-trace-flag! to)
{
#ifndef RECORD_CALL_CHAIN
  REG0 = FALSE_OBJ;
  /* this should never happen, as nobody should call 
     this function if get-compile-options indicates
     that this functionality is not available 
     */
#else
extern rs_bool do_record_call_chain;
rs_bool old;

  old = do_record_call_chain;
  if (EQ(to,TRUE_OBJ))
    do_record_call_chain = YES;
  else if (EQ(to,FALSE_OBJ))
    do_record_call_chain = NO;
  REG0 = rb_to_bo(old);
#endif
  RETURN1();
})

