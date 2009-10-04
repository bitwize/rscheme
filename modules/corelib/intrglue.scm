#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/corelib/intrglue.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.16
 | File mod date:    2004-03-25 13:28:23
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  corelib
 |
 | Purpose:          scheme-side interface for interrupts
 `------------------------------------------------------------------------|#

;;  this vector is available to the C interrupt dispatch code
;;  (runtime/intrs.c), from which it reads the handler proc
;;  the vector is indexed by RSSIG number, defined in intrs.h
;;
;;  slots 6, 7, and 8 are not used because the procedure to call
;;  comes from the interrupt info itself

(define *interrupt-handlers* '#(#f #f #f #f #f #f 0 0 0))

(define-safe-glue (setup-c-signal-handler! (sig_name <symbol>))
{
  int n = rs_c_signal_num(sig_name);
  if ((n < 0)
      ||
      (os_register_signal_handler( n, c_signal_catcher ) < 0))
    {
      scheme_error( "C signal '~s' is invalid", 1, sig_name );
    }
  RETURN0();
})

(define-safe-glue (ignore-c-signal! (sig_name <symbol>))
{
  int n = rs_c_signal_num(sig_name);
  if ((n < 0)
      ||
      (os_register_signal_handler( n, NULL ) < 0))
    {
      scheme_error( "C signal '~s' is invalid", 1, sig_name );
    }
  RETURN0();
})

;;
;; the proc should take a single argument which is the signal
;; instance information
;;

(define (register-interrupt-handler! handler-id (proc <function>))
  (vector-set!
   *interrupt-handlers*
   (case handler-id
     ((user-intr control-c) 0)
     ((timer) 1)
     ((child-exited) 2)
     ((finalize) 3)
     ((gc-flip) 4)
     ((c-signal) 5)
     (else (error "~s: invalid interrupt handler" handler-id)))
   proc)
  (values))


;;
;;  a pointer to this template is scheme_global[6], and is used
;;  by runtime/intrs.c to be the continuation from an interrupt
;;
;;

(define-glue (continue-intr-template) :template
{
jump_addr f;

    RESTORE_CONT0();	/* personally, we don't have anything
    			   saved in our continuation */
    f = half_restore();			/* but get the interrupted
    					   context's fixed registers */
    arg_count_reg = restore_arb();	/* and all the other regs */
    os_set_sigenable( YES );

#ifdef GNU_VINSNS
    goto *f;
#else
    return f;
#endif
})

;;;  this template is special in the following way:
;;;  it's entry point is never exercised.  The runtime system
;;;  (entry.c) constructs a continuation to it's monotone[1]
;;;  which is responsible for escaping the quasi-interpretive
;;;  spin loop when a call to call_scheme() is complete
;;;
;;;  although the first monotone is never used, I nevertheless
;;;  constructed it so that if this function were called, it
;;;  would terminate the call to call_scheme()

(define-glue (return-from-call-scheme) :template
{
  JUMP(arg_count_reg,finish_run_1);  /* never used */
}
("finish_run_1" {
   done_w_call_scheme();
   RETURN1(); /* never reached */
}))

(define-glue (enable-subprocess-capture)
{
  enable_subprocess_capture();
  RETURN0();
})

;;; returns a collection (vector) mapping
;;; signal numbers to names (symbols)
;;  (an entry may be #f if the name is not known)

(define (os-signal-name-vector)
  (rscheme-global-ref 40)) ;; c_signal_names a.k.a. *c-signal-names*

(define (signal-number->name (signum <fixnum>))
  (let ((v (os-signal-name-vector)))
    ; vector-length, >=, and < aren't defined yet
    (if (and (fixnum>=? signum 0)
	     (fixnum<? signum (gvec-length v))) 
	(gvec-ref v signum)
	#f)))
