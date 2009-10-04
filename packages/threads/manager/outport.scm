
(define-constant $closed-buffer "")

(define (port-is-closed port)
  (error "port is closed: ~s" port))

(define-class <queued-output-port> (<output-port>)
  (buffer type: <string> init-value: $closed-buffer)
  (buffer-index type: <fixnum> init-value: 0)
  (pending-writes init-value: #f) ;; #f or a <dequeue>
  (event type: <fixnum> init-value: 0)
  (fd type: <fixnum>)
  (block-size type: <fixnum> init-value: 4072)
  (error-proc init-value: #f)  ;; #f or a procedure of 2 arguments
  (flush-lines? type: <boolean> init-value: #f)
  (owner? type: <boolean> init-value: #f))

;;;

(define-safe-glue (qout-write-bytes (port <queued-output-port>) 
				    bvec
				    (offset <raw-int>)
				    (len <raw-int>))
  literals: ($closed-buffer (& port-is-closed) (& signal))
{
  if (EQ(gvec_ref(port,QOUT_BUFFER),LITERAL(0)))
   {
     APPLY(1,TLREF(1));
   }
  else
   {
     if (qout_write_bytes( port, bvec, offset, len ))
       {
	 RETURN0();
       }
     else
       {
	 /* arrange to continue, to check error code */
	 /* (and note that the RC will magically be in ref[0] of the frame,
	    see CR 677) */
	 SAVE_CONT1( qout_did_write_bytes );
	 SWITCH_THREAD( port, TSTATE_BLOCKED );
       }
   }
}
("qout_did_write_bytes" {

  /* restore continuation, including status in REG0 */
  RESTORE_CONT1();

  if (EQ( REG0, FALSE_OBJ )) /* no error */
   {
     RETURN0();
   }
  else
   {
     APPLY( 1, TLREF( 2 ) );  /* error during flush */
   }
}))

(define-safe-glue (qout-write-vec (port <queued-output-port>) 
				  (item_vec <vector>))
  literals: ($closed-buffer (& port-is-closed) (& signal))
{
  if (EQ(gvec_ref(port,QOUT_BUFFER),LITERAL(0)))
   {
     APPLY(1,TLREF(1));
   }
  else if (qout_writev( port, item_vec ))
   {
     RETURN0();
   }
  else
   {
     /* create a continuation with space for a return code */
     SAVE_CONT1( qout_did_write_vec );
     SWITCH_THREAD( port, TSTATE_BLOCKED );
   }
}
("qout_did_write_vec" {
  RESTORE_CONT1();
  if (EQ( REG0, FALSE_OBJ )) /* no error */
   {
     RETURN0();
   }
  else
   {
     APPLY( 1, TLREF( 2 ) );  /* error during flush */
   }
}))

(define-safe-glue (qout-flush (port <queued-output-port>) close_q)
   literals: ((& signal) $closed-buffer (& port-is-closed))
{
  if (EQ(gvec_ref(port,QOUT_BUFFER),LITERAL(1)))
   {
     APPLY(1,TLREF(2));
   }
  else if (qout_flush(port))
   {
     if (truish(close_q))
       gvec_write_ptr( port, QOUT_BUFFER, LITERAL(1) );
     RETURN0();
   }
  else
   {
     REG2 = raw_port;
     SAVE_CONT3(qout_did_flush);
     SWITCH_THREAD( port, TSTATE_BLOCKED );
   }
}
("qout_did_flush"
{
  RESTORE_CONT3();
  if (EQ(REG0,FALSE_OBJ))
   {
     if (truish(close_q))
       gvec_write_ptr( REG2, QOUT_BUFFER, LITERAL(1) );
     RETURN0();
   }
  else
    APPLY(1,TLREF(0));
}))

;;;


(define-method datum->writev ((self <fixnum>))
  self)

(define-method datum->writev ((self <symbol>))
  self)

(define-method datum->writev ((self <string>))
  (let loop ((i 0)
	     (r '#()))
    (bind ((str-part next-i (string->printable self i)))
      (if next-i
	  (loop next-i (vector r str-part))
	  (vector #\" r str-part #\")))))

(define-method datum->writev ((self <empty-list>))
  "()")

(define-method datum->writev ((self <number>))
  (number->string self))

(define-method datum->writev ((self <pair>))
  (let loop ((r (cdr self))
	     (x (vector #\( (datum->writev (car self)))))
    (if (pair? r)
	(loop (cdr r)
	      (vector x #\space (datum->writev (car r))))
	(if (null? r)
	    (vector x #\))
	    (vector x " . " (datum->writev r) #\))))))

(define-method write-datum ((self <queued-output-port>) item)
  (qout-write-vec self (vector (datum->writev item))))

;;;

(define-method write-string ((self <queued-output-port>) (str <string>))
  (qout-write-bytes self str 0 (string-length str)))

(define-method write-int ((self <queued-output-port>) (n <number>))
  (if (fixnum? n)
      (qout-write-vec self (vector n))
      (let (((s <string>) (number->string n)))
	(qout-write-bytes self s 0 (string-length s)))))

(define-method output-port-write-char ((self <queued-output-port>) (ch <char>))
  (qout-write-vec self (vector ch)))

(define-method close-output-port ((self <queued-output-port>))
  (let-syntax ((close! (syntax-form ()
                         (if (owner? self)
                             (begin
                               (set-owner?! self #f)
                               (fd-close (fd self))))
                         (set-fd! self -1)
                         (set-buffer! self $closed-buffer)
                         (values))))
    (handler-case
     (begin
       (qout-flush self #t)
       (close!))
     ((<condition> condition: c)
      (close!)
      (signal c)))))

(define-method flush-output-port ((self <queued-output-port>))
  (qout-flush self #f))

(define-method write-bytes ((self <queued-output-port>) bvec offset len)
  (qout-write-bytes self bvec offset len))

(define-method writev ((self <queued-output-port>) struct)
  (qout-write-vec self struct))

(define-method writev ((self <output-port>) (vec <vector>))
  (let loop (((i <fixnum>) 0))
    (if (fixnum<? i (vector-length vec))
	(let ((e (vector-ref vec i)))
	  (cond
	   ((string? e)
	    (write-string self e))
	   ((char? e)
	    (write-char e self))
	   ((fixnum? e)
	    (write-string self (number->string e)))
	   ((vector? e)
	    (writev self e))
	   ((bvec? e)
	    (write-bytes self e 0 (bvec-length e)))
	   ((symbol? e)
	    (write-string self (symbol->string e)))
	   (else
	    (error "writev: invalid item ~s" e)))
	  (loop (add1 i))))))

;;;

(define (open-queued-output fd #optional owner)
  (let ((p (make <queued-output-port>
                 buffer: (bvec-alloc <string> 4072)
                 pending-writes: (make-dequeue)
                 fd: fd
                 owner?: owner)))
    p))

(define (filedes->output-port filedes #optional owner)
  (open-queued-output filedes owner))
