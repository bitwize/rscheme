
(define-class <time> (<object>) :bvec image-mode: 7)
(define-class <interval> (<object>) :bvec image-mode: 7)

(define-syscall-glue (clock)
  literals: ((& <interval>))
{
  struct scheme_time t;
  clock_t c = clock();

#ifdef PLATFORM_RHAPSODY
#define CLOCKS_PER_SEC 100 /* anything like right? */
#endif

  t.sec = c / CLOCKS_PER_SEC;
  t.usec = (c % CLOCKS_PER_SEC) * (1000000 / CLOCKS_PER_SEC);
  REG0 = make_time( &t, TLREF(0) );
  RETURN1();
})

(define-syscall-glue (time)
  literals: ((& <time>))
{
  REG0 = alloc( sizeof(struct scheme_time), TLREF(0) );
  current_time( PTR_TO_DATAPTR(REG0) );
  RETURN1();
})

;;
;; this function interprets 'day' in the same way the
;; calendar package does
;;

(define-syscall-glue (day->time (day <raw-int>) secv)
  literals: ((& <time>))
{
  struct scheme_time *t;
  int time_sec, time_us;

  if (basic_raw_int_conv_p( secv )) {
    time_sec = basic_raw_int_conv( secv );
    time_us = 0;
  } else if (basic_raw_float_conv_p( secv )) {
    double frac, full;
    double x = basic_raw_float_conv( secv );

    frac = modf( x, &full );
    time_sec = full;
    time_us = frac * 1000000;
  } else {
    scheme_error( "day->time: expected basic numeric argument, got ~s",
                  1, secv );
    RETURN0();
  }

  REG0 = alloc( sizeof(struct scheme_time), TLREF(0) );
  t = PTR_TO_DATAPTR(REG0);
  t->usec = time_us;
  /* day number 719163 is Jan 1, 1970 */
  t->sec = (day - 719163) * 86400 + time_sec;
  RETURN1();
})

(define-syscall-glue (time-again! (t <time>))
{
  current_time( t );
  RETURN0();
})

(define-syscall-glue (time->calendar (t <time>) (localq <raw-bool>))
{
  struct tm *tm;

  tm = calendar_time( t, localq );
  REG0 = int2fx( tm->tm_year );
  REG1 = int2fx( tm->tm_mon );
  REG2 = int2fx( tm->tm_mday );
  REG3 = int2fx( tm->tm_hour );
  REG4 = int2fx( tm->tm_min );
  REG5 = int2fx( tm->tm_sec );
  REG6 = int2fx( tm->tm_yday );
  REG7 = int2fx( tm->tm_wday );
  REG8 = tm->tm_isdst ? TRUE_OBJ : FALSE_OBJ;
  RETURN(9);
})

;;
;;  (time->string time [format-string [localq]])
;;

(define-syscall-glue (time->string (time <time>) #rest)
{
  struct tm *tm;
  rs_bool localq;

  if (arg_count_reg > 3)
    scheme_error( "time->string: expected 1-3 arguments, got ~d",
		  1, int2fx(arg_count_reg) );

  if (arg_count_reg == 3)
    localq = truish( REG2 );
  else
    localq = YES;

  tm = calendar_time( time, localq );

  if (arg_count_reg > 1)
    {
      if (!STRING_P(REG1))
	scheme_error( "time->string: expected <string> as format arg, got ~s",
		      1, REG1 );

      if (string_length(REG1) == 0)
	{
	  REG0 = make_string( "" );
	}
      else
	{
	  char temp[401];
	  int n;

	  n = strftime( temp, 400, string_text(REG1), tm );
	  if (n > 0)
	    REG0 = make_string( temp );
	  else
	    REG0 = FALSE_OBJ;
	}
    }
  else
    {
      char *t, *t2;

      t = asctime( tm );
      if ((t2 = strchr(t,'\n')))
	*t2 = 0;		/* strip trailing '\n' */
      REG0 = make_string( t );
    }
  RETURN1();
})

(define-syscall-glue (seconds->interval isec)
 literals: ((& <interval>))
{
struct scheme_time t;

  if (OBJ_ISA_FIXNUM(isec))
    {
      t.usec = 0;
      t.sec = fx2int(isec);
      REG0 = make_time( &t, TLREF(0) );
    }
  else if (LONGFLOAT_P(isec))
    {
      IEEE_64 sec_d = extract_float(isec);
      double sec_i;
#if HAVE_ROUND
      t.usec = round( modf( sec_d, &sec_i ) * 1.0e6 );
#else
      t.usec = modf( sec_d, &sec_i ) * 1.0e6 + 0.5;
#endif
      t.sec = sec_i;
      REG0 = make_time( &t, TLREF(0) );
    }
  else 
    {
      REG0 = FALSE_OBJ;
    }
  RETURN1();
})

(define (epoch-seconds->time esec)
  (if (exact? esec)
      (exact->time esec)
      (inexact->time esec)))

(define-safe-glue (exact->time (esec <raw-int>))
 literals: ((& <time>))
{
  struct scheme_time t;
  t.usec = 0;
  t.sec = esec;
  REG0 = make_time( &t, TLREF(0) );
  RETURN1(); 
})

(define-safe-glue (inexact->time (esec <raw-float>))
 literals: ((& <time>))
{
  struct scheme_time t;
  double sec_i;

  t.usec = modf( esec, &sec_i ) * 1000000;
  t.sec = (int)esec;
  REG0 = make_time( &t, TLREF(0) );
  RETURN1();
})

(define-syscall-glue (time-microseconds (t <time>))
{
  REG0 = int2fx( t->usec );
  RETURN1();
})

(define-syscall-glue (time->epoch-seconds (t <time>))
{
  if (t->usec == 0 && (t->sec < (1<<29)))
    REG0 = int2fx(t->sec);
  else
    REG0 = make_float( t->usec * 1.0e-06 + t->sec );
  RETURN1();
})

(define-syscall-glue (interval->seconds (dt <interval>))
{
  if (dt->usec == 0 && (dt->sec < (1<<29)))
    REG0 = int2fx(dt->sec);
  else
    REG0 = make_float( dt->usec * 1.0e-06 + dt->sec );
  RETURN1();
})

(define-method write-object ((self <time>) port)
  (format port "#[<time> ~a]" (time->string self)))

(define-method display-object ((self <time>) port)
  (display (time->string self) port))


(define-method write-object ((self <interval>) port)
  (format port "#[<interval> ~a]" (interval->string self)))

(define-method display-object ((self <interval>) port)
  (display (interval->string self) port))

(define-syscall-glue (interval->string (interval <interval>))
{
struct scheme_time t = *interval;
char *p, temp[50];
double sec;

    p = temp;
    if (t.sec < 0)
    {
        *p++ = '-';
	if (t.usec == 0)
	    t.sec = -t.sec;
	else
	{
	    t.sec = -(t.sec + 1);
	    t.usec = 1000000 - t.usec;
	}
    }

    if (t.sec == 0)
    {
	if (t.usec < 1000)
	{
	    if (t.usec == 0)
	    {
		p[0] = '0';
		p[1] = 0;
	    }
	    else
		sprintf( p, "%d us", (int)t.usec );
	}
	else
	    sprintf( p, "%.3g ms", t.usec / 1000.0 );
    }
    else
    {
	sec = t.sec + t.usec * 1.0e-6;
	if (sec < 60.0)
	{
	    sprintf( p, "%.3g s", sec );
	}
	else if (sec < 3600.0)
	{
	    sprintf( p, "%.3g m", sec / 60.0 );
	}
	else if (sec < 24.0 * 3600.0)
	{
	    sprintf( p, "%.3g h", sec / 3600.0 );
	}
	else
	{
	    sprintf( p, "%.3g d", sec / (24.0 * 3600.0) );
	}
    }
    REG0 = make_string( temp );
    RETURN1();
})

(define-syscall-glue (time+interval (t <time>) (i <interval>))
 literals: ((& <time>))
{
struct scheme_time a;

    a.usec = t->usec + i->usec;
    a.sec = t->sec + i->sec;
    REG0 = make_time( &a, TLREF(0) );
    RETURN1();
})

(define-syscall-glue (interval+interval (a <interval>) (b <interval>))
 literals: ((& <interval>))
{
struct scheme_time r;

    r.usec = a->usec + b->usec;
    r.sec = a->sec + b->sec;
    REG0 = make_time( &r, TLREF(0) );
    RETURN1();
})

(define-syscall-glue (interval-interval (a <interval>) (b <interval>))
 literals: ((& <interval>))
{
struct scheme_time r;

    r.usec = a->usec - b->usec;
    r.sec = a->sec - b->sec;
    REG0 = make_time( &r, TLREF(0) );
    RETURN1();
})

(define-syscall-glue (negative-interval (a <interval>))
 literals: ((& <interval>))
{
struct scheme_time r;

    r.usec = -a->usec;
    r.sec = -a->sec;
    REG0 = make_time( &r, TLREF(0) );
    RETURN1();
})

(define-syscall-glue (time-time (a <time>) (b <time>))
 literals: ((& <interval>))
{
struct scheme_time r;

    r.usec = a->usec - b->usec;
    r.sec = a->sec - b->sec;
    REG0 = make_time( &r, TLREF(0) );
    RETURN1();
})

;;;  works on <time>'s and <interval>'s

(define-syscall-glue (scmtime-cmp t1o t2o req)
{
  struct scheme_time *t1 = PTR_TO_SCMTIME( t1o );
  struct scheme_time *t2 = PTR_TO_SCMTIME( t2o );
  int rel;
      
  if (t1->sec == t2->sec)
    {
      if (t1->usec < t2->usec)
	rel = 4;
      else if (t1->usec > t2->usec)
	rel = 1;
      else
	rel = 2;
    }
  else if (t1->sec < t2->sec)
    rel = 4;
  else if (t1->sec > t2->sec)
    rel = 1;
  else
    rel = 2;
  REG0 = rb_to_bo( fx2int(req) & rel );
  RETURN1();
})

(define (time<? (a <time>) (b <time>))  (scmtime-cmp a b #b100))
(define (time<=? (a <time>) (b <time>)) (scmtime-cmp a b #b110))
(define (time=? (a <time>) (b <time>))  (scmtime-cmp a b #b010))
(define (time>=? (a <time>) (b <time>)) (scmtime-cmp a b #b011))
(define (time>? (a <time>) (b <time>))  (scmtime-cmp a b #b001))

(define (interval<? (a <interval>) (b <interval>))  (scmtime-cmp a b #b100))
(define (interval<=? (a <interval>) (b <interval>)) (scmtime-cmp a b #b110))
(define (interval=? (a <interval>) (b <interval>))  (scmtime-cmp a b #b010))
(define (interval>=? (a <interval>) (b <interval>)) (scmtime-cmp a b #b011))
(define (interval>? (a <interval>) (b <interval>))  (scmtime-cmp a b #b001))


(define (clock-thunk thunk)
  (let ((a (clock)))
    (thunk)
    (let ((b (clock)))
      (interval-interval b a))))

(define (time-thunk thunk)
  (let ((a (time)))
    (thunk)
    (let ((b (time)))
      (time-time b a))))
