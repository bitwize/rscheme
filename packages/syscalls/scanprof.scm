#|------------------------------------------------------------*-Scheme-*--|
 | File:    packages/syscalls/scanprof.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rosette.com>
 |          as part of the RScheme project, licensed for free use
 |
 | Version: 1.5
 | Date:    2003-06-12 21:53:57
 | Build:   v0.7.3.4-b7u, 2007-05-30
 |
 | Purpose: Read (parse) an RScheme profile dump file
 `------------------------------------------------------------------------|#

(define (with-profile-trace path proc-tbl)
  (with-profile-trace* path proc-tbl #f))

(define (with-profile-trace* path proc-tbl debug?)
  (let ((proc-vec (list->vector
		   (map (lambda (k)
			  (let ((p (assq k proc-tbl)))
			    (if p
				(let ((proc (cdr p)))
				  (if debug?
				      (lambda args
					(format #t "~s => ~s\n" k args)
					(apply proc args))
				      proc))
				(error "missing proc for profile entry: ~s" 
				       k))))
			'(eof
			  invalid
			  calls
			  returns
			  bjumps
			  jumps
			  fails
			  intr
			  start
			  done
			  gc
			  saves
			  captured
			  restored
			  def
			  cal-start
			  cal-stop
			  alloc
			  free
                          cal-realtime
                          mark
                          timepoint
			  frac))))
	(src (fopen path "r")))
    (fseek src 0 2)
    (let ((total-len (ftell src)))
      (fseek src 0 0)
      (let ((frac (inexact->exact (floor (/ total-len 50))))
	    (breaks (map (lambda (f)
			   (cons (* f 2) 
				 (inexact->exact 
				  (round (* total-len (/ f 50))))))
			 (cdr (range 51)))))
	(with-profile-trace**
	 proc-vec
	 src
	 breaks
	 (cdr (car breaks))
	 total-len)
	(fclose src)))))

(define (with-profile-trace** proc-vec src breaks next-break-posn total-len)
  (let loop ()
    (if (>= (ftell src) next-break-posn)
        (begin
          ((vector-ref proc-vec 22) (car (car breaks)))
          (set! breaks (cdr breaks))
          (if (pair? breaks)
              (set! next-break-posn (cdr (car breaks)))
              (set! next-break-posn (+ total-len 10)))))
    (if (%profile-parse-next src proc-vec)
        (loop))))

(define-safe-glue (%profile-parse-next src proc_vec)
  properties: ((other-h-files "<rscheme/profile.h>"))
  literals: ((& <time>))
{
  FILE *f = OBJ_TO_RAW_PTR(src);
  unsigned proc_n;
  obj proc, tmp_o, v = proc_vec;
  struct timeval tmp_t;
  UINT_32 tmp_len;
  struct RS_pr_header h;
  unsigned buf[300];
  int rc;

  /* by default, the proc gets no arguments */
  arg_count_reg = 0;

  if ((rc = fread( &h, 1, sizeof(h), f )) != sizeof(h)) {
    /* printf( "<rc %d>\n", rc ); */
    proc_n = 0;
    goto ok;
  }

#if 0
  printf( "<%02x '%c' %02x %04x> ", h.code, h.code, h.var_len, h.rec_bytes );
#endif
  if (h.rec_bytes > sizeof(h)) {
    int n = h.rec_bytes - sizeof(h);
    if (fread( (char *)&buf[1], 1, n, f ) != n) {
      proc_n = 0;
      goto ok;
    }
    *((struct RS_pr_header *)&buf[0]) = h;
  }

#if 0
  printf( "[" );
  {
   unsigned i;
    for (i=0; i<h.rec_bytes; i += 4) {
        printf( "%s%08x", (i==0) ? "" : " ", buf[i/4] );
    }
  }
  printf( "]\n" );
#endif

#define USE_RECORD(t)  struct RS_pr_ ## t *r = (struct RS_pr_ ## t *)&buf[0]

  switch (h.code)
    {
    default:
      proc_n = 1;
      break;

    case RSPROF_MT_CALLS:
      {
        USE_RECORD( MT_CALLS );
        REG0 = OBJ(VAL( r->tmpl ) - POINTER_TAG + FIXNUM_TAG);
        REG1 = int2fx( r->argc );
        arg_count_reg = 2;
      }
      proc_n = 2;
      break;

    case RSPROF_CAL_REALTIME:
      {
        USE_RECORD( CAL_REALTIME );
        REG0 = rsprof_tstamp_to_obj( r->tstamp, TLREFB(0) );
        REG1 = os_time( &r->systime, TLREFB(0) );
        REG2 = int2fx( r->recnum );
        arg_count_reg = 3;
      }
      proc_n = 19;
      break;

    case RSPROF_CAL_START:
      proc_n = 15;
      goto withtime;

    case RSPROF_CAL_STOP:
      proc_n = 16;
      goto withtime;

    case RSPROF_MT_RETURNS:
      proc_n = 3;
      break;

    case RSPROF_MT_BJUMPS:
      proc_n = 4;
      break;

    case RSPROF_MT_JUMPS:
      proc_n = 5;
      break;

    case RSPROF_MT_FAILS:
      proc_n = 6;
      break;

    case RSPROF_MT_INTR:
      proc_n = 7;
      break;

    case RSPROF_MT_START:
      {
        USE_RECORD( MT_START );
        REG0 = OBJ(VAL( r->tmpl ) - POINTER_TAG + FIXNUM_TAG);
        REG1 = rsprof_tstamp_to_obj( r->tstamp, TLREFB(0) );
        arg_count_reg = 2;
      }
      proc_n = 8;
      break;

    case RSPROF_PROF_MARK:
      proc_n = 20;
      {
        USE_RECORD( PROF_MARK );
        REG0 = int2fx( r->code );
        REG1 = int2fx( r->data1 );
        REG2 = int2fx( r->data2 );
        arg_count_reg = 3;
      }
      break;

    case RSPROF_TIMEPOINT:
      proc_n = 21;
      {
        USE_RECORD( TIMEPOINT );
        REG0 = int2fx( r->id );
        REG1 = rsprof_tstamp_to_obj( r->tstamp, TLREFB(0) );
        arg_count_reg = 2;
      }
      break;

    case RSPROF_MT_DONE:
      proc_n = 9;
    withtime:
      {
        USE_RECORD( MT_DONE );
        REG0 = rsprof_tstamp_to_obj( r->tstamp, TLREFB(0) );
        arg_count_reg = 1;
      }
      break;

    case RSPROF_GC_WORK:
      proc_n = 10;
      goto withtime;

    case RSPROF_SAVES:
      proc_n = 11;
      break;

    case RSPROF_CAPTURED:
      proc_n = 12;
    withobj:
      {
        USE_RECORD( CAPTURED );
        REG0 = OBJ(VAL( r->contn ) - POINTER_TAG + FIXNUM_TAG);
        arg_count_reg = 1;
      }
      break;

    case RSPROF_RESTORED:
      proc_n = 13;
      goto withobj;

    case RSPROF_DECL_NAME:
      proc_n = 14;
      {
        USE_RECORD( DECL_NAME );

        REG0 = OBJ(VAL(r->item) - POINTER_TAG + FIXNUM_TAG);
	REG1 = bvec_alloc( h.var_len+1, string_class );
	memcpy( string_text(REG1), &r->name[0], h.var_len );
	arg_count_reg = 2;
	break;
      }

    case RSPROF_OBJ_ALLOCED:
      proc_n = 17;
      {
        USE_RECORD( OBJ_ALLOCED );
        REG0 = OBJ(VAL(r->item) - POINTER_TAG + FIXNUM_TAG);
        REG1 = OBJ(VAL(r->item_class) - POINTER_TAG + FIXNUM_TAG);
        REG2 = int2fx( r->bytes );
        arg_count_reg = 3;
      }
      break;

    case RSPROF_OBJ_DIED:
      proc_n = 18;
      {
        USE_RECORD( OBJ_DIED );
        REG0 = OBJ(VAL(r->item) - POINTER_TAG + FIXNUM_TAG);
        arg_count_reg = 1;
      }
      break;

    }
ok:
  APPLY( arg_count_reg, gvec_ref( v, SLOT(proc_n) ) );
})
