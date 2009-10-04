
(define *environ* #f)
(define *num-environ* 0)

;;;
;;; make sure these things are invalidated when we store an image
;;;


(%early-once-only
 (add-image-save-hook!
  (lambda ()
    (set! *environ* #f)
    (set! *num-environ* 0)
    (set! *c-environ-ok?* #t)
    (set! *c-environ-backup* #f)
    (set! *path* #f))))

(define *c-environ-ok?* #t)   ;; #f when C environ out of date
(define *c-environ-backup* #f) ;; for backing up environ
(define *path* #f)

(define (get-env key)
  (if *environ*
      (let ((p (assoc key *environ*)))
	(if p
	    (cdr p)
	    #f))
      (getenv key)))

(define (set-env key value)
  (let ((p (assoc key (the-environ))))
    (set! *c-environ-ok?* #f)
    (set! *path* #f)
    (if p
	(set-cdr! p value)
	(begin
	  (set! *num-environ* (+ *num-environ* 1))
	  (set! *environ* (cons (cons key value) *environ*))))))

(define (reset-env lst)
  (if (and (list? lst)
	   (every? (lambda (ent)
		     (and (pair? ent)
			  (string? (car ent))
			  (string? (cdr ent))))
		   lst))
      (begin
	(set! *environ* lst)
	(set! *num-environ* (length lst))
	(set! *path* #f)
	(set! *c-environ-ok?* #f))
      (error "reset-env: list invalid: ~s" lst)))

(define (unset-env (key <string>))
  (set! *c-environ-ok?* #f)
  (set! *path* #f)
  (let loop ((lst (the-environ)) (prev #f))
    (if (null? lst)
	#f
	(if (string=? (car (car lst)) key)
	    (begin
	      (if prev
		  (set-cdr! prev (cdr lst))
		  (set! *environ* (cdr lst)))
	      (set! *num-environ* (- *num-environ* 1))
	      (car lst))
	    (loop (cdr lst) lst)))))
    
;; passing an argument to export-environ or the-environ
;; forces a re-cache

(define-glue (export-environ)
 literals: ((& *c-environ-ok?*) (& *environ*) (& *num-environ*)
	    (& *c-environ-backup*)
	    "export-environ: internal error #~d")
{
  extern char **environ;

  if (EQ(TLREF(0),FALSE_OBJ) || (arg_count_reg > 0))
    {
      int i, n = fx2int(TLREF(2));
      obj lst, index, cindex;

      index = alloc( SLOT(n+1), vector_class );
      cindex = alloc( sizeof(char *) * (n+1), byte_vector_class );
      gvec_write_fresh( index, SLOT(n), cindex );
      environ = (char **)PTR_TO_DATAPTR(cindex);
      environ[n] = NULL;

      TLSET(0,TRUE_OBJ);
      TLSET(3,index);

      for (lst=TLREF(1),i=0; i<n; i++,lst=pair_cdr(lst))
	{
	  obj a, k, v;
	  char *d;
	  size_t klen, vlen;

	  if (!PAIR_P(lst))
	    scheme_error( string_text(LITERAL(4)), 1, int2fx(2) );
	  a = pair_car(lst);
	  if (!PAIR_P(a))
	    scheme_error( string_text(LITERAL(4)), 1, int2fx(3) );
	  k = pair_car(a); klen = string_length(k);
	  v = pair_cdr(a); vlen = string_length(v);
	  a = bvec_alloc( klen + vlen + 2, string_class );

	  if (!STRING_P(k))
	    scheme_error( string_text(LITERAL(4)), 1, int2fx(4) );
	  if (!STRING_P(v))
	    scheme_error( string_text(LITERAL(4)), 1, int2fx(5) );

	  d = (char *)PTR_TO_DATAPTR(a);
	  memcpy( d, PTR_TO_DATAPTR(k), klen );
	  d[klen] = '=';
	  memcpy( d + klen + 1, PTR_TO_DATAPTR(v), vlen );
	  d[klen+vlen+1] = 0;

	  gvec_write_fresh( index, SLOT(i), a );
	  environ[i] = d;
	}
      if (!EQ(lst,NIL_OBJ))
	scheme_error( string_text(LITERAL(4)), 1, int2fx(1) );
    }
  REG0 = TLREF(1);
  RETURN1();
})

(define-glue (the-environ)
 literals: ((& *environ*) "" (& *num-environ*))
{
  extern char **environ;
  obj first, last;
  int n;
  char **p, *b;

  first = TLREF(0);
  if (EQ(first,FALSE_OBJ) || (arg_count_reg > 0))
    {
      n = 0;
      first = last = NIL_OBJ;
      for (p=environ; *p; p++)
	{
	  char *e;
	  obj k, v;
	  size_t klen, vlen;
	  obj cell;

	  e = *p;
	  b = strchr(e,'=');
	  if (b)
	    {
	      klen = b - e;  /* doesn't include NUL */
	      vlen = strlen(b); /* includes NUL */
	      k = bvec_alloc( klen + 1, string_class );
	      v = bvec_alloc( vlen, string_class );
	      memcpy( PTR_TO_DATAPTR(k), e, klen );
	      memcpy( PTR_TO_DATAPTR(v), b+1, vlen );
	    }
	  else
	    {
	      k = make_string(e);
	      v = LITERAL(1);
	    }
	  cell = cons( cons( k, v ), NIL_OBJ );
	  if (EQ(first,NIL_OBJ))
	    first = cell;
	  else
	    gvec_write_fresh( last, SLOT(1), cell );
	  last = cell;
	  n++;
	}
      TLSET(2,int2fx(n));
      TLSET(0,first);
    }
  REG0 = first;
  RETURN1();
})

(define (find-in-path name)
  (if (not *path*)
      (set! *path* (string-split (get-env "PATH") #\:)))
  (let loop ((p *path*))
    (if (null? p)
	(error "~a: could not find along PATH" name)
	(let ((t (string-append (car p) "/" name)))
	  (if (file-access? t (access-mask execute))
	      t
	      (loop (cdr p)))))))
