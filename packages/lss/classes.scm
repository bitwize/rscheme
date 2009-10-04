
(define-class <lss> (<object>)
  (lss-raw-ptr type: <fixnum>))

(define-class <lss-error> (<condition>)
  (lss-error-number type: <fixnum>))

(define-method display-object ((self <lss-error>) port)
  (format port "** LSS Error ~d **\n" (lss-error-number self)))

;;;

(define-lss-glue (lss-open* (file <raw-string>)
			    (writable <raw-bool>)
			    generation)
  literals: ((& <lss-error>))
{
  LSS *l;
  int opts = LSS_OPEN + (writable ? LSS_RDWR : LSS_RDONLY);

  if (truish(generation))
    {
      l = lss_open( file, opts + LSS_BACKLEVEL, fx2int( generation ) );
    }
  else
    {
      l = lss_open( file, opts );
    }
  if (!l)
    {
      raise_error( make2( TLREFB(0), NIL_OBJ, int2fx( errno ) ) );
    }
  REG0 = C_PTR_TO_OBJ(LSS *,l);
  RETURN1();
})

(define-lss-glue (lss-openx* (files <vector>) 
                             (gen <raw-int>)
                             (flags <raw-int>))
  literals: ((& <lss-error>))
{
  LSS *l;
  const char *(filev[LSS_MAX_VOLUMES+1]);
  int i, n;

  n = SIZEOF_PTR( files ) / SLOT(1);
  if (n > LSS_MAX_VOLUMES) {
    scheme_error( "lss-openx*: Too many volumes (~d with max of ~d) specified",
                  2, int2fx( n ), int2fx( LSS_MAX_VOLUMES ) );
  }

  for (i=0; i<n; i++)
    {
      obj entry = vector_ref( files, int2fx(i) );
      CHECK_STRING( entry );
      filev[i] = string_text( entry );
    }
  filev[n] = NULL;

  errno = 0;
  l = lss_openx( filev, gen, flags );

  if (!l)
    {
      raise_error( make2( TLREFB(0), NIL_OBJ, int2fx( errno ) ) );
    }
  REG0 = C_PTR_TO_OBJ(LSS *,l);
  RETURN1();
})

(define-lss-glue (lss-create* (file <raw-string>) (fmode <raw-int>))
  literals: ((& <lss-error>))
{
  LSS *l;

  errno = 0;
  l = lss_open( file, LSS_CREATE, fmode );

  if (!l)
    {
      raise_error( make2( TLREFB(0), NIL_OBJ, int2fx( errno ) ) );
    }
  REG0 = C_PTR_TO_OBJ(LSS *,l);
  RETURN1();
})

(define-lss-glue (lss-commit* (lss <lss>) (flag <raw-int>))
{
  REG0 = int_64_compact( int_32_to_int_64( lss_commit( lss, flag ) ) );
  RETURN1();
})

(define-lss-glue (lss-tune* (lss <lss>) 
                            (key <raw-string>) 
                            (value <raw-string>))
{
  int rc;
  rc = lss_tune( lss, key, value );
  if (rc < 0) {
    errno = -rc;
    os_error( "lss_tune", 2, raw_key, raw_value );
  }
  RETURN0();
})

(define-method lss-tune ((self <lss>) key value)
  (lss-tune* self key value))

(define-lss-glue (lss-read (lss <lss>) (record_num <raw-int>))
{
  LSSAccess *a = lss_read_access( lss, record_num );
  zipbuf b[2];
  obj str;
  size_t len;

  len = lss_access_bytes( a );
  str = bvec_alloc( len + 1, string_class );

  b[0].ptr = PTR_TO_DATAPTR( str );
  b[0].limit = (char *)b[0].ptr + len;
  b[1].ptr = NULL;

  lss_readv( lss, b, a );
  lss_read_release( lss, a );
  REG0 = str;
  RETURN1();
})

(define-lss-glue (lss-write* (lss <lss>) 
			     (record_num <raw-int>)
			     (data <bvec>)
			     (start <raw-int>)
			     (end <raw-int>)
			     zip)
{
  UINT_8 *ptr = data;
  zip_algorithm *za = NULL;

  if (truish(zip))
    za = lss_find_zip_algorithm( string_text(zip) );
  lss_write( lss, record_num, ptr + start, end - start, za );
  RETURN0();
})

(define-lss-glue (lss-delete (lss <lss>) (record_num <raw-int>))
{
  int rc;

  rc = lss_delete( lss, record_num );
  if (rc >= 0) {
    RETURN0();
  } else {
    REG0 = int2fx( rc );
    RETURN1();
  }
})

(define (lss-write (lss <lss>) 
		   (record-number <fixnum>) 
		   (str <string>) 
		   #optional zip)
  (lss-write* lss 
	      record-number
	      str
	      0
	      (string-length str)
	      zip))

(define-lss-glue (lss-file* (lss <lss>) (vol <raw-int>))
{
  REG0 = make_string( lss_filename( lss, vol ) );
  RETURN1();
})

(define (lss-file lss #optional (vol default: -1))
  (lss-file* lss vol))

(define-lss-glue (lss-close (lss <lss>))
{
  lss_close( lss );
  RETURN0();
})

;;;
;;;  somewhat more friendly procedures
;;;

(define (lss-create file #optional (mode default: #o666))
  (make <lss>
	lss-raw-ptr: (cond
                      ((string? file)
                       (lss-create* file mode))
                      ((vector? file)
                       (lss-openx* file
                                   0
                                   (bitwise-or (bitwise-and mode #o777)
                                               #x3000)))
                      (else
                       (error "lss-create: invalid file spec ~s" file)))))

(define (lss-commit lss #optional (flag default: 0))
  (lss-commit* lss flag))

(define (lss-open file #optional gen (read-only? default: #f))
  (make <lss>
	lss-raw-ptr: (cond
                      ((string? file)
                       (lss-open* file (not read-only?) gen))
                      ((vector? file)
                       (lss-openx* file 
                                   (or gen 0)
                                   (if read-only? 0 #x2000)))
                      (else
                       (error "lss-open: invalid file spec ~s" file)))))

