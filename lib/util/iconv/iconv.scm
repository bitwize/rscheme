

(define-method write-bytes ((self <output-port>) bvec offset len)
  (let ((x (make-string len)))
    (bvec-copy x 0 bvec offset len)
    (write-string self x)))

(define-class <iconv-error> (<condition>)
  errno)

(define-method display-object ((self <iconv-error>) port)
  (format port "iconv error (~d) ~a\n"
          (errno self)
          (errno-message (errno self))))

(define-class <iconv-desc> (<object>) :bvec)

;;;
;;;  Note the wierd (i.e., backwards) order of the arguments
;;;  to iconv-open.  I left them this way because that is
;;;  the order of arguments to the underlying iconv_open().

(define-safe-glue (iconv-open (to <raw-string>) (from <raw-string>))
  properties: ((other-h-files "<iconv.h>"))
  literals: ((& <iconv-desc>))
{
  iconv_t i;
  obj d;

  i = iconv_open( to, from );
  if (i == (iconv_t)-1) {
    RETURN0();
  } else {
    d = bvec_alloc( sizeof( i ), TLREF(0) );
    *((iconv_t *)PTR_TO_DATAPTR(d)) = i;
    REG0 = d;
    RETURN1();
  }
})

(define-safe-glue (iconv-close (self <iconv-desc>))
{
  iconv_close( *((iconv_t *)PTR_TO_DATAPTR(self)) );
  RETURN0();
})

(define-safe-glue (iconv-bytes (self <iconv-desc>) 
                               s (soffset <raw-int>) (slen <raw-int>)
                               d (doffset <raw-int>) (dlen <raw-int>))
  properties: ((other-h-files "<iconv.h>" "<errno.h>"))
  literals: ((& <iconv-error>))
{
  iconv_t i = *((iconv_t *)PTR_TO_DATAPTR(self));
  size_t n, sn, dn;
  char *sp, *dp;

  CHECK_BVEC( s );
  CHECK_BVEC( d );

  sp = ((char *)PTR_TO_DATAPTR(s)) + soffset;
  dp = ((char *)PTR_TO_DATAPTR(d)) + doffset;
  sn = slen;
  dn = dlen;

  n = iconv( i, &sp, &sn, &dp, &dn );
  if ((((int)n) < 0) && (errno != E2BIG)) {
    obj prop = NIL_OBJ;
    raise_error( make2( TLREF(0), prop, int2fx(errno) ) );
  }

  REG0 = int2fx( n );
  REG1 = int2fx( sn );
  REG2 = int2fx( dn );
  RETURN(3);
})

(define (unicode-string->utf8 (str <unicode-string>))
  (let ((cnv (iconv-open "UTF8" "UCS-2"))
        (buf (make-string 30))
        (o (open-output-string)))
    ;;
    (let loop ((i 0)
               (n (- (bvec-length str) 2)))
      (if (= n 0)
          (begin
            (iconv-close cnv)
            (close-output-port o))
          (bind ((n sn dn (iconv-bytes cnv str i n buf 0 30)))
            (write-bytes o buf 0 (- 30 dn))
            (loop (+ i (- n sn)) sn))))))

(define (utf8->unicode-string (str <string>))
  (if (string=? str "")
      (let ((x (bvec-alloc <unicode-string> 2)))
        (bvec-write-unsigned-16 x 0 0)
        x)
      ;;
      (let* ((cnv (iconv-open "UCS-2" "UTF8"))
             (N (* 2 (string-length str)))
             (buf (make-string N))
             (o #f))
        ;;
        (define (finish s #optional n)
          (let* ((n (or n (string-length s)))
                 (x (bvec-alloc <unicode-string> (+ 2 n))))
            (bvec-copy x 0 s 0 n)
            (bvec-write-unsigned-16 x n 0)
            (iconv-close cnv)
            x))
        ;;
        (let loop ((i 0)
                   (n (string-length str)))
          (if (= n 0)
              ;; given the over-estimation of `N', it's not clear
              ;; how we would ever get here, since we already handled
              ;; the empty string case...
              (finish (close-output-port o))
              (bind ((nx sn dn (iconv-bytes cnv str i n buf 0 N)))
                (if (and (= i 0) (= sn 0))
                    ;; did it all in one conversion!
                    (finish buf (- N dn))
                    (begin
                      (if (not o)
                          (set! o (open-output-string)))
                      (write-bytes o buf 0 (- N dn))
                      (loop (+ i (- n sn)) sn)))))))))


      
