
;(define-syntax (debug-ssl . body) #f)

(define *debug-ssl* #f) ; this turns on verbose mode for sslmgr, too

(define-syntax (debug-ssl . body) 
  (if *debug-ssl*
      (begin . body)
      #f))

;;;

(define-safe-glue (crack-mux-header (s <string>))
{
  unsigned long hdr = *(unsigned long *)PTR_TO_DATAPTR(s);
  REG0 = int2fx( (hdr >> 24) & 0xFF );
  REG1 = int2fx( hdr & 0x3FFFF );
  RETURN(2);
})

(define-safe-glue (make-mux-header (type <raw-int>) (len <raw-int>))
{
  obj s;
  unsigned long hdr;

  if ((type < 0) || (type > 0xFF)) {
    scheme_error( "make-mux-header: type ~s out of range", 1, raw_type );
  }
  if ((len < 0) || (len > 0x3FFFF)) {
    scheme_error( "make-mux-header: length ~s out of range", 1, raw_len );
  }

  hdr = ((type << 24) & 0xFF000000) + (len & 0x3FFFF);
  s = bvec_alloc( sizeof(hdr)+1, string_class );
  *(unsigned long *)PTR_TO_DATAPTR( s ) = hdr;
  REG0 = s;
  RETURN1();
})

(define-class <ssl-socket> (<object>)
  (properties init-value: '#())
  peer
  (certificate-var type: <async-variable>)
  plaintext-in
  plaintext-out
  mux-out
  process)

(define-method has-certificate? ((self <ssl-socket>))
  (async-variable-set? (certificate-var self)))

(define-method certificate ((self <ssl-socket>))
  (force (certificate-var self)))
      
      

(define-method input-port ((self <ssl-socket>))
  (plaintext-in self))

(define-method output-port ((self <ssl-socket>))
  (plaintext-out self))

(define $ctl-shutdown (string-append (make-mux-header #xCC 1) "c"))


(define-syntax (control-write self packet)
  (debug-ssl (format #t "*** >>> CONTROL ~s\n" (string-ref packet 4)))
  (write-string (mux-out self) packet))


(define-method close ((self <ssl-socket>))
  (control-write self $ctl-shutdown)
  (flush-output-port (mux-out self))
  ;; this should trigger the subprocess to exit
  (close-output-port (plaintext-out self))
  (check-exit-status (process self))
  ;;
  (close-input-port (plaintext-in self))
  ;; this is closed by the status-in monitoring thread when it sees EOF
  ;; (close-input-port (status-in self))
  (values))

(define-class <ssl-mux-output-port> (<output-port>)
  ;; XXX <ssl-mux-output-port> needs to be buffered, and only
  ;; send a data packet down when we are flushed or overflow
  mux-output)

(define-method close-output-port ((self <ssl-mux-output-port>))
  (close-output-port (mux-output self)))


(define-method flush-output-port ((self <ssl-mux-output-port>))
  ;; XXX TODO: ping the subprocess to make sure everything is flushed
  ;;     not just outside the process, but outside our process _cluster_
  (flush-output-port (mux-output self)))

(define-method output-port-write-char ((self <ssl-mux-output-port>)
                                       (ch <char>))
  (debug-ssl (format #t "*** >>> DATA ~#@*70s\n" ch))
  (qout-write-vec (mux-output self) 
                  (vector (make-mux-header #xDD 1) ch)))
  
(define-method write-string ((self <ssl-mux-output-port>) (str <string>))
  (let-syntax ((write-chunk (syntax-form (arg)
                              (let* (((chunk <string>) arg)
                                     ((n <fixnum>) (string-length chunk)))
                                (if (> n 0)
                                    (qout-write-vec 
                                     (mux-output self) 
                                     (vector
                                      (make-mux-header #xDD n)
                                      chunk)))))))
    (debug-ssl (format #t "*** >>> DATA ~#@*70s\n" str))
    (if (< (string-length str) 250000)
        (write-chunk str)
        (let loop ((i 0))
          (let ((j (+ i 250000)))
            (if (>= j (string-length str))
                (write-chunk (substring str i))
                (begin
                  (write-chunk (substring str i j))
                  (loop j))))))))


(%strategy bytecode
 (define (file-close fd) (fd-close fd))
)

(define *sslmgr-executable* "./sslmgr")


(define (ssl-connect (sockaddr <inet-socket-addr>) 
                     #key
                     (certinfo default: '())
                     (local default: #f)
                     (passphrase default: #f))
  (let ((s (internal-socket-connect sockaddr "ssl" local)))
    ;; sslmgr wants the socket in blocking mode...
    (fd-set-blocking (filedes s) #t)
    ;; create the subprocess and wrapper ports
    (make-sslmgr* (filedes s) sockaddr certinfo #f passphrase)))
  
(define (make-sslmgr cnx peer certinfo)
  (make-sslmgr* cnx peer certinfo #t #f))

(define (make-sslmgr* cnx peer certinfo server? pp)
  (bind ((r0 w0 (pipe))
         (r1 w1 (pipe))
         (rpin wpin (make-internal-pipe)))      ; pin="Plaintext INput"
    ;;
    (let ((proc (run* *sslmgr-executable*
                      `("-F" 
                        ,(if server?
                             "-pfdsrv:3" 
                             "-pfdclient:3")
                        ,@(or (debug-ssl '("-v")) '())
                        ,@certinfo) 
                      (vector r0 w1 2 cnx)))
          (mux-in (filedes->input-port r1 #t))
          (mux-out (filedes->output-port w0 #t)))
      ;;
      ;; mark the writable fd as non-blocking, which is (still)
      ;; required (at least on the write side) to avoid us blocking
      ;; in case our subprocess stops reading its input
      (fd-set-blocking w0 #f)
      ;;
      (file-close r0)
      (file-close w1)
      (file-close cnx)
      ;;
      (let* ((ssl (make <ssl-socket>
                        peer: peer
                        plaintext-in: rpin
                        mux-out: mux-out
                        certificate-var: (make-async-variable
                                          (~ "~a:cert" peer))
                        plaintext-out: (make <ssl-mux-output-port>
                                             mux-output: mux-out)
                        process: proc)))
        ;;
        (if pp
            (begin
              (format #t "Setting PASSPHRASE = ~s\n" pp)
              (set-property! ssl 'passphrase pp)))
        ;;
        (thread-resume
         (make-thread
          (lambda ()
            (handler-case
             (let loop ()
               (bind ((l (read-string mux-in 4))
                      (type len (crack-mux-header l))
                      (data (read-string mux-in len)))
                 (case type
                   ((#xCC)
                    (debug-ssl (format #t "*** <<< CONTROL ~s\n" data))
                    (process-control-packet ssl data))
                   ((#xDD)
                    (debug-ssl (format #t "*** <<< DATA ~s\n" data))
                    (write-string wpin data))
                   (else
                    (error "Bad framing header type: ~s" type)))
                 (loop)))
             ((<partial-read> condition: err)
              (debug-ssl (format #t "*** <<< EOF ~a\n" err))
              (values))
             ((<condition> condition: err)
              (debug-ssl (format #t "*** <<< ERROR ~a\n" err))
              (values)))
            (kill-async-variable! (certificate-var ssl))
            (close-input-port mux-in)
            (close-output-port wpin))
          (~ "ssl[~d].in" cnx)))
        ssl))))

(define control-header (reg-expr->proc '(seq (save (+ (not #\space)))
                                             (* #\space))))

(define (process-control-packet (ssl <ssl-socket>) data)
  (bind ((s e tag (control-header data)))
    (cond
     ((string=? tag "peer-cert")
      (let ((cert (substring data e)))
        (if (string=? cert "-none-")
            (set-async-variable! (certificate-var ssl) #f)
            (set-async-variable! (certificate-var ssl) cert))))
     ((string=? tag "passwd:private")
      (let (((p <string>) (get-property ssl 'passphrase)))
        (control-write ssl
                       (string-append 
                        (make-mux-header #xCC (+ 1 (string-length p)))
                        "p"
                        p))
        (flush-output-port (mux-out ssl))))
     (else
      ;; ignore it for Now...
      (values)))))

;;;
;;;  The passphrase needs to be set in munged form...

(define (mini-munge! str)
  (for-each
   (lambda (i)
     (let ((ch (string-ref str i)))
       (string-set! 
        str 
        i
        (cond
         ((char-upper-case? ch)
          (integer->char (+ (char->integer #\A)
                            (- (char->integer #\Z) (char->integer ch)))))
         ((char-lower-case? ch)
          (integer->char (+ (char->integer #\a)
                            (- (char->integer #\z)
                               (char->integer ch)))))
         (else
          ch)))))
   (range (string-length str)))
  str)
