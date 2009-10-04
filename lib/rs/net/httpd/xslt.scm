
(define-class <xsl-transformer> (<object>)
  stylesheet)

(define (load-transformer file)
  ;; if we were actually internally compiling these,
  ;; we might return a cached transformer...
  (make <xsl-transformer>
        stylesheet: file))

(define-method xml-apply-transform ((self <xsl-transformer>) doc)
  (let* ((f (call-with-temp-file
             (lambda (port)
               (write-string port (sxml->string doc)))))
         (x (open-input-process (format #f "xsltproc ~a ~a" (stylesheet self) f)))
         (result (port->string x)))
    (close-input-port x)
    result))

(define-method xml-apply-transform-to-string ((self <xsl-transformer>) target)
  (let* ((f (call-with-temp-file
             (lambda (port)
               (write-string port target))))
         (x (open-input-process (format #f "xsltproc ~a ~a" (stylesheet self) f)))
         (result (port->string x)))
    (close-input-port x)
    result))

(define *tmp-id* 0)

(define (call-with-temp-file proc)
  (let ((pid (getpid)))
    (let loop ((k 0))
      (set! *tmp-id* (add1 *tmp-id*))
      (let* ((i *tmp-id*)
             (t (format #f "/tmp/tmp.~d.~d.~d" pid i k))
             (f (fd-open t 
                         (make-fd-open-mode 'write 'create 'exclusive)
                         #o600)))
        (if f
            (begin
              (fd-close f)
              (call-with-output-file t proc)
              t)
            (loop (random 1000)))))))
              
(define (xsl-transform stylesheet doc)
  (xml-apply-transform (load-transformer stylesheet) doc))
   
#|
  (let* ((f (call-with-temp-file
             (lambda (port)
               (write-string port doc))))
         (x (open-input-process (format #f "xsltproc ~a ~a" stylesheet f)))
         (result (port->string x)))
    (close-input-port x)
    result)
|#

;;;
