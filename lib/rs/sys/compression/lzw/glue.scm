(define-safe-glue (lzw-compress (str <string>) port)
  properties: ((other-c-files "lzwc.c")
               (other-h-files "lzwc.h"))
{
  LZWStream *s = lzw_create( port );
  lzw_run( s, string_text( str ), string_length( str ) );
  RETURN0();
})



#|
(define s (open-output-string))
(lzw-compress (file->string "/tmp/a.rast") s)
(outhex "/tmp/a.hex" (get-output-string s))
|#

#|
(define (outhex dest src)
  (call-with-output-file
      dest
    (lambda (port)
      (let loop ((i 0))
        (if (< i (string-length src))
            (begin
              (if (= (modulo i 32) 0)
                  (newline port))
              (format port "~02x" (bvec-ref src i))
              (loop (add1 i)))
            (newline port))))))
|#
      
(define (postscript-lzw-encode (src <string>))
  (let ((dest (open-output-string)))
    (lzw-compress src dest)
    (get-output-string dest)))

;;0000 00ff ffff 0000  00ff 0000 ffff ffff  | ................

#|
(postscript-lzw-encode "\0\0\0\377\377\377\0\0\0\377\0\0")

(compress (make-compressor) '(0 0 0 255 255 255 0 0 0 255 0 0))

|#
