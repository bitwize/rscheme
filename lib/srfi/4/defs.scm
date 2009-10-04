(define-homogeneous-numeric-vector-type u8 <uint-8> 
  1 #f 
  bvec-ref 
  bvec-set!)

(define-homogeneous-numeric-vector-type s8 <int-8> 
  1 #t 
  bvec-read-signed-8 
  bvec-write-signed-8)

;;;

(define-homogeneous-numeric-vector-type u16 <uint-16> 
  2 #f 
  bvec-read-unsigned-16
  bvec-write-unsigned-16)

(define-homogeneous-numeric-vector-type s16 <int-16>
  2 #t
  bvec-read-signed-16
  bvec-write-signed-16)

;;;
#|... need unsigned-32 support
(define-homogeneous-numeric-vector-type u32 <uint-32> 
  4 #f 
  bvec-read-unsigned-32
  bvec-write-unsigned-32)
|#

(define-homogeneous-numeric-vector-type s32 <int-32>
  4 #t
  bvec-read-signed-32
  bvec-write-signed-32)

;;;

#|... need unsigned-64 support
(define-homogeneous-numeric-vector-type u64 <uint-64> 
  8 #f 
  bvec-read-unsigned-64
  bvec-write-unsigned-64)
|#

(define-syntax (write-s64 self index value)
  (let ((temp value))
    (bvec-write-float-64 self index (if (raw-int-> double-float? temp)
					temp
					(exact->inexact temp)))))

(define-homogeneous-numeric-vector-type s64 <int-64>
  8 #t
  bvec-read-signed-64
  bvec-write-signed-64)

;;;

#| ...NEED raw-float-32->raw-float...

(define-homogeneous-numeric-vector-type f32
  <ieee-32> 4 #t
  bvec-read-float-32 bvec-write-float-32)
|#

(define-syntax (write-f64 self index value)
  (let ((temp value))
    (bvec-write-float-64 self index (if (double-float? temp)
					temp
					(exact->inexact temp)))))

(define-homogeneous-numeric-vector-type f64
  <ieee-64> 8 #t
  bvec-read-float-64 write-f64)
