
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Supporting Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def (<< n i)
  (logical-shift-left n i))

(def bs-append string-append)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Integer to byte string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def (get-byte n i)
  (if (= 0 i)
      (bitwise-and #b11111111 n)
      (get-byte (logical-shift-right n 8) (- i 1))))

(def (uint8->bs n)
  (string (integer->char n)))

(def (uint16->bs n)
  (list->string
   (map integer->char
	(list (get-byte n 1)
	      (get-byte n 0)))))

(def (uint32->bs n)
  (list->string
   (map integer->char
	(list (get-byte n 3)
	      (get-byte n 2)
	      (get-byte n 1)
	      (get-byte n 0)))))

(def (uint64->bs n)
  (list->string
   (map integer->char
	(list (get-byte n 7)
	      (get-byte n 6)
	      (get-byte n 5)
	      (get-byte n 4)
	      (get-byte n 3)
	      (get-byte n 2)
	      (get-byte n 1)
	      (get-byte n 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Byte string to integer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def (bs->uint16 bs)
  (+ (<< (char->integer (sref bs 0)) 8)
     (char->integer (sref bs 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Converting bit-fields to byte strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Function: (make-bs-formatter format)
;;
;; format - A list of bit-field sizes. The sum of the sizes should be
;; a multiple of 8.
;;
;; Returns a function which accepts a list of integers and packs them
;; according to the field format.
;;
;; Example:
;;
;; (define formatter (make-bs-formatter '(1 4 1 1 1 1 3 4)))
;;
;; (formatter (list qr opcode aa tc rd ra z rcode))

(def make-bs-formatter
  (let ((sizes->offsets
         (letrec ((helper
                   (fn (sizes index)
		     (if (null? sizes)
			 '()
                         (cons index
                               (helper (cdr sizes)
                                       (+ index (car sizes))))))))

           (fn (sizes)
             (reverse
              (helper (reverse sizes) 0))))))
    (fn (format)
      (let ((offsets (sizes->offsets format)))
        (fn (data)
          (apply + (map << data offsets)))))))

