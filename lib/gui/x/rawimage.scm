
;;; put-raw-image 
;;; drawable gcontext data 
;;; &key (:start 0) :depth :x :y :width :height (:left-pad 0) :format 

(define (put-raw-image (drawable <x-drawable>)
                       (gcontext <x-gcontext>)
                       data
                       #key (start default: 0)
                            depth
                            x
                            y
                            width
                            height
                            (left-pad default: 0)
                            format)
  (bind ((n (bvec-length data))
         (pad-n pad-str (pad4 n)))
    (internal-send
     (x-display drawable)
     (vector (make-buffer u1: 72 ; PutImage
                          u1: (vmemq format '#(bit-image
                                               xy-image
                                               z-image))
                          u2: (+ 6 (quotient (+ n pad-n) 4))
                          u4: (x-id drawable)
                          u4: (x-id gcontext)
                          u2: width
                          u2: height
                          s2: x
                          s2: y
                          u1: left-pad
                          u1: depth
                          u2: 0)
             data
             pad-str))))
