(define-class <glue> (<object>)
  ;; TeX#150
  (natural type: <fixnum> init-value: 0)                ; natural width
  ;;
  (stretch type: <fixnum> init-value: 0)
  (stretch-order type: <fixnum> init-value: 0);0=normal, 1=fil, 2=fill, 3=filll
  ;;
  (shrink type: <fixnum> init-value: 0)
  (shrink-order type: <fixnum> init-value: 0));0=normal, 1=fil, 2=fill, 3=filll

(define-class <glue-set> (<object>)
  (glue-set-sign type: <fixnum> init-value: 0)
  (glue-set-order type: <fixnum> init-value: 0)
  (glue-set-ratio type: <real> init-value: 0))

;;;   we sometimes test for (eq? something $zero-glue) to elide
;;;   a glue-node or some such

(define-constant $zero-glue (make <glue>))
(define-constant $zero-glue-set (make <glue-set>))

;;;

(define (parfillskip)
  (make <glue>
        stretch: 2
        stretch-order: 1))

#|
(define (left-skip)
  $zero-glue)

(define (right-skip)
  $zero-glue)
|#

(define-method write-object ((self <glue>) port)
  (format port "#[<glue> ~d" (natural self))
  ;;
  (if (not (zero? (stretch self)))
      (format port " + ~d~a" 
              (stretch self)
              (vector-ref '#("" "fil" "fill" "filll") 
                          (stretch-order self))))
  ;;
  (if (not (zero? (shrink self)))
      (format port " - ~d~a" 
              (shrink self)
              (vector-ref '#("" "fil" "fill" "filll") 
                          (stretch-order self))))
  ;;
  (format port "]"))

(define-method to-string ((self <glue>))
  (string-append
   (to-string (natural self))
   ;;
   (if (not (zero? (stretch self)))
       (string-append "+"
                      (to-string (stretch self))
                      (vector-ref '#("" "fil" "fill" "filll") 
                                  (stretch-order self)))
       "")
  ;;
  (if (not (zero? (shrink self)))
      (string-append "-"
                     (to-string (shrink self))
                     (vector-ref '#("" "fil" "fill" "filll") 
                                 (stretch-order self)))
      "")))

(define fil-pattern (unformat->proc "~dfil"))
(define fill-pattern (unformat->proc "~dfill"))
(define filll-pattern(unformat->proc "~dfilll"))

(define (parse-stretch str)
  (cond
   ((string=? str "")
    (values 0 0))
   ((string->number str)
    (values (string->number str) 0))
   ((fil-pattern str) => (lambda (n) (values n 1)))
   ((fill-pattern str) => (lambda (n) (values n 2)))
   ((filll-pattern str) => (lambda (n) (values n 3)))
   (else
    (error "Could not parse glue ~s" str))))

(define (sxml->glue n)
  (bind ((natural (or (string->number (xpath-str n "@length")) 0))
         (stretch stretch-order (parse-stretch (xpath-str n "@stretch")))
         (shrink shrink-order (parse-stretch (xpath-str n "@shrink"))))
    (make <glue>
          natural: (qty->scaled natural)
          stretch: stretch
          stretch-order: stretch-order
          shrink: shrink
          shrink-order: shrink-order)))
