#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/lowscm/dequeue.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.2
 | File mod date:    1997-11-29 23:10:37
 | System build:     v0.7.3.4-b7u, 2007-05-30
 |
 `------------------------------------------------------------------------|#

;;
;;  a dequeue implemented with vectors
;;

(define-class <dequeue> (<object>)
  (state type: <vector>)
  (front type: <fixnum> init-value: 0)
  (back type: <fixnum> init-value: 0))

(define-method write-object ((self <dequeue>) port)
  (format port "#[<dequeue> ~d: ~j]" 
	  (dequeue-count self) 
	  (dequeue-state self)))

(define (make-dequeue)
  (make <dequeue>
	state: (make-vector 5)
	front: 0
	back: 0))

(define (dequeue-empty? (dq <dequeue>))
  (eq? (front dq) (back dq)))

(define (dequeue-count (dq <dequeue>))
  (let ((len (- (front dq) (back dq))))
    (if (< len 0)
	(+ len (vector-length (state dq)))
	len)))

(define (dequeue-state (dq <dequeue>))
  (let (((f <fixnum>) (front dq))
	((l <fixnum>) (vector-length (state dq)))
	((v <vector>) (state dq)))
    ;;
    (let loop (((i <fixnum>) (back dq))
	       (r '()))
      (if (eq? i f)
	  r
	  (if (eq? i l)
	      (loop 0 r)
	      (loop (add1 i) (cons (vector-ref v i) r)))))))

(define (expand-dequeue (dq <dequeue>))
  ;; crude implementation
  (let ((v (vector-append (list->vector (reverse (dequeue-state dq)))
			  '#(#f #f #f #f #f))))
    (set-front! dq (dequeue-count dq))
    (set-back! dq 0)
    (set-state! dq v)))

;;

(define-syntax (dq-inc dq i)
  (if (eq? (add1 i) (vector-length (state dq)))
      0
      (add1 i)))

(define-syntax (dq-dec dq i)
  (if (eq? i 0)
      (sub1 (vector-length (state dq)))
      (sub1 i)))

(define-syntax (assert-non-empty dq)
  (if (eq? (back dq) (front dq))
      (error "dequeue is empty")))
  
;;

(define (push-back! (dq <dequeue>) item)
  (let* (((oldb <fixnum>) (back dq))
	 ((newb <fixnum>) (dq-dec dq oldb)))
    (if (eq? newb (front dq))
	(begin
	  (expand-dequeue dq)
	  (push-back! dq item))
	(begin
	  (vector-set! (state dq) newb item)
	  (set-back! dq newb)))))


(define (push-front! (dq <dequeue>) item)
  (let* (((oldf <fixnum>) (front dq))
	 ((newf <fixnum>) (dq-inc dq oldf)))
    (if (eq? newf (back dq))
	(begin
	  (expand-dequeue dq)
	  (push-front! dq item))
	(begin
	  (vector-set! (state dq) oldf item)
	  (set-front! dq newf)))))

(define (pop-front! (dq <dequeue>))
  (assert-non-empty dq)
  (let* (((oldf <fixnum>) (front dq))
	 ((newf <fixnum>) (dq-dec dq oldf)))
    (set-front! dq newf)
    (vector-ref (state dq) newf)))

(define (pop-back! (dq <dequeue>))
  (assert-non-empty dq)
  (let* (((oldb <fixnum>) (back dq))
	 ((newb <fixnum>) (dq-inc dq oldb)))
    (set-back! dq newb)
    (vector-ref (state dq) oldb)))
