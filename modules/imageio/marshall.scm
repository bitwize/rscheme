#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/imageio/marshall.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.7
 | File mod date:    2007-01-28 10:02:09
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  imageio
 |
 `------------------------------------------------------------------------|#

(define (assign-ids sections rplc)
  (let ((idt (make-object-table)))
    (let ((i 0))
      (vector-for-each
       (lambda (v)
         (vector-for-each
          (lambda (v)
            (table-insert! idt v i)
            (set! i (+ i 1)))
          v))
       sections)
      ;;
      ;;  If object A is being replaced by B
      ;;  (i.e., A-->B appears in the rplc mapping)
      ;;  then give A the same id that B has
      ;;
      (if rplc
          (table-for-each
           rplc
           (lambda (h k v)
             (let ((j (table-lookup idt v)))
               (if j
                   (table-insert! idt k j))))))
      ;;
      (values idt i))))

(define (image->string img rws root #optional rplc)
    (let* ((info (render-marshalling img (assign-ids img rplc) rws root))
     	   (len (vector-ref info 0))
    	   (buffers (vector-ref info 1))
	   (errors (vector-ref info 2)))
	(if (null? errors)
	    (let ((b (bvec-alloc <string> (+ len 1))))
		(compact-buffers b 0 buffers)
		b)
	    errors)))

;;  info[2] is a list of illegal refs
;;
(define (image->compressed-string img rws root #optional rplc)
  ;;
  ;; `rws' is an <object-table> mapping instances
  ;; to a list of slots that are to be rewritten.
  ;;
  ;; This allows a slot whose current value is an immob
  ;; to be rewritten on output, or when multiple slots
  ;; point to the same value they can be split to point
  ;; to different objects.

  (let ((info (render-marshalling img (assign-ids img rplc) rws root)))
    (if (null? (vector-ref info 2))
        (compress (vector-ref info 1))
        (error "image->compressed-string: refs remain: ~s"
               (vector-ref info 2)))))

(define (decompress str)
    (let ((info (uncompress str)))
	(let ((b (bvec-alloc <string> (+ (vector-ref info 0) 1))))
	    (compact-buffers b 0 (vector-ref info 1))
	    b)))

(define *anchor-table* #f)

(define (pickle item #optional rplc)
  (let ((tbl (make-object-table))
	(rws (make-object-table))
	(sections (vector '() '() '() (or rplc (make-object-table)))))
    (fluid-let ((*anchor-table* (make-table eq? integer->hash)))
      (pickle* item tbl rws sections))
    (values (vector (list->vector (vector-ref sections 0))
                    (list->vector (vector-ref sections 1))
                    (list->vector (vector-ref sections 2)))
	    rws)))

(define (pickle* item tbl rws sections)
  (cond
   ((table-lookup tbl item)
    ;; it's been handled already
    (values))
   ((table-lookup (vector-ref sections 3) item)
    => (lambda (alt)
         (pickle* alt tbl rws sections)))
   ((ptr? item)
    (table-insert! tbl item #t)
    ;;(format #t "pickle* ~r ~#*30s (~s)\n" item item pickler)
    ((pickler item) item tbl rws sections))))

(define (pickle/ref item tbl rws sections)
    (vector-set! sections 
		    0 
		    (cons item (vector-ref sections 0))))

(define (pickle/gvec item tbl rws sections)
    (vector-set! sections 
		    1 
		    (cons item (vector-ref sections 1)))
    (let loop ((i (gvec-length item)))
	(if (eq? i 0)
	    #t
	    (let ((i-1 (- i 1)))
		(pickle* (gvec-ref item i-1) tbl rws sections)
		(loop i-1))))
    (pickle* (object-class item) tbl rws sections))

(define (pickle/template item tbl rws sections)
    (vector-set! sections 
		 1 
		 (cons item (vector-ref sections 1)))
    ;;
    (let ((li (linkage-info item))
	  (cp (code-pointer item)))
      (let ((f (if (fixnum? li)
		   (get-fn-descr-anchor *anchor-table* li)
		   li))
	    (c (if (fixnum? cp)
		   (if (fixnum? li)
		       (get-code-ptr-anchor *anchor-table* li cp)
		       (error "~s: template has code-pointer but no linkage-info" item))
		   cp)))
	(vector-set! sections 
		     0 
		     (cons* f c (vector-ref sections 0)))
	(table-insert! rws 
		       item
		       (list (cons 0 c) 
			     (cons 1 f)))))
    ;;
    (let loop ((i (gvec-length item)))
	(if (eq? i 2)
	    #t
	    (let ((i-1 (- i 1)))
		(pickle* (gvec-ref item i-1) tbl rws sections)
		(loop i-1))))
    (pickle* (object-class item) tbl rws sections))

(define (pickle/bvec item tbl rws sections)
    (vector-set! sections 
		    2 
		    (cons item (vector-ref sections 2)))
    (pickle* (object-class item) tbl rws sections))

(define (pickler item)
  (cond
    ((class? item)
	pickle/ref)
    ((symbol? item)
	pickle/ref)
    ((instance? item <template>)
	pickle/template)
;    ((instance? item <descriptor>)
;	pickle/descriptor)
    ((bvec? item)
	pickle/bvec)
    ((gvec? item)
	pickle/gvec)
    (else
	(abort 'pickler
		"Don't know how to pickle: ~s" item))))

(define (file->datum f)
    (call-with-input-file f
	(lambda (in)
	    (let loop ((r '()))
		(let ((d (read in)))
		    (if (eof-object? d)
			(reverse r)
			(loop (cons d r))))))))

(define (pickle->compressed-string root)
    (bind ((sections rws (pickle root))
    	   (info (image->compressed-string sections rws root))
	   (result (bvec-alloc <string> (+ (vector-ref info 0) 1))))
	(format #t "refs: ~s\n" (vector-ref sections 0))
	(compact-buffers result 0 (vector-ref info 1))
	result))

(define (pickle->string root)
  (bind ((sections rws (pickle root)))
    (image->string sections rws root)))

(define (unpickle src refs)
    (derender-unmarshalling src refs))
