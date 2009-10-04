,(use tables)
,(use rs.db.rstore)
,(use rs.db.lss)
,(use sort)

(define (debug . args)
  ;(apply format #t args)
  (values))


;;;
;;;  A garbage collector for an RScheme persistent store (rstore)
;;;

;;;
;;;  This GC uses an rstore for storing its own state.  Here,
;;;  we define the data model
;;;

;;;
;;;  an <object-map> is a vector of offsets (object start locations)
;;;  of objects.  The low two bits of the offset are used as follows:
;;;
;;;      bit 0 = gray
;;;      bit 1 = black
;;;
;;;  an interior page of a large object has an empty <object-map>

(define-class <object-map> (<object>) :gvec)

(define-class <page> (<object>)
  (page-num type: <fixnum>)             ; base_page_num
  ;;
  ;;  encode the other part of a PageRef
  ;;
  ;;           17               2     1         0
  ;;          +------------------+----------+-------+
  ;;          |     nth_page     | indirect | first |
  ;;          +------------------+----------+-------+
  ;;  which is the same representation used in [RStore-3.0]
  ;;
  (page-ref type: <fixnum>)
  ;;
  (in-gray-list? init-value: #f)
  ;;
  ;;  record where the objects are in this page
  ;;
  (object-map type: <object-map>))

(define-method write-object ((self <page>) port)
  (format port "#[~a ~#x]" 
          (class-name (object-class self)) 
          (page-num self)))

(define-class <first-page> (<page>)
  (large-obj-length type: <fixnum>)
  (large-obj-swiz-mode type: <fixnum>))

(define-class <interior-page> (<page>)
  (first-page type: <first-page>))

(define-class <gc-state> (<object>)
  ;;
  ;; the target pstore which we are scanning
  ;;
  target-pstore                 ; type: (union <string> <vector>)
  target-generation             ; generation
  ;;
  ;; this contains entries for all the persistent pages;
  ;; a given page number maps to either the symbol 'unknown
  ;; or a <page> object
  ;;
  (page-index type: <hash-integer-table> init-function: make-fixnum-table)
  ;;
  ;;  a list of pages that have gray objects
  ;;
  (gray-pages init-value: '())
  ;;
  ;;  the rich-model used by the target repository
  ;;
  (target-rich-model init-value: #f)
  ;;
  ;;  a map of swizzle modes for the application
  ;;
  (swizzle-mode-table init-value: #f))

;;;

(define-class <active-cycle> (<object>)
  (backing-store type: <persistent-store>)
  (persistent-state type: <gc-state>)
  (target-lss type: <lss>))

;;;

(define (bind-pstore-to-classes sto)
  (register-indirect-page sto 1 (vector <gc-state>
                                        <object-map>
                                        <page>
                                        <first-page>
                                        <interior-page>)))

(define (restart-gc-cycle (backing-store <string>))
  (let ((sto (open-persistent-store backing-store)))
    (bind-pstore-to-classes sto)
    (let ((s (root-object sto)))
      (make <active-cycle>
            backing-store: sto
            persistent-state: s
            target-lss: (lss-open (target-pstore s) 
                                  (target-generation s)
                                  #t)))))

(define (create-gc-cycle
         (backing-store <string>)       ; where to put the GC's backing store
         generation
         target                         ; who to scan
         pivot-supplier)                ; fn to call to get pivot vectors
  (let* ((lss (lss-open target generation #t))
         (sto (create-persistent-store backing-store))
         (area (default-allocation-area sto))
         (boot (pstore-meta-identify-rich-model lss))
         (s (make <gc-state>
                  %alloc-area: area
                  target-rich-model: (car boot)
                  target-pstore: target
                  target-generation: (or generation (lss-get-generation lss))
                  swizzle-mode-table: (pstore-meta-std-indirects))))
    (bind-pstore-to-classes sto)
    (pivot-supplier
     (lambda (indir-page-num pivot-vec)
       (format #t "adding ~d entries to indirect pivot table[~d]\n"
               (vector-length pivot-vec)
               indir-page-num)
       (pstore-meta-insert-indirects (swizzle-mode-table s)
                                     indir-page-num
                                     pivot-vec)))
    (commit sto s)
    (let ((ac (make <active-cycle>
                    backing-store: sto
                    persistent-state: (root-object sto)
                    target-lss: lss)))
      (init-gray-set ac (cadr boot) (cddr boot))
      ac)))

;;;
;;;

(define (lookup-page (ac <active-cycle>) page-num page-ref)
  (let* (((ps <gc-state>) (persistent-state ac))
         (pg (table-lookup (page-index ps) page-num)))
    (cond
     (pg pg)
     ((eq? page-ref #b101)
      (new-page/normal ac ps page-num))
     ((eq? (bitwise-and page-ref #b11) #b01)
      (new-page/first ac ps page-num page-ref))
     (else
      (error "don't know how to handle non-first page in lookup-page")))))

(define (make-new-page ps maker starts)
  (let* (((v <object-map>) (gvec-alloc <object-map> (length starts) 0))
         (pg (maker v)))
    (table-insert! (page-index ps) (page-num pg) pg)
    (let loop (((i <fixnum>) 0)
               (l (reverse starts)))
      (if (null? l)
          pg
          (begin
            (gvec-set! v i (logical-shift-left (car l) 2))
            (loop (add1 i) (cdr l)))))))

(define (new-page/first (ac <active-cycle>) (ps <gc-state>) page-num page-ref)
  (let ((f (make-new-page ps 
                          (lambda (v)
                            (make <first-page>
                                  page-num: page-num
                                  page-ref: page-ref
                                  object-map: v
                                  large-obj-length: -1
                                  large-obj-swiz-mode: -1))
                          '(#x2b))))
    (for-each
     (lambda (k)
       (make-new-page ps
                      (lambda (v)
                        (make <interior-page>
                              page-num: (+ page-num k)
                              page-ref: (logical-shift-left k 2)
                              object-map: v
                              first-page: f))
                      '(0)))
     (cdr (range (logical-shift-right page-ref 2))))
    f))

(define (new-page/normal (ac <active-cycle>) (ps <gc-state>) page-num)
  (make-new-page ps
                 (lambda (v)
                   (make <page>
                         page-num: page-num
                         page-ref: #b101
                         object-map: v))
                 (cdr (pstore-meta-scan-starts (target-rich-model ps)
                                               (target-lss ac)
                                               page-num
                                               (swizzle-mode-table ps)))))

(define (init-gray-set (ac <active-cycle>) page-num offset)
  (debug "mark-as-gray: <~x:~x>\n" page-num offset)
  (mark-gray ac (lookup-page ac page-num #b101) offset))

(define (mark-gray (ac <active-cycle>) (pg <page>) offset)
  (let* (((m <object-map>) (object-map pg))
         ((n <fixnum>) (gvec-length m)))
    (let loop (((i <fixnum>) 0))
      (if (fixnum<? i n)
          (let (((e <fixnum>) (gvec-ref m i)))
            (if (eq? (logical-shift-right e 2) offset)
                (begin
                  (debug "  mark-gray <~x:~x> -- flags ~b\n"
                          (page-num pg)
                          offset
                          (bitwise-and e #b11))
                  (if (eq? (bitwise-and e #b11) 0)
                      (begin
                        ;; mark the object on this page as GRAY
                        (gvec-set! m i (bitwise-or e #b1))
                        ;; put this page on the global GRAY list
                        (if (not (in-gray-list? pg))
                            (let* ((ps (persistent-state ac))
                                   (ops (object-pages ps pg)))
                              (debug " object pages: ~s\n" ops)
                              (for-each
                               (lambda (p)
                                 (set-in-gray-list?! p #t))
                               ops)
                              (set-gray-pages! ps 
                                               (append! (gray-pages ps) ops))))
                        (values)))
                  (values))
                (loop (add1 i))))
          (error "object <~x:~x> -- could not find on page!" 
                 (page-num pg)
                 offset)))))

;;; return a list of pages to be marked gray if an object
;;; on the given page is marked gray.  This is normally only
;;; the page itself, but is all of the pages of a large object

(define (object-pages ps (pg <page>))
  (if (eq? (page-ref pg) #b101)
      (list pg)
      (map (lambda (i)
             (let ((p (table-lookup (page-index ps) (+ i (page-num pg)))))
               (gvec-set! (object-map p)
                          0 
                          (bitwise-or (gvec-ref (object-map p) 0) #b1))
               p))
           (range (logical-shift-right (page-ref pg) 2)))))

;;;
;;;  Build an ordered list of gray objects on a page
;;;  (mark them black in the process!)
;;;

(define (get-gray-list-and-mark-black (pg <page>))
  (let* (((m <object-map>) (object-map pg))
         ((n <fixnum>) (gvec-length m))
         (top (cons #f '())))
    (let loop (((i <fixnum>) 0)
               (prev top))
      (if (fixnum<? i n)
          (let (((e <fixnum>) (gvec-ref m i)))
            (loop (add1 i)
                  (if (eq? (bitwise-and e #b1) #b1)
                      (let ((n (cons (logical-shift-right e 2) '())))
                        (set-cdr! prev n)
                        (gvec-set! m i (bitwise-or (bitwise-and e #x3FFFC) 
                                                   #b10))
                        n)
                      prev)))
          (cdr top)))))

;;;
;;;  Blacken the gray objects on a page
;;;

(define (blacken-page-grays (ac <active-cycle>) (pg <page>))
  (debug "/============= blacken page grays: ~s ===\n" pg)
  (let ((e (blacken-page-grays* ac pg)))
    (debug "\\============= blacken page grays: ~s === effort = ~s\n" pg e)
    e))
           
(define (blacken-page-grays* (ac <active-cycle>) (pg <page>))
  (let (((R <fixnum>) (page-ref pg))
        (gray-objs (get-gray-list-and-mark-black pg)))
    (debug "blackening <~x:*> : ~d objects ~s\n" 
            (page-num pg) 
            (length gray-objs)
            gray-objs)
    ;; short circuit if nothing to do
    (if (null? gray-objs)
        0
        (cond
         ((eq? R #b101)
          (blacken-page-grays/normal ac pg gray-objs))
         ((= (bitwise-and R #b11) #b00)
          (blacken-page-grays/interior ac pg))
         ((= (bitwise-and R #b11) #b01)
          (blacken-page-grays/first ac pg))
         (else
          (error "Don't know how to deal with page-ref ~x for ~s" R pg))))))


(define (blacken-page-grays/first (ac <active-cycle>) (pg <first-page>))
  (let* (((ps <gc-state>) (persistent-state ac))
         (q (pstore-meta-scan-first-pp (target-rich-model ps)
                                       (target-lss ac)
                                       (page-num pg)
                                       (swizzle-mode-table ps))))
    (lookup-page-refs ac (car q))
    (set-large-obj-length! pg (caadr q))
    (set-large-obj-swiz-mode! pg (cdadr q))
    (grayify-pointers ac (cddr q))
    ;; return a metric of the work we did
    (length (cddr q))))

(define (grayify-pointers ac ptr-list)
  (for-each
   (lambda (ptr)
     (debug "  PTR: ~s\n" ptr)
     (let ((pref (car ptr))
           (offset (cdr ptr)))
       (if (vector-ref pref 2)
           (mark-gray ac (vector-ref pref 2) offset))))
   ptr-list))

(define (size-on-page (pg <interior-page>))
  (let ((N (- (large-obj-length (first-page pg)) 
              #x28
              (* #x2000 (- (logical-shift-right (page-ref pg) 2) 1)))))
    (debug "   (size left from page ~s => ~#x)\n" pg N)
    (min N #x2000)))

(define (blacken-page-grays/interior (ac <active-cycle>) (pg <interior-page>))
  ;; we were supposed to process the first-page before getting here!
  (assert (>= (large-obj-swiz-mode (first-page pg)) 0))
  (let* (((ps <gc-state>) (persistent-state ac))
         ((fp <first-page>) (first-page pg))
         (q (pstore-meta-scan-interior-pp (target-rich-model ps)
                                          (target-lss ac)
                                          (page-num pg)
                                          (large-obj-swiz-mode fp)
                                          (size-on-page pg))))
    (debug " interior scan ::= ~s\n" q)
    (lookup-page-refs ac (car q))
    (grayify-pointers ac (cdr q))
    ;; return a metric of the work we did
    (length (cdr q))))

(define (lookup-page-refs ac page-refs)
  (for-each
   (lambda (pr)
     (let ((pg (if (eq? (bitwise-and (vector-ref pr 1) #b10) #b00)
                   (vector-set! pr 2 (lookup-page ac
                                                  (vector-ref pr 0)
                                                  (vector-ref pr 1)))
                   #f)))
       (debug "PAGE: <~#x ~x> => ~s\n" 
              (vector-ref pr 0) 
              (vector-ref pr 1) 
              pg)))
   page-refs))
   
(define (blacken-page-grays/normal (ac <active-cycle>) (pg <page>) gray-list)
  (let* (((ps <gc-state>) (persistent-state ac))
         (refs (pstore-meta-scan-objects (target-rich-model ps)
                                         (target-lss ac)
                                         (page-num pg)
                                         (swizzle-mode-table ps)
                                         gray-list)))
    (debug "  scanned:  ~d page-refs, ~s ptrs\n"
           (length (car refs))
           (map length (cdr refs)))
    ;; lookup the actual page objects
    (lookup-page-refs ac (car refs))
    ;; now, mark all of those as gray
    (for-each
     (lambda (obj-offset ptr-list)
       (debug "OBJ <~x:~x> has ~d pointers\n" 
              (page-num pg) 
              obj-offset
              (length ptr-list))
       (grayify-pointers ac ptr-list))
     gray-list
     (cdr refs))
    ;;
    (length (cdr refs))))

;;;
;;;  Keep working until we've run out of fuel
;;;

;;;  Pop a small pool of pages from the head of the
;;;  gray-pages list

(define (pop-pool (ps <gc-state>) n)
  (if (or (eq? n 0)
          (null? (gray-pages ps)))
      '()
      (let ((next (car (gray-pages ps))))
        (set-gray-pages! ps (cdr (gray-pages ps)))
        (set-in-gray-list?! next #f)
        (cons next (pop-pool ps (- n 1))))))

        
(define (do-scanning-work (ac <active-cycle>) fuel)
  (let ((ps (persistent-state ac)))
    (let loop ((fuel fuel))
      (if (> fuel 0)
          (if (null? (gray-pages ps))
              fuel                      ; fuel left over
              (loop (do-scan-of-one-pool ac fuel)))
          fuel))))

(define (do-scan-of-one-pool (ac <active-cycle>) fuel)
  ;; work on a small pool of pages at a time
  (let* ((ps (persistent-state ac))
         (pool (pop-pool ps 3)))
    ;;
    (debug "pool: scan of ~s starting with ~d fuel\n"
            (map page-num pool)
            fuel)
    ;;
    (let pool-loop ((fuel fuel))
      (if (< fuel 0)
          ;; ran out of fuel... put the pool back on the head
          (begin
            (set-gray-pages! ps (append pool (gray-pages ps)))
            fuel)
          (let ((effort (reduce (lambda (sum pg)
                                  (+ sum (blacken-page-grays ac pg)))
                                0
                                pool)))
            (debug "pool: scan at cost of ~d effort\n" effort)
            (if (eq? effort 0)
                (begin
                  (debug "pool: scan done with ~d fuel left\n" fuel)
                  fuel)                    ; fuel left over
                (pool-loop (- fuel effort))))))))

;;;
;;;  Run a full GC
;;;

(define (full-gc (ac <active-cycle>))
  (let loop ()
    (gc-status (persistent-state ac))
    (if (<= (do-scanning-work ac 1000) 0)
        (begin
          (commit (backing-store ac))
          (loop)))))

;;;
;;;   Print a status report
;;;

(define (gc-status (ps <gc-state>))
  (format #t "GC progress:  ~d pages with gray objects\n"
          (length (gray-pages ps)))
  (for-each
   (lambda (k)
     (let* ((pg (table-lookup (page-index ps) k))
            ((m <object-map>) (object-map pg))
            ((n <fixnum>) (gvec-length m)))
       (let loop (((i <fixnum>) 0)
                  ((white <fixnum>) 0)
                  ((black <fixnum>) 0)
                  ((gray <fixnum>) 0))
         (if (fixnum<? i n)
             (let (((e <fixnum>) (gvec-ref m i)))
               (case (bitwise-and e #b11)
                 ((#b00) (loop (add1 i) (add1 white) black gray))
                 ((#b10) (loop (add1 i) white (add1 black) gray))
                 ((#b01) (loop (add1 i) white black (add1 gray)))
                 ((#b11) (error "strange flag"))))
             (format #t "  <~08x:> ~4d ( white: ~4d  gray: ~4d  black: ~4d )\n"
                     k
                     n
                     white
                     gray
                     black)))))
   (sort (key-sequence (page-index ps)) <)))

;;;

(define *a* #f)

(define (t)  (set! *a* (create-gc-cycle "g0.cycle" "test-target.sto")))
(define (t2)  (set! *a* (create-gc-cycle "g0.cycle" "test2.sto")))
(define (tbig)  (set! *a* (create-gc-cycle "g0.cycle" "/tmp/big1.sto")))

;------------------------------------------------------------------

;(t)

;(define (t3 . x)
;  (pstore-meta-scan-objects *m* *l* #x1000000 *swiz-mode-table* x))

;(define *m* (target-rich-model (persistent-state *a*)))
;(define *l* (target-lss *a*))
;(define *swiz-mode-table* (pstore-meta-std-indirects))
