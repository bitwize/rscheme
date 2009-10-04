;;;

(define (pstore-volume-set (ps <persistent-store>))
  (let ((lss (underlying-lss ps)))
    (let loop ((f '())
               (i 0))
      (let ((n (lss-file lss i)))
        (if n
            (loop (cons n f) (+ i 1))
            (list->vector (reverse f)))))))


;;;

(define-class <online-compacter> (<object>)
  (owner type: <persistent-store>)
  (active type: <fixnum> init-value: 0)
  (gc-temp-file init-value: #f)         ; can also be a <string> to persist gc
  (volumes type: <vector>)
  (progress-contn init-value: #f)
  (use-bookmark type: <fixnum> init-value: 1)
  (active-gc-cycle init-value: #f)
  (progress-state init-value: #f)
  (status-messages)
  (tuning-parameters init-value: '#(1000 1300 3000 100000 100 3000)))

(define-class <object-map> (<object>) :gvec)

(define-class <page> (<object>)
  ;;  this should really be a <integer>, since a page num could be > 500M
  (page-num type: <fixnum>)             ; base_page_num
  ;;
  ;;  `page-ref' encodes the other part of a PageRef
  ;;
  ;;           17               2     1         0
  ;;          +------------------+----------+-------+
  ;;          |     nth_page     | indirect | first |
  ;;          +------------------+----------+-------+
  ;;
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
  (format port "#[<page>  ~#x]" (page-num self)))

(define-class <first-page> (<page>)
  (large-obj-length type: <fixnum>)
  (large-obj-swiz-mode type: <fixnum>))

(define-class <interior-page> (<page>)
  (first-page type: <first-page>))

(define-method write-object ((self <first-page>) port)
  (format port "#[<fpage> ~#x 0/~d]" 
          (page-num self)
          (logical-shift-right (page-ref self) 2)))

(define-method write-object ((self <interior-page>) port)
  (format port "#[<ipage> ~#x ~d/~d]" 
          (page-num self)
          (logical-shift-right (page-ref self) 2)
          (logical-shift-right (page-ref (first-page self)) 2)))

;;;
;;;  Page classes are used as follows:
;;;
;;;       <page>                A page that contains small objects
;;;       <first-page>          The first page of a large object
;;;       <interior-page>       A subsequent page of a large object

;;;
;;;  This is the root object of the temporary storage
;;;

(define-class <gc-state> (<object>)
  (state init-value: 'scan)
  ;;
  ;; the target pstore which we are scanning
  ;;
  (target-pstore type: <vector>)        ; vector of volume names
  (target-generation type: <fixnum>)    ; generation
  ;;
  ;; this contains entries for all the persistent pages;
  ;; a given page number maps to either the symbol 'unknown
  ;; or a <page> object
  ;;
  (page-index type: <hash-integer-table> init-function: make-fixnum-table)
  ;;
  ;;  a queue of pages that have gray objects
  ;;
  gray-queue
  ;;
  ;;  the rich-model used by the target repository
  ;;
  (target-rich-model init-value: #f)
  ;;
  ;;  a map of swizzle modes for the application
  ;;
  (swizzle-mode-table init-value: #f)
  ;;
  ;;  the agenda for actually deleting objects
  ;;
  (deletion-agenda init-value: #f))

;;; to help distinguish between the target pstore being GC'd
;;; and the pstore that is holding our GC state, we will
;;; call the pstore holding our state the "sstore" for State STORE.

(define (bind-sstore-to-classes sto)
  (register-indirect-page sto 1 (vector <gc-state>
                                        <object-map>
                                        <page>
                                        <first-page>
                                        <interior-page>)))

;;;

(define-class <active-gc-cycle> (<object>)
  temp-store ;; <persistent-store> or #f
  (persistent-state type: <gc-state>)
  (target-lss type: <lss>))

(define-method finalize ((self <active-gc-cycle>))
  ;(format #t "finalized ~s\n" self)
  (gvec-set! self 1 #f)
  (if (temp-store self)
      (close-persistent-store (temp-store self)))
  (lss-close (target-lss self)))

(define (mark-finalizable item)
  (register-for-finalization item)
  item)

(define (initiate-gc-cycle (target <persistent-store>)
                           temp-file            ; #f or <string>
                           generation           ; #f or <fixnum>
                           (comp <online-compacter>)
                           get-fuel)
  (let* ((vols (pstore-volume-set target))
         (ssto (and temp-file (create-persistent-store temp-file)))
         (lss (lss-open vols generation #t))
         ;; the info that comes back is:  (model root-page . root-offset)
         (boot (pstore-meta-root-info lss)))
    (if ssto
        (bind-sstore-to-classes ssto))
    (let* ((r (make <gc-state>
                    target-rich-model: (car boot)
                    target-pstore: vols
                    target-generation: generation
                    swizzle-mode-table: (pstore-meta-std-indirects)
                    gray-queue: (make-dequeue)))
           (r (if ssto
                  (copy-into-pstore (default-allocation-area ssto) r)
                  r)))
      ;;
      ;;  Copy the pivot information from the target store
      ;;
      (table-for-each
       (indirect-pages target)
       (lambda (h k v)
         (dm 101 "indirect page[0x~04x] ==> ~d objects" k (vector-length v))
         (pstore-meta-insert-indirects (swizzle-mode-table r) k v)))
      ;;
      ;; XXX should we mark this as finalizable, in case our caller
      ;; drops the pointer somewhere along the way?
      (let ((cyc (mark-finalizable
                  (make <active-gc-cycle>
                        temp-store: ssto
                        persistent-state: r
                        target-lss: lss))))
        ;;  Make sure we know about all allocated objects,
        ;;  so we know what can be deleted when we're done
        ;;  (since objects that are now free may become allocated
        ;;  before we're done)
        (init-page-index cyc comp get-fuel)
        ;;
        ;;  Add the root object to the gray set
        (mark-gray cyc (lookup-page cyc (cadr boot) #b101) (cddr boot))
        ;;
        ;;
        ;; The <gc-state> is in a completely consistent state, ready to
        ;; start the traversal of the whole sh!bang.
        ;;
        (if ssto
            (begin
              (commit ssto r)
              (assert (eq? (persistent-state cyc) (root-object ssto)))))
        cyc))))


;;;

(define-method write-object ((self <partial-continuation>) port)
  (format port "#[<partial-continuation> @~a ~a]"
          (machine-bits->string self)
          (name (gvec-ref self 1))))
          
(define (mark-gray (ac <active-gc-cycle>) (pg <page>) offset)
  (let* (((m <object-map>) (object-map pg))
         ((n <fixnum>) (gvec-length m)))
    (let loop (((i <fixnum>) 0))
      (if (fixnum<? i n)
          (let (((e <fixnum>) (gvec-ref m i)))
            (if (eq? (logical-shift-right e 2) offset)
                (begin
                  (dm 114 "mark-gray <~x:~x> -- flags ~02b"
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
                              (dm 115 "object pages: ~s" ops)
                              (for-each
                               (lambda (p)
                                 (dequeue-push-back! (gray-queue ps) p)
                                 (set-in-gray-list?! p #t))
                               ops)))
                        (values)))
                  (values))
                (loop (add1 i))))
          (error "object <~x:~x> -- could not find on page!" 
                 (page-num pg)
                 offset)))))

;;;

;;; return a list of pages to be marked gray if an object
;;; on the given page is marked gray.  This is normally only
;;; the page itself; however, for a large object, the object-pages
;;; is all of its pages

(define (object-pages ps (pg <page>))
  (if (eq? (page-ref pg) #b101)
      (list pg)
      (map (lambda (i)
             (let ((p (table-lookup (page-index ps) (+ i (page-num pg)))))
               ;; set the first bit in the page's object-map... why?
               (gvec-set! (object-map p)
                          0 
                          (bitwise-or (gvec-ref (object-map p) 0) #b1))
               p))
           (range (logical-shift-right (page-ref pg) 2)))))


(define (lookup-page (ac <active-gc-cycle>) (page-num <fixnum>) (page-ref <fixnum>))
  (let* (((ps <gc-state>) (persistent-state ac))
         (pg (table-lookup (page-index ps) page-num)))
    (if (not pg)
        (error "no entry for page <~x:> (#b~03b)" page-num page-ref))
    pg))

;;;
;;;  Add newly discovered pages for processing in this GC cycle
;;;

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

(define (new-page/normal (ac <active-gc-cycle>) (ps <gc-state>) page-num)
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


(define (new-page/first (ac <active-gc-cycle>) 
                        (ps <gc-state>) page-num n-pages)
  (let ((f (make-new-page ps 
                          (lambda (v)
                            (make <first-page>
                                  page-num: page-num
                                  page-ref: (bitwise-or #b01
                                                        (logical-shift-left 
                                                         n-pages
                                                         2))
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
     (cdr (range n-pages)))
    f))

;;;
;;;======================================================================
;;;
;;;   Facilities for making some progress (i.e., taking some of those
;;;   gray objects and turning them black)
;;;
;;;======================================================================

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

(define (blacken-page-grays (ac <active-gc-cycle>) (pg <page>))
  (dm 201 "page grays: ~s" pg)
  (let ((e (blacken-page-grays* ac pg)))
    (dm 209 "spent effort ~s to blacken page ~s grays" e pg)
    e))
           
(define (blacken-page-grays* (ac <active-gc-cycle>) (pg <page>))
  (let (((R <fixnum>) (page-ref pg))
        (gray-objs (get-gray-list-and-mark-black pg)))
    (dm 202 "blackening <~x:*> : ~d objects ~s" 
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

;;;
;;;  ptr-list ::= (ptr ...)
;;;
;;;  ptr ::= (#(...) ...)

(define (grayify-pointers (ac <active-gc-cycle>) ptr-list)
  (for-each
   (lambda (ptr)
     (dm 211 "grayify ptr: ~s" ptr)
     (let ((pref (car ptr))
           (offset (cdr ptr)))
       (if (vector-ref pref 2)
           (mark-gray ac (vector-ref pref 2) offset))))
   ptr-list))

(define (size-on-page (pg <interior-page>))
  (let* ((nth (logical-shift-right (page-ref pg) 2))
         (N (- (large-obj-length (first-page pg)) 
               (- #x2000 #x28)          ; subtract what's on first page
               (* #x2000 (- nth 1)))))  ; subtract what's on previous pages
    (dm 212 "size left from page ~s => ~#x" pg N)
    (min N #x2000)))

(define (blacken-page-grays/interior (ac <active-gc-cycle>) 
                                     (pg <interior-page>))
  ;; we were supposed to process the first-page before getting here!
  (assert (>= (large-obj-swiz-mode (first-page pg)) 0))
  (let* (((ps <gc-state>) (persistent-state ac))
         ((fp <first-page>) (first-page pg))
         (q (pstore-meta-scan-interior-pp (target-rich-model ps)
                                          (target-lss ac)
                                          (page-num pg)
                                          (large-obj-swiz-mode fp)
                                          (size-on-page pg))))
    (dm 221 "interior scan (mode ~s) ::= ~s" (large-obj-swiz-mode fp) q)
    (lookup-page-refs ac (car q))
    (grayify-pointers ac (cdr q))
    ;; return a metric of the work we did
    (page-billing (list (cdr q)))))

(define (lookup-page-refs ac page-refs)
  (for-each
   (lambda ((pr <vector>))
     (let ((pg (if (eq? (bitwise-and (vector-ref pr 1) #b10) #b00)
                   (vector-set! pr 2 (lookup-page ac
                                                  (vector-ref pr 0)
                                                  (vector-ref pr 1)))
                   #f)))
       (dm 231 "lookup-page-refs: <~#x ~x> => ~s" 
           (vector-ref pr 0) 
           (vector-ref pr 1) 
           pg)))
   page-refs))
   
(define (blacken-page-grays/normal (ac <active-gc-cycle>) (pg <page>) gray-list)
  (dm 241 "blacken-page-grays/normal ~s" pg)
  (let* (((ps <gc-state>) (persistent-state ac))
         (refs (pstore-meta-scan-objects (target-rich-model ps)
                                         (target-lss ac)
                                         (page-num pg)
                                         (swizzle-mode-table ps)
                                         gray-list)))
    (dm 242 "scanned:  ~d page-refs, ~s ptrs"
        (length (car refs))
        (map length (cdr refs)))
    ;; lookup the actual page objects
    (lookup-page-refs ac (car refs))
    ;; now, mark all of those as gray
    (for-each
     (lambda (obj-offset ptr-list)
       (dm 243 "OBJ <~x:~x> has ~d pointers" 
              (page-num pg) 
              obj-offset
              (length ptr-list))
       (grayify-pointers ac ptr-list))
     gray-list
     (cdr refs))
    ;;
    (page-billing (cdr refs))))

(define (page-billing refs)
  (let loop ((sum 50)   ; bill 50 for each page
             (r refs))
    (if (null? r)
        (begin
          (dm 299 "billing for ~s is: ~s" refs sum)
          sum)
        (loop (+ sum 
                 4                      ; bill 3 for each object
                 (length (car r)))      ; bill 1 for each pointer
              (cdr r)))))

(define (blacken-page-grays/first (ac <active-gc-cycle>) (pg <first-page>))
  (dm 251 "blacken-page-grays/first ~s" pg)
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
    (page-billing (list (cddr q)))))


;;;======================================================================
;;;
;;;   Facilities for managing the overall cycle progress
;;;   (i.e., getting some work from the list of pages-with-grays)
;;;
;;;======================================================================

;;;  Gray the next page from the queue of gray pages (a gray page is
;;;  a page with gray objects on it)

(define (pop-next (ps <gc-state>))
  (let ((q (gray-queue ps)))
    (if (dequeue-empty? q)
        #f
        (let (((next <page>) (dequeue-pop-front! q)))
          (set-in-gray-list?! next #f)
          next))))

(define (scan-next-page (ac <active-gc-cycle>) fuel)
  ;; work on one page at a time...
  (let* ((ps (persistent-state ac))
         (p (pop-next ps)))
    ;;
    (assert p)
    ;;
    (dm 301 "pool: scan of <~x:> with ~d s-fuel" (page-num p) fuel)
    ;;
    (let ((effort (blacken-page-grays ac p)))
      (dm 303 "pool: scan of <~x:> at cost of ~d s-fuel" (page-num p) effort)
      (- fuel effort))))

;;;======================================================================
;;;
;;;   Facility for finding out how things are going/have gone
;;;
;;;======================================================================

(define (report-page-stats (ac <active-gc-cycle>))
  (let (((ps <gc-state>) (persistent-state ac))
        (total-num-black 0)
        (total-bytes-black 0))
    (for-each
     (lambda (k)
       (let* ((pg (table-lookup (page-index ps) k))
              ((m <object-map>) (object-map pg))
              ((n <fixnum>) (gvec-length m))
              ((num-black <fixnum>) 0)
              ((bytes-black <fixnum>) 0)
              ((num-white <fixnum>) 0)
              ((bytes-white <fixnum>) 0)
              (offsets (vector-append
                        (vector-map (lambda (ent)
                                      (logical-shift-right ent 2))
                                    (clone2 m <vector>))
                        '#(8192))))
         ;; XXX note: we seem to assign too much to the 
         ;; last object of the page (i.e., for the page which is
         ;; currently being allocated from)
         (let loop (((i <fixnum>) 0))
           (if (fixnum<? i n)
               (let (((e <fixnum>) (gvec-ref m i))
                     ((size <fixnum>) (- (vector-ref offsets (+ i 1))
                                         (vector-ref offsets i))))
                 (if (eq? (bitwise-and e #b11) #b00)
                     (begin
                       (set! num-white (add1 num-white))
                       (set! bytes-white (fixnum+ bytes-white size)))
                     (begin
                       (set! num-black (add1 num-black))
                       (set! bytes-black (fixnum+ bytes-black size))))
                 (loop (add1 i)))
               (begin
                 (format #t "s ~08x (~-3d / ~-4d B blk), (~-3d / ~-4d B wht) ~-3d% full ; ~s\n"
                         (page-num pg)
                         num-black
                         bytes-black
                         num-white
                         bytes-white
                         (round (/ bytes-black 81.92))
                         pg)
                 (set! total-num-black
                       (+ total-num-black num-black))
                 (set! total-bytes-black
                       (+ total-bytes-black bytes-black)))))))
     (sort (key-sequence (page-index ps)) <))
    (format #t "t (~-3d / ~-4d B blk) ~-3d% black/disk\n" 
            total-num-black
            total-bytes-black
            (round (* 100.0
                      (/ total-bytes-black (total-bytes-on-disk ps)))))))

(define (total-bytes-on-disk (self <gc-state>))
  (reduce + 0.0 
          (map (lambda (f)
                 (stat-size (stat f)))
               (vector->list (target-pstore self)))))


;;;======================================================================
;;;
;;;   Compute the agenda for freeing up storage
;;;
;;;======================================================================

(define (compute-deletion-agenda (self <active-gc-cycle>))
  (set-deletion-agenda! (persistent-state self) 
                        (build-freeable-page-list self))
  (values))

(define (build-freeable-page-list (self <active-gc-cycle>))
  (let ((r '()))
    (table-for-each
     (page-index (persistent-state self))
     (lambda (h k v)
       (let ((fl (white-objects-on-page v)))
         (if (pair? fl)
             (set! r (cons (list->vector (cons* k (page-ref v) fl)) r))))))
    (sort r (lambda (a b)
              (< (vector-ref a 0)
                 (vector-ref b 0))))))

(define-method white-objects-on-page ((p <interior-page>))
  '())

(define-method white-objects-on-page ((p <first-page>))
  (let (((entry <fixnum>) (gvec-ref (object-map p) 0)))
    (if (eq? (bitwise-and entry #b11) #b00)
        (list entry)
        '())))


(define-method white-objects-on-page ((p <page>))
  (let* (((o <object-map>) (object-map p)))
    (let loop (((i <fixnum>) (sub1 (gvec-length o)))
               (w '()))
      (if (fixnum<? i 0)
          w
          (let (((entry <fixnum>) (gvec-ref o i)))
            (loop (sub1 i) 
                  (if (eq? (bitwise-and entry #b11) #b00)
                      (cons entry w)
                      w)))))))

;;;======================================================================
;;;
;;;   Apply the results of the scan to actually free up the
;;;   unreferenced storage
;;;
;;;======================================================================

(define (execute-deletion-agenda (self <active-gc-cycle>) target)
  (let ((fpages (deletion-agenda (persistent-state self)))
        ((num-kills <fixnum>) 0))
    (dm 801 "~d pages with dead objects" (length fpages))
    (for-each
     (lambda ((agenda-item <vector>))
       (let* ((page-num (vector-ref agenda-item 0))
              (flags (vector-ref agenda-item 1)))
         (let loop ((i 2))
           (if (< i (vector-length agenda-item))
               (let* (((e <fixnum>) (vector-ref agenda-item i))
                      (offset (logical-shift-right e 2)))
                 ;(dm 802 "deleting <~08x:~04x> (~03b)" page-num offset flags)
                 (location-deallocate target page-num flags offset)
                 (set! num-kills (add1 num-kills))
                 (loop (+ i 1)))
               (dm 803 "deleted ~d objects from page <~08x>"
                   (- (vector-length agenda-item) 1)
                   page-num)))))
     fpages)
    num-kills))

(define (free-white-objects (self <active-gc-cycle>) target)
  (let* ((px (page-index (persistent-state self)))
         (fpages (sort (key-sequence px) <)))
    (dm 801 "~d pages with dead objects" (length fpages))
    (for-each
     (lambda (page-num)
       (let* ((pg (table-lookup px page-num))
              ((m <object-map>) (object-map pg))
              ((n <fixnum>) (gvec-length m)))
         (let loop (((i <fixnum>) 0)
                    (num-deleted 0))
           (if (fixnum<? i n)
               (let (((e <fixnum>) (gvec-ref m i)))
                 (if (eq? (bitwise-and e #b11) #b00)
                     (let ((offset (logical-shift-right e 2))
                           (flags #b101))
                       (dm 802 "deleting <~08x:~04x> (~03b)" page-num offset flags)
                       (location-deallocate target page-num flags offset)
                       (loop (add1 i) (add1 num-deleted)))
                     (loop (add1 i) num-deleted)))
               (dm 803 "deleted ~d objects from page <~08x>" 
                   num-deleted 
                   page-num)))))
     fpages)))

;;;

#|
(define (followup-scan (self <active-gc-cycle>))
  (let (((gc <gc-state>) (persistent-state self))
        (r '())
        ((v <vector>) (sort
                       (lss-record-query (target-lss self) 
                                         '(0 . 0) 
                                         '(9 . #xfffffff))
                       <)))
    (let loop (((i <fixnum>) 0)
               ((next <fixnum>) 1))
      (if (< i (vector-length v))
          (let (((r <fixnum>) (vector-ref v i)))
            (if (fixnum<? r next)
                (loop (add1 i) next)
                (let ((px (table-lookup (page-index gc) r)))
                  (if px
                      (loop (add1 i) (add1 r))
                      (let ((n (pstore-meta-scan-npages (target-rich-model gc)
                                                        (target-lss self)
                                                        r
                                                        (swizzle-mode-table gc))))
                        (loop (add1 i) (fixnum+ next n)))))))
          (values)))))
|#

(define (init-page-index (self <active-gc-cycle>) (comp <online-compacter>) get-fuel)
  (let (((gc <gc-state>) (persistent-state self))
        (r '())
        ((v <vector>) (sort
                       (lss-record-query (target-lss self) 
                                         '(0 . 0) 
                                         '(9 . #xfffffff))
                       <)))
    ;;
    (define page-count 
      (let ((model (target-rich-model gc))
            (lss (target-lss self))
            (swiz (swizzle-mode-table gc)))
        (lambda (r)
          (pstore-meta-scan-npages model lss r swiz))))
    ;;
    (let ((t (init-page-index-tuning comp)))
      (let loop (((i <fixnum>) (if (eq? (vector-ref v 0) 0)
                                   1
                                   0))
                 ((next <fixnum>) #x1000000))     ; skip indirect page data
        (if (< i (vector-length v))
            (let (((r <fixnum>) (vector-ref v i)))
              (get-fuel t (format #f "init-page-index ~d of ~d" i (vector-length v)))
              (if (fixnum<? r next)
                  (loop (add1 i) next)
                  (let (((n <fixnum>) (page-count r)))
                    (dm 105 "page <~x:> count ~d" r n)
                    (if (eq? n 1)
                        (new-page/normal self gc r)
                        (new-page/first self gc r n))
                    (loop (add1 i) (fixnum+ r n))))))))))

;;;======================================================================
;;;
;;;   External (application) interface
;;;

;;;
;;;  Finish GC work
;;;
;;;  NOTE: computing the deletion agenda should be
;;;        moved into the "progress" phase
;;;
(define (pstore-gc-finalize (self <active-gc-cycle>) 
                            (target <persistent-store>))
  (if (not (deletion-agenda (persistent-state self)))
      (begin
        (compute-deletion-agenda self)
        (set-state! (persistent-state self) 'finalize)
        (commit self)))
  (let ((num-del (execute-deletion-agenda self target)))
    (set-state! (persistent-state self) 'complete)
    (commit self)
    num-del))

(define-method commit ((self <active-gc-cycle>))
  (if (temp-store self)
      (commit (temp-store self)))
  (values))


(define-constant $young-bookmark 1)

;;;
;;;  Online LSS compaction, and the hook for doing online GC
;;;


;;;  tune-scan-chunk: how much scanning work to do (measured in XX)
;;;                   each time tune-scan-fuel units of fuel are
;;;                   consumed
;;;
;;;  tune-idle-pause: how much fuel it takes to get the GC cycle off
;;;                   the ground (i.e., between cycles)
;;;
;;;  tune-idle-gc-wait: how much fuel to transfer to the transient GC
;;;                     while waiting for the transient GC to complete


(define (persistent-traversal-chunk-tuning (self <online-compacter>))
  (vector-ref (tuning-parameters self) 0))

(define (persistent-traversal-work-tuning (self <online-compacter>))
  (vector-ref (tuning-parameters self) 1))

(define (reclamation-tuning (self <online-compacter>))
  (vector-ref (tuning-parameters self) 2))

(define (idle-pause-tuning (self <online-compacter>))
  (vector-ref (tuning-parameters self) 3))

(define (init-page-index-tuning (self <online-compacter>))
  (vector-ref (tuning-parameters self) 4))

(define (eager-clean-page-tuning (self <online-compacter>))
  (vector-ref (tuning-parameters self) 5))

(define (set-tuning-parameter! (self <online-compacter>) key value)
  (let ((k (case key
             ((persistent-traversal-chunk) 0)
             ((persistent-traversal-work) 1)
             ((reclamation) 2)
             ((idle-pause) 3)
             ((init-page-index) 4)
             (else (error "Unknown tuning parameter: ~s" key)))))
    (let ((v (clone (tuning-parameters self))))
      (vector-set! v k value)
      (set-tuning-parameters! self v)
      (values))))
             
;;;
 
(define-syntax (delta-time t1 t0)
  (with-module syscalls
    (time-time t1 t0)))

(define-syntax (current-time)
  (with-module syscalls
    (time)))

(define (sm (self <online-compacter>) fmt . args)
  (let ((q (status-messages self)))
    (if (> (dequeue-count q) 25)
        (dequeue-pop-front! q))
    (dequeue-push-back! q (vector (current-time) fmt args))
    ;(format #t "---> ~a\n" (apply format #f fmt args))
    (values)))

(define (stop-online-compacter (self <persistent-store>))
  (set-compaction-hook! self #f))

(define (start-online-compacter (self <persistent-store>)
                                #key
                                (tune-scan-chunk default: #f)
                                (tune-scan-fuel default: #f)
                                (tune-idle-pause default: #f)
                                (tune-idle-gc-wait default: #f)
                                )
  (dm "initializing online compacter")
  (assert (not (compaction-hook self)))
  ;; switch the undlerying system back to IDLE phase, in case it wasn't
  (gc:work self 100 #t)
  (flush-live-pages self)
  ;;
  (let* ((l (underlying-lss self))
         (tip (lss-get-tip l))
         (tipsize (lss-get-vol-size l tip))
         (vols (pstore-volume-set self))
         ;(tmpf (string-append (vector-ref vols 0) ".gc.sto"))
         (oc (make <online-compacter>
                   owner: self
                   active: (lss-get-tip l)
                   status-messages: (make-dequeue)
                   ;gc-temp-file: tmpf
                   volumes: vols)))
    ;;
    (if tune-scan-chunk
        (set-tuning-parameter! oc 'persistent-traversal-chunk tune-scan-chunk))
    (if tune-scan-fuel
        (set-tuning-parameter! oc 'persistent-traversal-work tune-scan-fuel))
    (if tune-idle-pause
        (set-tuning-parameter! oc 'idle-pause tune-idle-pause))
    ;;
    (if (= (vector-length vols) 1)
        (error "start-online-compactor: only works for multi-volume pstores"))
    ;;
    (if (and (gc-temp-file oc)
             (stat (gc-temp-file oc)))
        (unlink (gc-temp-file oc)))
    ;;
    (set-compaction-hook! self (gc-compaction-hook-proc self oc))
    ;;
    (set-progress-contn! oc (call-with-current-continuation/one-shot
                             (lambda (cc)
                               (compaction-cycle oc cc
                                                 tune-scan-chunk
                                                 tune-scan-fuel
                                                 tune-idle-pause
                                                 tune-idle-gc-wait))))
    (values)))

(define (gc-compaction-hook-proc (self <persistent-store>)
                                 (of <online-compacter>))
  (lambda (commit-id start-tip-size)
    (if (eq? commit-id '*QUERY*)
        of
        (let ((t0 (time)))
          (dm 2001 "GC hook from state: ~s" (gc:work self 0 #f))
          ;;
          (fuel-online-compaction! self
                                   (- (get-tip-size self) start-tip-size)
                                   of)
          (let ((dt (time-time (time) t0)))
            (dm 2007 "GC hook end in state: ~s" (gc:work self 0 #f))
            (dm 2008 "online GC work pause: ~a" dt))))))

(define (fuel-online-compaction! (self <persistent-store>) fuel oc)
  ;; XXX optimization? put fuel tank on this side of the 
  ;; continuation switch, to avoid a common-case double cc save
  (if oc
      (set-progress-contn!
       oc
       (call-with-current-continuation/one-shot
        (lambda (cc)
          ((progress-contn oc) cc fuel))))))
;;;

(define (print-compaction-status (self <persistent-store>))
  (if (compaction-hook self)
      (let* (((c <online-compacter>) ((compaction-hook self) '*QUERY* 0))
             (x (progress-state c)))
        (if (pair? x)
            (case (car x)
              ((block)
               ;;  progress-state:  (block STARTTIME)
               (format #t "status: blocked waiting for next commit\n\tsince ~s\n"
                       (cadr x)))
              ((wait)
               ;;  progress-state:  (wait STATE FUEL STARTTIME)
               (format #t "status: (~s state) waiting for ~d fuel\n\tsince ~s\n"
                       (cadr x)
                       (caddr x)
                       (cadddr x)))
              ((run)
               ;;  progress-state:  (run RUNTIME wait STATE FUEL STARTTIME)
               ;;              or:  (run RUNTIME block STARTTIME)

               (case (caddr x)
                 ((wait)
                  (format #t "status: (~s state) running since ~s\n\tafter waiting ~a for ~d fuel\n"
                          (list-ref x 3)
                          (cadr x)
                          (with-module syscalls
                            (time-time (cadr x)
                                       (list-ref x 5)))
                          (list-ref x 4)))
                 ((block)
                  (format #t "status: running since ~s\n\tafter being blocked for ~a\n"
                          (cadr x)
                          (with-module syscalls
                            (time-time (cadr x) (cadddr x)))))
                 (else
                  (format #t "status: running since ~s\n" (cadr x)))))
              (else
               (format #t "status: ~s\n" x)))
            (format #t "status: ~s\n" x))
        (print-message-queue (status-messages c)))
      (format #t "status: No compaction\n")))

(define (print-message-queue q)
  (bind ((v (dequeue-state q))
         (ds maxw (delta-strs v)))
    ;;
    (vector-for-each
     (lambda ((e <vector>)
              (ts <string>))
       (format #t "~a~a~a: "
               (time->string (vector-ref e 0) "%Y-%m-%d %H:%M:%S")
               (make-string (- maxw (string-length ts)) #\space)
               ts)
       (apply format #t (vector-ref e 1) (vector-ref e 2))
       (newline))
     v
     ds)))

(define (delta-strs v)
  ;;
  (define (str1 t1 t0)
    (with-module
        syscalls
      (sprintf-float " (+%.3f)" 40 (interval->seconds (time-time t1 t0)))))
  ;;
  (let ((a (make-vector (vector-length v) "")))
    (let loop ((i 1)
               (maxw 0))
      (if (< i (vector-length v))
          (let ((s (str1 (vector-ref (vector-ref v i) 0)
                         (vector-ref (vector-ref v (sub1 i)) 0))))
            (vector-set! a i s)
            (loop (add1 i) (max maxw (string-length s))))
          (values a maxw)))))
;;;

(define (flush-live-pages (self <persistent-store>))
  (let loop ((n 0))
    (if (gc:work self 2 #t)
        (loop (+ n 1))
        (dm "flushed ~d leftover live pages" n))))

;;;

(define (persistent-traversal (self <online-compacter>) get-fuel)
  ;;
  (bind (((q <dequeue>) (live-queue (owner self)))
         (commit-id (list->values (vector->list (dequeue-pop-front! q))))
         ((ac <active-gc-cycle>) (initiate-gc-cycle (owner self) 
                                                    #f
                                                    commit-id
                                                    self
                                                    get-fuel)))
    ;;
    (define (another-page-of-live-ones)
      ;; return #t if done
      (if (dequeue-empty? q)
          #t   
          (let* ((next (dequeue-pop-front! q))
                 (live-list (gc:work (owner self) 3 next)))
            (dm "-- live list ~s" live-list)
            (dm "Next in queue: <~08x>, had ~s live" 
                next 
                (length (cdr live-list)))
            (let (((p <page>) (lookup-page ac next (car live-list))))
              (for-each
               (lambda (l)
                 (mark-gray ac p l))
               (cdr live-list))
              #f))))
    ;;
    (define (do-traversal)
      ;; return #t if done
      ;; billing in s-fuel units is done by `page-billing', and
      ;; is approximately follows:
      ;;        page    50
      ;;        object  3
      ;;        pointer 1
      (let loop ((s-fuel (persistent-traversal-work-tuning self)))
        (if (dequeue-empty? (gray-queue (persistent-state ac)))
            #t
            (if (< s-fuel 0)
                #f
                (loop (scan-next-page ac s-fuel))))))
    ;;
    (let loop ((iter 0))
      (if (eq? (pstore-gc-phase (owner self)) 'PSCAN)
          (begin
            (get-fuel (persistent-traversal-chunk-tuning self) 
                      (format #f "pscan(PSCAN:~d)" iter))
            (dm 504 "attempting traversal termination")
            (if (and (do-traversal) (another-page-of-live-ones))
                (begin
                  (dm 506 "traversal is complete")
                  (if (force *log-verbose-mode*) (report-page-stats ac))
                  ac)
                (loop (add1 iter))))
          (begin
            (get-fuel (persistent-traversal-chunk-tuning self)
                      (format #f "pscan(TSCAN:~d)" iter))
            (dm 504 "getting some traversal out of the way")
            (let ((x (do-traversal))
                  (y (another-page-of-live-ones)))
              (if (and x y)
                  (begin
                    (dm 505 "traversal awaiting more work to do")
                    (get-fuel 'FLUSH)))
              (loop (add1 iter))))))))


(define (pstore-gc-phase (self <persistent-store>))
  (vector-ref (gc:work self 0 #f) 0))

(define (eager-cleaning (self <online-compacter>) get-fuel)
  (gc:work (owner self) 101 #t)         ; switch to PREP phase
  (let loop ()
    (let ((rec (gc:work (owner self) 1 #t)))
      (if rec
          (begin
            (dm 730 "cleaned page <~x>... ~s" rec (gc:work (owner self) 6000 #f))
            (get-fuel (eager-clean-page-tuning self) "cleaning")
            (loop))
          (dm 731 "done with eager cleaning... ~s" (gc:work (owner self) 6000 #f))))))

(define (reclaim-objects-on-page (ps <persistent-store>) (p <page>) kill-list)
  ;;
  (let* ((page-num (page-num p))
         (flags (page-ref p)))
    ;;
    (for-each
     (lambda ((ent <fixnum>))
       (let ((offset (logical-shift-right ent 2)))
         (dm 802 "deleting <~x:~04x> (~03b)" page-num offset flags)
         (location-deallocate ps page-num flags offset)))
     kill-list)))

(define (kill-pointers-in-dead-objects-on-page (ps <persistent-store>) (p <page>) kill-list)
  ;;
  (let* ((page-num (page-num p))
         (flags (page-ref p)))
    ;;
    (gc:work ps 6 (vector page-num flags (map (lambda ((ent <fixnum>))
                                                (logical-shift-right ent 2))
                                              kill-list)))))

(define (do-reclamation/kill-pointers (self <online-compacter>) 
                                      (ac <active-gc-cycle>) 
                                      get-fuel)
  (table-for-each
   (page-index (persistent-state ac))
   (lambda (h (k <fixnum>) (v <page>))
     (get-fuel 50 "reclaim")   ; basic loop overhead
     (let ((fl (white-objects-on-page v)))
       (if (pair? fl)
           (let ((n (length fl)))
             (dm 801 "page <~x:> has ~d dead objects" (page-num v) n)
             ;; cost of touching a page...
             (get-fuel (reclamation-tuning self) (format #f "killptr(~d)" n))
             (kill-pointers-in-dead-objects-on-page (owner self) v fl)))))))

(define (do-reclamation/dealloc (self <online-compacter>) 
                                (ac <active-gc-cycle>) 
                                get-fuel)
  (table-for-each
   (page-index (persistent-state ac))
   (lambda (h (k <fixnum>) (v <page>))
     (get-fuel 50 "reclaim")   ; basic loop overhead
     (let ((fl (white-objects-on-page v)))
       (if (pair? fl)
           (let ((n (length fl)))
             (dm 801 "page <~x:> has ~d dead objects" (page-num v) n)
             ;; cost of touching a page...
             (get-fuel (reclamation-tuning self) (format #f "reclaim(~d)" n))
             (reclaim-objects-on-page (owner self) v fl)))))))
  
(define (do-reclamation (self <online-compacter>) 
                        (ac <active-gc-cycle>) 
                        get-fuel)
  (do-reclamation/kill-pointers self ac get-fuel)
  (do-reclamation/dealloc self ac get-fuel))


(define (compaction-cycle (self <online-compacter>) contn
                          tune-scan-chunk
                          tune-scan-fuel
                          tune-idle-pause
                          tune-idle-gc-wait)
  (let ((fuel-tank 0)
        (last-gc-id #f))
    ;;
    (define (run)
      (set-progress-state! self (cons* 'run (current-time) 
                                       (progress-state self)))
      (values))
    ;;
    (define (get-fuel f #optional state)
      (cond
       ((eq? f 'FLUSH)
        (set! fuel-tank 0))
       (else
        (set-progress-state! self (list 'wait state f (current-time)))
        (set! fuel-tank (- fuel-tank f))
        (let awaiting-fuel ()
          (dm "fuel tank: ~s (state ~a)" fuel-tank state)
          (if (>= fuel-tank 0)
              (run)
              (bind ((next fuel (call-with-current-continuation/one-shot
                                 contn)))
                (set! fuel-tank (+ fuel-tank fuel))
                (set! contn next)
                (awaiting-fuel)))))))
    ;;
    (define (idle)
      (get-fuel (idle-pause-tuning self) "idle"))
    ;;
    (define (clear)
      (if (gc-temp-file self)
          (unlink (gc-temp-file self))))
    ;;
    (define (wait-for-pending-state)
      (if (not (eq? (pstore-gc-phase (owner self)) 'PENDING))
          (begin
            (get-fuel 'FLUSH)
            (get-fuel 1 "wait-pending")
            (wait-for-pending-state))))
    ;;
    (define (wait-for-next-commit)
      (let ((was (pstore-last-commit-id (owner self))))
        (let loop ()
          (get-fuel 'FLUSH)
          (get-fuel 1 "wait-commit")
          (if (= was (pstore-last-commit-id (owner self)))
              (loop)))))
    ;;
    (let loop ()
      (dm 180 "Start of cycle...")
      (set! fuel-tank 0)                        ; empty out our fuel tank,
                                                ; forcing an idle phase
      ;;
      (dm 181 "idle...")
      (idle)
      ;;
      (dm 182 "packing...")
      (gc:work (owner self) 106 #f)             ; transition IDLE->PACKING
      (compact-lss self get-fuel)
      ;;
      (dm 183 "prep (eager cleaning)...")
      (eager-cleaning self get-fuel)
      ;;
      (dm 184 "wait for pending...")
      (wait-for-pending-state)
      ;;
      (dm 185 "persistent traversal...")
      (let ((ac (persistent-traversal self get-fuel)))
        ;; wait for a commit so that the extraHeapPointers list gets
        ;; flushed.  That way we don't accidently reclaim an object
        ;; that has a slot in the extraHeapPointers list
        (wait-for-next-commit)
        ;;
        (dm 186 "reclamation...")
        (gc:work (owner self) 105 #f)     ; switch to reclamation phase
        (do-reclamation self ac get-fuel))
      ;;
      (dm 186 "cleanup...")
      (gc:work (owner self) 100 #f)     ; switch back to idle phase
      (clear)
      (loop))))

;;;
;;;  Figure out which volume should be cleared out during
;;;  a `compact-lss' phase.  The volume to clear is the one
;;;  after the active volume, because we want to clear the
;;;  one that is (a) oldest, and (b) about to become active
;;;

(define (volume-to-clear (self <online-compacter>))
  (let ((n (vector-length (volumes self))))
    (modulo (+ (active self) 1) n)))

(define (compact-lss (self <online-compacter>) get-fuel)
  (let* ((vol (volume-to-clear self))
         (lss (underlying-lss (owner self)))
         (t0 (current-time))
         (worklist (lss-record-query lss
                                     (cons vol 0)
                                     (cons vol #x1FFFFFFF)))
         (total-bytes 0))
    ;;
    (if (not worklist)
        (error "could not search for records on vol[~d]" vol))
    ;;
    (let ((n (vector-length worklist)))
      (vector-for-each/i
       (lambda (i r)
         (let ((u (lss-move-record lss (active self) r)))
                                        ;(format #t "  (pack record #~08x -- size ~d)\n" r u)
           (set! total-bytes (+ total-bytes u))
           ;; note that the record we just copied is going to show up
           ;; as fuel, so we need to get at *least* that much fuel, and
           ;; probably more to avoid hogging the process
           (get-fuel (* u 3) (format #f "pack ~d of ~d" i n))))
       worklist)
      ;;
      (bind ((nr nb (compact-some-more lss vol (active self) 2 get-fuel)))
        (set! n (+ n nr))
        (set! total-bytes (+ total-bytes nb)))
      ;;
      (bind ((nr nb (compact-some-more lss vol (active self) 4 get-fuel)))
        (set! n (+ n nr))
        (set! total-bytes (+ total-bytes nb)))
      ;;
      (let ((gen (compact-lss-wipe self vol)))
        (sm self "pack ~d R (~d B) from v[~d] in ~a; G ~s"
            n
            total-bytes
            vol
            (delta-time (current-time) t0)
            gen)
        gen))))

(define (compact-some-more lss from-vol to-vol pass get-fuel)
  (let ((mask (logical-shift-left 1 from-vol)))
    (let loop ((nr 0)
               (nb 0))
      (let ((r (lss-find-record-on lss mask pass)))
        ;; note that there is no `get-fuel' between when the last
        ;; `lss-find-record-on' returns #f and when we call `lss-detach-vol'
        ;;
        ;; Hence, in the absence of threads, there is no opportunity for
        ;; the application to mess things up by writing into the old
        ;; volume (which it shouldn't be doing anyway, since the tip is
        ;; now on the new volume)
        ;;
        (if r
            (let ((u (lss-move-record lss to-vol r)))
              (get-fuel (* u 3) (format #f "pack-more: ~x" r))
              (loop (+ nr 1) (+ nb u)))
            (values nr nb))))))


(define (compact-lss-wipe (self <online-compacter>) vol)
  (let* ((lss (underlying-lss (owner self)))
         (gen (lss-commit lss $young-bookmark)))         ; Bookmark #1
    ;;
    (let ((f (lss-file lss vol)))
      (lss-detach-vol lss vol)
      (if (stat f)
          (unlink f))
      ;; reattach it
      (lss-attach-vol lss vol f)
      ;; and make it the new destination
      (lss-set-tip lss vol)
      (set-active! self vol)
      (lss-commit lss)          ; make sure there's a CR on it
      ;; return the generation tag for the checkpoint
      gen)))


(define-method print ((self <page>))
  (format #t "===================== PAGE <~x:> =====================\n" (page-num self))
  (format #t "===  ~s\n" self)
  (format #t "===~a\n" (if (in-gray-list? self) "  (in gray list)" ""))
  (format #t "===  Objects:\n")
  ;;
  (let* (((m <object-map>) (object-map self))
         ((n <fixnum>) (gvec-length m)))
    ;;
    (let loop (((i <fixnum>) 0))
      (if (< i n)
          (let* (((ent <fixnum>) (gvec-ref m i))
                 ((size <fixnum>) (- (if (< (+ i 1) n)
                                         (logical-shift-right (gvec-ref m (add1 i)) 2)
                                         8192)
                                     (logical-shift-right ent 2))))
            ;;
            (format #t "===     ~-3d : ~04x  ~-5d bytes : ~a\n"
                    i
                    (logical-shift-right ent 2)
                    size
                    (case (bitwise-and ent #b11)
                      ((#b00) "white")
                      ((#b01) "gray")
                      ((#b10) "black")
                      ((#b11) "?superblack")))
            (loop (add1 i)))
          (format #t "===\n")))))
