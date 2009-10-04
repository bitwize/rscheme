;;; Build a table which maps nodes to a list of nodes in the"equivalent"
;;; same "family", where A and B are in the same family if, for some
;;; version v of A, A[v].versioned-object == B

(define (build-node-family-map)
  (let ((v (make-object-table)))
    ;;
    (for-each
     (lambda (n)
       (if (not (table-key-present? v n))
           (let ((fam (find-node-family n)))
             (for-each (lambda (equiv)
                         (table-insert! v equiv fam))
                       fam))))
     (value-sequence *master-node-index*))
    ;;
    v))

;;; If the two nodes share the same exact version map, then they are
;;; "combinable" into a single node.  Otherwise, they will be split
;;; into separate nodes

(define (combinable? fam)
  (if (eq? (versions (car fam))
           (versions (cadr fam)))
      (begin
        (if (not (eq? (group (car fam))
                      (group (cadr fam))))
            (format #t "Warning: ~s group ~a <> ~s group ~a\n"
                    (car fam)
                    (name (group (car fam)))
                    (cadr fam)
                    (name (group (cadr fam)))))
        ;; n.b., stable-properties is never used
        #t)
      #f))


(define (find-node-family node)
  ;; see if all of the versions of a node point back to the node
  (let ((fam (list node))
        (q (make-dequeue)))
    (dequeue-push-back! q node)
    (let loop ()
      (if (dequeue-empty? q)
          fam
          (let ((node (dequeue-pop-front! q)))
            (for-each-version
             (versions node)
             (lambda (path vnode)
               (let ((x (versioned-object (value vnode))))
                 (if (not (eq? x node))
                     (begin
                       (format #t "node ~s [~a] => ~s => ~s\n"
                               node
                               path
                               (value vnode)
                               x)
                       (if (not (memq x fam))
                           (begin
                             (dequeue-push-back! q x)
                             (set! fam (cons x fam)))))))))
            (loop))))))

(define (node-similar? (a <node-version>) (b <node-version>))
  (if (and (eq? (modification-time a) (modification-time b))
           (eq? (permissions a) (permissions b))
           (string=? (content->string (contents a)) 
                     (content->string (contents b))))
      (if (equal? (change-items a) (change-items b))
          #t
          (let ((u (union (change-items a) (change-items b))))
            (format #t "[fixed crlist ~s U ~s = ~s]\n" 
                    (map (lambda (fsc) (id (base-request fsc)))
                         (change-items a))
                    (map (lambda (fsc) (id (base-request fsc)))
                         (change-items b))
                    (map (lambda (fsc) (id (base-request fsc))) 
                         u))
            (set-change-items! a u)
            (set-change-items! b u)
            #t))
      #f))

;;

(define (describe-branching fam remaptbl)
  ;; combinability has already been checked
  (assert (eq? (versions (car fam)) (versions (cadr fam))))
  ;;
  (let ((v (make-object-table))
        (vmap (versions (car fam))))
    ;;
    (define (tagit (x <node-version>))
      (let ((tag (string-append
                  (if (eq? (versioned-object x) (car fam))
                      "A#"
                      "B#")
                  (to-string (version-tag x))
                  "<"
                  (machine-bits->string x)
                  ">")))
        (table-insert! v x tag)
        tag))
    ;;
    (for-each-version
     (versions (car fam))
     (lambda (path vnode)
       (let* ((x (value vnode))
              (tag (tagit x)))
         ;;
         (format #t "  vmap[~a] = ~a~a~a\n" 
                 path 
                 tag
                 (if (eq? x (current-version (car fam)))
                     " = A.tip"
                     "")
                 (if (eq? x (current-version (cadr fam)))
                     " = B.tip"
                     "")))))
    ;;
    (define (scan path x)
      (let ((r (if (previous-version x)
                   (scan (string-append path "^") (previous-version x))
                   '())))
        (if (not (table-key-present? v x))
            (let ((tag (tagit x)))
              (format #t "  ~a = ~a" path tag)
              ;; find the doppleganger, d, which is the entry in the
              ;; version map with the same version id
              (let ((d (value (find-leaf vmap (leaf-name (version-tag x))))))
                (if (node-similar? x d)
                    (begin
                      (format #t "   *** similar to ~a\n"
                              (table-lookup v d))
                      (table-insert! remaptbl x d)
                      r)
                    (begin
                      (format #t " (nothing similar)\n")
                      (cons x r)))))
            r)))
    ;;
    ;; figure out which are missing from the version map
    (let ((missing (append (scan "A.tip" (current-version (car fam)))
                           (scan "B.tip" (current-version (cadr fam))))))
      (if (not (= (length missing) 0))
          (error "Surprise: missing: ~s\n" missing)))
    ;;
    (for-each
     (lambda (nv)
       (format #t "      ~32s   ~a   ~a\n"
               nv
               (modification-time nv)
               (machine-bits->string (modification-time nv))))
     (key-sequence v))
    ;;
    v))

#|
(define *remap-table* (make-object-table))

(define (remap x)
  (or (table-lookup *remap-table* x) x))


(define (build-remap-table)
  (set! *remap-table* (make-object-table))
  (let ((nfm (build-node-family-map)))
    (for-each
     describe-branching
     (select (lambda (a)
               (and (pair? (cdr a))
                    (instance? (car a) <file>)))
             (value-sequence nfm)))))
|#
     
#|
(define *nfm* (build-node-family-map))
(define *fdups* (select (lambda (a)
                          (and (pair? (cdr a))
                               (instance? (car a) <file>)))
                        (value-sequence *nfm*)))
  
(for-each describe-branching *fdups*)

|#

(define (repair-branches)
  (bind ((firstlinked master (build-node-first-linked-map)))
    (set! *master-node-index* master)
    ;;
    (let* ((nfm (build-node-family-map))
           (q '())
           (nrm (make-object-table))
           (vrm (make-object-table)))
      ;;
      (table-for-each
       nfm
       (lambda (h k a)
         (if (pair? (cdr a))
             (begin
               (set! q (cons a q))
               (if (instance? (car a) <file>)
                   (repair-branched-file a nrm vrm)
                   (repair-branched-dir a))))))
      ;;
      ;; q = 
      ;; vrm = version remap table (as by "similar to" message)
      ;; nrm = node remap table
      (let ((both (make-object-table)))
        (table-for-each vrm (lambda (h k v) (table-insert! both k v)))
        (table-for-each nrm (lambda (h k v) (table-insert! both k v)))
        (global-search-and-replace (root-object *pstore*) both))
      (values vrm q nrm))))

(define (repair-branched-dir a)
  (format #t "Don't know how to fix: ~s\n" a))

(define (repair-branched-file a node-remap vrmap)
  (format #t "----- Fixing ~s ------\n" a)
  (assert (combinable? a))
  ;;
  (table-insert! node-remap (cadr a) (car a))
  ;;
  (describe-branching a vrmap))
#|
  (bind ((tagtbl ))
    (print tagtbl)
    ;;
    (define (F (x <file-version>))
      (or (table-lookup rmap x) x))
    ;;
    (values))
|#

(define (global-search-and-replace root fixup)
  (let ((q (make-dequeue))
        (visit (make-object-table)))
    ;;
    (define (spot x)
      (if (and (gvec? x)
               (not (table-key-present? visit x))
               (allocation-area->store (object->allocation-area x)))
          (begin
            (table-insert! visit x #t)
            (dequeue-push-back! q x))))
    ;;
    (define (spotv (x <gvec> :trust-me))
      (format #t "~#@*60s\n" x)
      (let (((n <fixnum>) (gvec-length x)))
        (let loop (((i <fixnum>) 0)
                   (any? #f))
          (if (eq? i n)
              any?
              (let ((e (gvec-ref x i)))
                (cond
                 ((table-lookup fixup e)
                  => (lambda (rplc)
                       (gvec-set! x i rplc)
                       (spot rplc)
                       (loop (add1 i) #t)))
                 (else
                  (spot e)
                  (loop (add1 i) any?))))))))
    ;;
    (spot root)
    ;;
    (let loop ((i 0)
               (j 0))
      (if (dequeue-empty? q)
          i
          (begin
            (format #t "visit[~d] (~d) " i j)
            (if (spotv (dequeue-pop-front! q))
                (loop (+ i 1) (+ j 1))
                (loop i (+ j 1))))))))


(define (picknode lst k)
  (select (lambda (v)
            (eq? (id (versioned-object v)) k))
          lst))

#|
(define (PT x y)
  (persistent->transient (parts->persistent *pstore* x 5 y)))

(define vv1 (PT #x1001ec3 #x0a6b))
(define vv2 (PT #x1000b9f #x00fb))

|#

;;;
;;;  Edit (by side-effect) the order of work items in the
;;;  history of a CR, so that the <work-request> item
;;;  that closes the CR comes before anything except
;;;

(define-method printh ((self <change-request>))
  (define (ts x)
    (if x
        (time->string (timestamp x) "%Y-%m-%d %H:%M:%S")
        ""))
  ;;
  (define (lupns w)
    (cond
     ((and (close-audit-entry w)
           (table-lookup *new-state-map* w))
      => (lambda (x)
           (~ "  => ~s" x)))
     (else
      "")))
  ;;
  (for-each
   (lambda (w)
     (format #t "H ~s  ~20a ~20a ~s~a\n" 
             (transient->persistent w)
             (ts (activate-audit-entry w))
             (ts (close-audit-entry w))
             w
             (lupns w)))
   (reverse (history self)))
  (for-each
   (lambda (w)
     (format #t "A ~s  ~20a ~20a ~s~a\n" 
             (transient->persistent w)
             (ts (activate-audit-entry w))
             (ts (close-audit-entry w))
             w
             (lupns w)))
   (active-items self))
  (values))


(define (fixup-history-order (self <change-request>))
  ;;
  (let ((from-fix (find-first (lambda (p)
                                (and (instance? p <work-request>)
                                     (eq? (state p) 'fixing)))
                              (history self)))
        (fsc (find-first (lambda (p)
                           (instance? p <fs-change>))
                         (history self))))
    (if (and from-fix fsc)
        (let* ((v (list->vector (history self)))
               (i (vmemq from-fix v))
               (j (vmemq fsc v)))
          (if (< j i)
              (vector->list
               (vector-append (subvector v 0 j)
                              (vector from-fix)
                              (subvector v j i)
                              (subvector v (+ i 1))))
              #f))
        #f)))

(define (fixup-all-crs)
  (bind ((firstlinked master (build-node-first-linked-map)))
    ;;
    (set-property! *application* 'migrate:first-linked-in firstlinked)
    (set-property! *application* 'migrate:master-node-index master)
    ;;
    (for-each
     (lambda (cr)
       ;;
       ;;  (1) Fix the order of the closing <work-request> w.r.t. <fs-change>s
       ;;
       (let ((f (fixup-history-order cr)))
         (if f
             (begin
               (format #t "Fixing up: ~s\n" cr)
               (set-history! cr f))))
       ;;
       ;;  (2) For any <fs-change>'s, delete any new node versions
       ;;      that are completely unlinked
       ;;
       (for-each 
        (lambda (wi)
          (if (instance? wi <fs-change>)
              (let ((kill (select (lambda (nv)
                                    (not (table-key-present? 
                                          master
                                          (versioned-object nv))))
                                  (new-versions wi))))
                (if (pair? kill)
                    (begin
                      (format #t "CR ~s : Dead nodes ~s\n" (id cr) kill)
                      (let loop ((kill kill)
                                 (l (new-versions wi)))
                        (if (null? kill)
                            (set-new-versions! wi l)
                            (loop (cdr kill) (delq! (car kill) l)))))))))
        (append (active-items cr) (history cr))))
     ;;
     (sort
      (value-sequence (change-request-table *application*))
      (lambda (a b)
        (< (id a) (id b)))))))

(define (fixup-all-node-ids)
  (let ((tbl (make-fixnum-table)))
    (set-property! *application* 'migrate:master-node-id-index tbl)
    ;;
    (let ((vis (make-object-table))
          (k 80000))
      ;;
      (define (visit (n <node>))
        (if (not (table-lookup vis n))
            (begin
              (table-insert! vis n k)
              (set-id! n k)
              (table-insert! tbl k n)
              (set! k (+ k 1))
              ;;
              (if (instance? n <directory>)
                  (for-each-version
                   (versions n)
                   (lambda (path vleaf)
                     (let (((v <directory-version>) (value vleaf)))
                       (for-each visit (map cdr (contents v))))))))))
      ;;
      (for-each
       (lambda (fs)
         (visit (root-directory fs))
         (table-for-each
          (snapshot-table fs)
          (lambda (h k snap)
            (for-each (lambda (ver)
                        (visit (versioned-object ver)))
                      (value-sequence (node-version-map snap))))))
       (value-sequence (file-system-table *application*)))
      ;;
      vis)))

;;;
;;;  Build a table which maps each node
;;;  (which is not a root directory of a filespace) to a
;;;  <directory-version> which _first_ links to it
;;;
;;;  We will use this to determine which direction a 
;;;  (DIRVERSION, NODE) dependency runs.  If a DIRV is
;;;  the first linker for a NODE, then the NODE depends
;;;  on the DIRV.  Otherwise, the DIRV depends on the NODE.
;;;

(define (build-node-first-linked-map)
  (bind ((master dirs (find-all-nodes))
         (is-root (make-node-table))
         (first-linked (make-node-table)))
    ;;
    (for-each
     (lambda (fs)
       (table-insert! is-root (root-directory fs) #t))
     (value-sequence (file-system-table *application*)))
    ;;
    (for-each
     (lambda (dir)
       (for-each-version
        (versions dir)
        (lambda (path vleaf)
          (let ((nv (value vleaf)))
            (for-each
             (lambda (c)
               ;; c is an (entry-name . node) tuple,
               ;; where entry-name is a <string>
               ;; and node is a <node>
               (let ((l (table-lookup first-linked (cdr c))))
                 (if (or (not l)
                         (time<? (modification-time nv)
                                 (modification-time (cdr l))))
                     (table-insert! first-linked 
                                    (cdr c)
                                    (cons (car c) nv)))))
             (contents nv))))))
     dirs)
    ;;
    (table-for-each
     master
     (lambda (h (k <node>) (v <node>))
       (if (and (not (table-lookup first-linked k))
                (not (table-lookup is-root k)))
           (error "~s ~s is never linked !?\n" k v))))
    ;;
    (values first-linked master)))


(define (scan-transitions)
  (with-output-to-file
      "/tmp/summary.dat"
    (lambda ()
      (for-each
       (lambda (cr)
         (printh cr)
         (newline))
       (sort (value-sequence (change-request-table *application*))
             (lambda (a b)
               (< (id a) (id b))))))))
