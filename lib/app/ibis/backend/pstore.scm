,(use rs.db.rstore rs.db.lss)

(define (version->string v)
  (case v
    ((0) "1.0")
    ((1) "1.1")
    ((2) "1.2")))

(define $current-model-version 2)

(define-class <system-state> (<object>)
  (open-store-app-root :sealed))

(define-class <open-store> (<system-state>)
  open-store-basename
  open-store-pstore
  open-store-lss)

(define-thread-var *open-store* #f)

(define (with-system-state state thunk)
  (thread-let ((*open-store* state))
    (thunk)))

(define (set-system-state! state)
  (set! *open-store* state))

(define (with-transient-system-state state thunk)
  (thread-let ((*open-store* (make <system-state>
                                   open-store-app-root: state)))
    (thunk)))

(define (current-application-root-object)
  (open-store-app-root *open-store*))


(define-method model-version ((self <application-root>))
  (get-property self 'version 0))

(define (check-for-compatibility ps)
  (let ((v (model-version (root-object ps))))
    (if (not (= v $current-model-version))
        (em 1371
            "Stored model is at version `~a', current model is version `~a'"
            (version->string v)
            (version->string $current-model-version)))))

(define (migrate-to-latest ps)
  (let* ((r (root-object ps))
         (v (model-version r))
         (f (get-migration-fn v $current-model-version)))
    (note 1372 "Migrating from model version `~a' to `~a'"
          (version->string v)
          (version->string $current-model-version))
    (bind ((new-root info (f r)))
      (note 1373 "Visited ~d objects, migrated ~d" 
            (car info) 
            (cadr info))
      (set-property! r 'version $current-model-version)
      (let ((cr (commit ps new-root)))
        (note 1374 "Baseline ~a commit record: ~s" 
              (version->string $current-model-version) 
              cr)
        (values)))))

(define (config-pstore ps)
  (register-indirect-page ps 10 (vector <information-base>
                                        <relationship-1_1>
                                        <ibase-transaction>
                                        <index-entry>
                                        <index>
                                        <user-1_0>
                                        <component>
                                        <access>
                                        <priviledge-group>
                                        <authority>
                                        <user>
                                        <ibis-relationship>))
  (register-indirect-page ps 11 (vector <item>  ; actually abstract
                                        <note>
                                        <issue>
                                        <theme>
                                        <position>
                                        <argument>
                                        <affirmative>
                                        <negative>
                                        <audit-note>)))

(define (make-file-vector basename)
  (vector (string-append basename ".v0")
          (string-append basename ".v1")))

(define (store-exists? basename)
  (let ((v (make-file-vector basename)))
    (or (os-file-exists? (vector-ref v 0))
        (os-file-exists? (vector-ref v 1)))))

(define (open-and-config basename lss-proc tag)
  (let* ((v (make-file-vector basename))
         (l (if tag
                (lss-open v tag)
                (lss-proc v)))
         (p (open-pstore-on-lss l)))
    (config-pstore p)
    (values l p)))

(define (create-ibis-store basename #key (admin default: #f))
  (if (store-exists? basename)
      (em 1378 "~a: Store already exists" basename))
  (bind ((l p (open-and-config basename lss-create #f)))
    (lss-set-tip l 1)
    (commit p (make-information-base admin-user: (or admin
                                                     (getenv "USER")
                                                     "root")))
    (make <open-store>
          open-store-basename: basename
          open-store-app-root: (root-object p)
          open-store-pstore: p
          open-store-lss: l)))

(define (reopen-db! (s <open-store>))
  (close-persistent-store (open-store-pstore s))
  (bind ((l p (open-and-config (open-store-basename s) lss-open #f)))
    (check-for-compatibility p)
    (set-open-store-pstore! s p)
    (set-open-store-app-root! s (root-object p))
    (set-open-store-lss! s l)
    s))

(define (migrate-ibis-store basename #key (tag default: #f))
  (if (not (store-exists? basename))
      (em 1379 "~a: No such store" basename))
  (bind ((l p (open-and-config basename lss-open tag)))
    (print (root-object p))
    (migrate-to-latest p)
    (close-persistent-store p)))

(define (open-ibis-store basename #key (tag default: #f))
  (if (not (store-exists? basename))
      (em 1379 "~a: No such store" basename))
  (bind ((l p (open-and-config basename lss-open tag)))
    (check-for-compatibility p)
    (make <open-store>
          open-store-basename: basename
          open-store-app-root: (root-object p)
          open-store-pstore: p
          open-store-lss: l)))

(define *do-auto-commit* #t)

(define (auto-commit)
  (if *do-auto-commit*
      (let* ((ps (open-store-pstore *open-store*))
             (n (num-dirty-pages ps)))
	(if (= n 0)
            (note 1324 "No changes to commit")
            (let ((cr (commit ps)))
              (note 1325 "For ~d dirty pages, commit record: ~s" n cr))))
      (note 1326 "Auto-commit disabled")))

(define (auto-rollback)
  (let* ((ps (open-store-pstore *open-store*))
         (n (num-dirty-pages ps)))
    (if (= n 0)
        (note 1331 "Nothing to roll back")
        (begin
          (note 1331 "Rolling back from ~d dirty pages" n)
          (reopen-db! *open-store*)))))

  
