
(define *migration-state* #f)

(define (database-migrate db-name pcixf-path vxf-path lvl-map dest-path)
  (let ((a (make-application db-name)))
    (set-state! a 'migrate)
    (set-properties! 
     a 
     (list
      (cons 'cmvc-data
	    (vector 'migrate 
		    $migration-steps
		    pcixf-path
		    #f   ;; [3] tables assq-list
		    #f   ;; [4] Component index
		    #f   ;; [5] Defect index
		    #f   ;; [6] File index
		    #f   ;; [7] Level index
		    #f   ;; [8] Path index
		    #f   ;; [9] Release index
		    #f   ;; [10] Track index
		    #f   ;; [11] User index
		    #f   ;; [12] Version index
		    (make <audit-log-entry>
			  user: "sysadm"
			  timestamp: *timestamp*
			  arg-list: (vector db-name
					    pcixf-path 
					    vxf-path
					    lvl-map
					    dest-path)
			  operation: 'migrate
			  result: '#uninit
			  info: '())
		    vxf-path  ;; [14] vxf base path
		    #f        ;; [15] source-file table (vcs)
		    '()       ;; [16] active checkouts
		    lvl-map)))) ;; [17] level map base
    (create-database a dest-path)
    ;;
    (set! *migration-state* (migration-data))
    (set! *audit-entry* (vector-ref *migration-state* 13))))

(define (migrated-source-file src-id)
  (or (table-lookup (vector-ref *migration-state* 15) src-id)
      (error "~s: not a valid source-id" src-id)))

(define (migration-data)
  (cdr (assq 'cmvc-data (properties *application*))))

(define (continue-migration dbpath)
  (database-connect dbpath 'update)
  (set! *migration-state* (migration-data))
  (set! *audit-entry* (vector-ref *migration-state* 13)))

(define (do-migration-increment)
  (let ((h (vector-ref *migration-state* 1)))
    (if (pair? h)
	(if (do-migration-step (car h))
	    (vector-set! *migration-state* 1 (cdr h))))
    (commit *pstore*)
    (null? h)))


(define (migration-progress fmt . args)
  (apply format #t fmt args))

(define $migration-steps '(110 120 
			   210 220 230 240 
			   310 320 330 340
			   410 420 430 435 440 450 460 470
			   920 930 990))

  
(define (do-migration-step i)
  (migration-progress "doing migration step: ~d\n" i)
  (case i
    ((110) (migrate-cmvc-tables))
    ((120) (migrate-cmvc-indices))
    ((210) (migrate-basic-config))
    ((220) (migrate-users))
    ((230) (migrate-components))
    ((240) (migrate-user-authority))
    ((310) (migrate-defects))
    ((320) (migrate-notes))
    ((330) (migrate-defect-ownership))
    ((340) (migrate-defect-history))
    ((410) (migrate-releases))
    ((420) (migrate-source-files))
    ((430) (migrate-files))
    ((435) (migrate-files-out))
    ((440) (migrate-levels))
    ((450) (migrate-tracks))
    ((460) (migrate-changes))
    ((470) (migrate-fix-records))
    ((920) (migrate-defect-finalize))
    ((930) (migrate-unlock-dirs))
    ((990) (migrate-finalize))
    (else (error "unknown migration step: ~s\n" i))))

