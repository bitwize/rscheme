;;
;;  Pass 1
;;
;;  Create the indices
;;

(define (migrate-cmvc-tables)
  (let ((tbl-list '())
        (dir (vector-ref *migration-state* 2))
	(area (make-area)))
  (let-syntax ((migr (syntax-form (row-class table-name)
  			(let ((tbl (load-pcixf-table
				    area
				    (string-append 
					dir
					"/"
					(symbol->string (mquote table-name)))
				    row-class)))
			  (set! tbl-list (cons (cons (mquote table-name) tbl)
			    		       tbl-list))))))
        (migr Access AccessTable)
        (migr Approval Approvals)
        (migr Approver Approvers)
        (migr Authority Authority)
        (migr Cfgcomproc Cfgcomproc)
        (migr Cfgrelproc Cfgrelproc)
        (migr Change Changes)
        (migr CompMember CompMembers)
        (migr Component Components)
        (migr Config Config)
        (migr Coreq Coreqs)
        (migr Defect Defects)
        (migr Environment Environments)
        (migr File Files)
        (migr FileOut FilesOut)
        (migr Fix Fix)
        (migr History History)
        (migr Host Hosts)
        (migr Interest Interest)
        (migr LevelMember LevelMembers)
        (migr Level Levels)
        (migr Note Notes)
        (migr Notification Notification)
        (migr Path Path)
        (migr Release Releases)
        (migr Sequence Sequence)
        (migr Size Sizes)
        (migr Test Tests)
        (migr Track Tracks)
        (migr User Users)
        (migr Verify Verify)
        (migr Version Versions))
    (vector-set! *migration-state* 3 tbl-list)
    #t))
