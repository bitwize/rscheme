

(for-each
 (lambda ((self <file>))
   (for-each-version
    (versions self)
    (lambda (path vleaf)
      (let (((fv <file-version>) (value vleaf)))
        (if (instance? (contents fv) <binary-file-content>)
            (format #t "~s\n" fv))))))
 ;;
 (select 
  (rcurry instance? <file>)
  (value-sequence
   (get-property *application* 'migrate:master-node-index))))

