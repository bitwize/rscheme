
(define (enumerate-nodes (self <file-space>) (tops <list>))
  (enumerate-nodes* self
                    tops
                    (lambda (node-version path)
                      (list (if (instance? node-version <directory-version>)
                                'dir
                                'file)
                            (steps->fs-path (reverse path)) 
                            node-version))))

(define (enumerate-subdirs (self <file-space>) (tops <list>))
  (enumerate-nodes* self 
                    tops
                    (lambda (node-version path)
                      (if (instance? node-version <directory-version>)
                          (steps->fs-path (reverse path))
                          #f))))

(define (enumerate-nodes* (self <file-space>) (tops <list>) addfilter)
  (let ((proc (the-version-proc self))
        (tbl (make-fixnum-table))
        (q (make-dequeue)))
    ;;
    (define (add nodev path)
      (let ((i (id (versioned-object nodev))))
        (if (not (table-lookup tbl i))
            (let ((x (addfilter nodev path)))
              (table-insert! tbl i #t)
              (if x
                  (dequeue-push-back! q x))))
        (values)))
    ;;
    (for-each 
     (lambda ((top <fs-absolute-path>))
       (enumerate-subdirs-rec (find-version self top)
                              proc
                              (reverse (steps top))
                              add))
     tops)
    ;;
    (vector->list (dequeue-state q))))


(define-method enumerate-subdirs-rec ((self <file-version>)
                                      vproc path add)
  (add self path))
                                      
(define-method enumerate-subdirs-rec ((self <directory-version>) 
                                      vproc path add)
  (add self path)
  ;;
  (fs-dir-for-each
   self
   (lambda (name node)
     (enumerate-subdirs-rec (vproc node) vproc (cons name path) add))))
