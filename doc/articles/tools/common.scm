
,(use tables
      paths
      rs.sys.threads.manager
      syscalls
      util.xml
      util.xpath
      rs.io.pushback
      graphics.geometry)

(define with-xml-refs (with-module util.sxml with-xml-application-entity-ref))

(define (for-each-sxml-element-from-file file proc)
  (call-with-input-file
      file
    (lambda (port)
      (let ((p (open-input-pushback-port port)))
        (let loop ()
          (if (not (eof-object? (peek-char p)))
              (begin
                (proc (sxml:root-element (port->sxml p)))
                (loop))))))))

