#! /u/donovan/bin/rsf -script

,(use rs.util.stdopt)

(define *argset*
  (make <opt-spec>
        options: '((#\v verbose)
                   (#\n number 1)
                   ("foo" foo)
                   ("bar" bar 1))))

(define (main args)
  (let ((non (for-each-opt
              args
              *argset*
              (lambda (opt #optional optarg)
                (if optarg
                    (format #t "option ~s arg ~s\n" opt optarg)
                    (format #t "option ~s\n" opt))))))
    (format #t "non options: ~s\n" non)
    #t))

