
(def (recv-from* sock buf offset len peek? out-of-band? peer-class)

  (let ((fd-set (make-fd-set (list sock) '() '())))

    (let loop ()

      (bind ((readable writable exception (fd-select 0 fd-set)))

	(if (null? readable)
	    (begin (thread-sleep 0.5)
		   (loop))
	    (recv-from sock buf offset len peek? out-of-band? peer-class))))))