(display "SourceBase 1.1\n")

(display "using modules...\n")

,(use syscalls
      calendar)

(define *load-time* (time))
,(use paths tables sets)

;;;---------------------------------- 0.7.4 ---------------
,(use rs.db.rstore
      rs.db.lss)
,(use rs.util.properties)

(define (object-allocation-area item)
  (object->allocation-area item))

(define (setup-indirect-page ps pg v)
  (register-indirect-page ps pg v))


;;;---------------------------------- 0.7.3 ---------------
;,(use rstore)
;;;--------------------------------------------------------



,(use unixm objsys sort imageio)
,(use regex)
,(use rs.sys.threads.manager)

;; 0.7.1 functions not in 0.7.2 or 0.7.3

(define (unionq . lsts)
  (reduce (lambda (s1 s2)
	    (union s1 s2 test: eq?))
	  '()
	  lsts))

;; changes to 0.7.3 semantics

(define-method to-string ((self <object>))
  (let ((p (open-output-string)))
    (format p "#[~s]" (name (object-class self)))
    (close-output-port p)))

;; our modules...

;(define-method set-primary-prompt! ((self <input-port>) arg))
;(define-method set-completions! ((self <input-port>) arg))

(define load
  (let ((load load))
    (lambda (arg)
	(format #t "loading: ~a...\n" arg)
	(load arg))))

;;
;;  forward decls
;;

(define-generic-function versioned-object)

;;
;;  errors, hooks, user exits and callbacks
;;

(load "callback/errors.scm")

;;
;;  load version-tree code
;;

(load "versionids/versionmap.scm")
(load "versionids/newleaf.scm")

;;
;; load the pathname code and other utils
;;

(load "util/fspath.scm")
(load "util/io.scm")
(load "util/tar.scm")
(load "util/iterator.scm")

;;
;;  load the data model
;;

(load "datamodel/app.scm")
(load "datamodel/audit.scm")
(load "datamodel/users.scm")
(load "datamodel/groups.scm")
(load "datamodel/fs.scm")
(load "datamodel/fsnodes.scm")
(load "datamodel/content.scm")
(load "datamodel/changereq.scm")
(load "datamodel/workitem.scm")
(load "datamodel/property.scm")

;;
;;  load the kernel code
;;

(load "kernel/kassert.scm")
(load "kernel/application.scm")
(load "kernel/allocation.scm")
(load "kernel/policy.scm")
(load "kernel/pathlook.scm")
(load "kernel/fsnodes.scm")
(load "kernel/pathmap.scm")
(load "kernel/link.scm")
(load "kernel/textcontent.scm")
(load "kernel/linediff.scm")
(load "kernel/workitem.scm")
(load "kernel/changereq.scm")
(load "kernel/locks.scm")
(load "kernel/properties.scm")
(load "kernel/diverge.scm")

;;
;;  load common user interface code
;;

(load "ui/history.scm")
(load "ui/changereq.scm")
(load "ui/node.scm")
(load "ui/lineitem.scm")
(load "ui/mailqueue.scm")
(load "ui/user.scm")
(load "ui/filesys.scm")
(load "ui/userformat.scm")
(load "ui/report.scm")

;;
;;  load the pstore interface
;;

(load "pstore/appclasses.scm")
(load "pstore/globals.scm")
(load "pstore/setup.scm")
(load "pstore/create.scm")
(load "pstore/open.scm")

;;
;;  load the API functions
;;

(load "api/framework.scm")
(load "api/filesys.scm")
(load "api/snapshot.scm")
(load "api/snapex.scm")
(load "api/database.scm")
(load "api/dir.scm")
(load "api/file.scm")
(load "api/user.scm")
(load "api/group.scm")
(load "api/changereq.scm")

;;
;;  load the shell
;;

(load "shell/vshparse.scm")
(load "shell/state.scm")
(load "shell/utils.scm")
(load "shell/viewcmds.scm")
(load "shell/editcmds.scm")
(load "shell/envcmds.scm")
(load "shell/eval.scm")
(load "shell/main.scm")

;;
;;  load the CLI server
;;
;; of course, only 0.7.3.1 and later implement `if-implements'
(if-implements (version >= 0 7 3 1)
  (begin)
  (load "telnetd/readstr.scm"))

(load "telnetd/server.scm")   ;; thread-aware (rs-0.7.3) impl
;;(load "telnetd/simple.scm") ;; single-threaded version

(load "cli/expandkeys.scm")
(load "cli/admin.scm")
(load "cli/view.scm")
(load "cli/edit.scm")
(load "cli/file.scm")
(load "cli/dir.scm")
(load "cli/changereq.scm")
(load "cli/snapshot.scm")
(load "cli/tee.scm")
(load "cli/fs.scm")
(load "cli/group.scm")
(load "cli/report.scm")
(load "cli/parsewhere.scm")
(load "cli/cvstime.scm")
(load "cli/main.scm")

;;
;;  end-user hooks
;;
(load "hooks.scm")

;;
;;  some test functions
;;

;(load "test.scm")
;(load "migrate/main.scm")


(format #t "load took ~a\n" (time-time (time) *load-time*))
