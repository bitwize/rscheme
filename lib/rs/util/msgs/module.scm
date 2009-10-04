(define-module rs.util.msgs (&module)
  (&module
   (import usual-inlines)
   (import tables syscalls sort)
   (load "message.scm")
   (load "errmsg.scm")
   (load "debug.scm")
   (load "warn.scm")
   (load "note.scm")
   ;;
   ;;  usage:
   ;;
   ;;     (define-message-table NAME [TAG])
   ;; e.g.,
   ;;     (define-message-table graphics.charpath 604)
   ;;
   (export make-anonymous-message-table
           define-message-table
           define-external-msg
           setup-message-table)
   (export alloc-message)
   (export with-message-prefix
           sm em dm wm note fm
           debug-messages-disable
           debug-messages-enable
           message-table-config
           message-enable
           message-disable
           show-message
           debug-message-enabled?
           other-message-enabled?)
   ;;
   (load "queue.scm")
   (export make-message-queue
           <message-queue>
           with-message-dest            ; a message-queue or output-port
           set-message-dest!
           current-message-dest
           message-extract-and-clear
           to-sxml
           make-message-forwarder)      ; a splitter
   ;;
   ;;  protocol so other modules can write their own message dests
   ;;
   (export display-message
           recompile-message-displayer
           <message>
           <fatal-message>
           <error-message>
           <warning-message>
           <info-message>
           <debug-message>
           <notice-message>
           finish-message-init
           <message-dest> 
           time->msg-time)
   (export message-type
           message-type-char
           message-id
           message-source
           message-default-text
           get-message-table
           get-message-table-by-id
           message-table-id)
   ;;
   ;;  Standard message macros:
   ;;
   ;;    (wm [ID] WARNING_MSG ...)
   ;;    (note [ID] NOTE_MSG ...)
   ;;    (em [ID] ERROR_MSG ...)             ; calls (error ...)
   ;;    (sm [ID] ERROR_MSG ...)             ; calls (signal ...)
   ;;    (dm [at: PLACE] [type: TYPE] [ID] DEBUG_MSG ...)
   ;;
   ;; where TYPE is one of: fatal error warning debug notice
   ))

(with-module mlink
  (set-usage-hooks!
   (get-module 'rs.util.msgs)
   (list (lambda (envt)
           (with-module compiler
             (with-module rs.util.msgs
               (bind! envt (make <top-level-var>
                                 name: '*messages*
                                 value: (make-anonymous-message-table)))))))))
'rs.util.msgs
