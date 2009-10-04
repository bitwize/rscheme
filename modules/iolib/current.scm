;;; analogue of unix's fd 0, 1, 2
;;;
(%early-once-only

(define $standard-input-port
  (make <std-input-port>
	name: "stdin"
	file-stream: (stdin)))

(define $standard-output-port
  (make <std-output-port>
	name: "stdout"
	file-stream: (stdout)))

(define $standard-error-port
  (make <std-output-port>
	name: "stderr"
	file-stream: (stderr)))
)
;;; reinitialize the default ports...

(set-file-stream! $standard-input-port (stdin))
(set-file-stream! $standard-output-port (stdout))
(set-file-stream! $standard-error-port (stderr))

;;; redirect these as convenient

(define-thread-var *input-port* $standard-input-port)
(define-thread-var *output-port* $standard-output-port)
(define-thread-var *error-port* $standard-error-port)

;;; analogue of unix's /dev/tty
;;; 
;;; these should only be rebound by different "sessions"
;;; (e.g., if a user is logging in to the process over a socket)
;;;
;;; For example, these are used as the i/o for break loops

(define-thread-var *console-input-port*  $standard-input-port)
(define-thread-var *console-output-port* $standard-output-port)
(define-thread-var *console-error-port* $standard-error-port)

;;;
;;; standard procedures to access these
;;;

(define-inline (current-input-port)
  *input-port*)

(define-inline (current-output-port)
  *output-port*)

(define-inline (current-error-port)
  *error-port*)
