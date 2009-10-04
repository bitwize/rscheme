
(define-class <odbc-connection> (<object>)
)

;;; these are actually containers for raw C objects

(define-class <henv> (<object>) :bvec)
(define-class <hdbc> (<object>) :bvec)
(define-class <hstmt> (<object>) :bvec)

(define-class <sql-error> (<condition>)
  sql-retcode
  sql-state
  sql-error-message)


;;;

(define-class <column> (<object>)
  (column-number type: <fixnum>)
  (column-name type: <string>)
  (column-type type: <symbol>)
  (column-scale type: <fixnum>)
  (column-precision type: <fixnum>)
  (column-nullable? type: <boolean> init-value: #t))

;;;

(define-class <extraction-plan> (<object>)
  (properties type: <vector> init-value: '#())
  (result-class type: <<class>>)
  (tuple-buffer type: <byte-vector>)
  instructions
  (sql-stmt type: <hstmt>))

(define-class <extraction-plan-schema> (<object>)
  (properties type: <vector> init-value: '#())
  (result-class type: <<class>>)
  (tuple-buffer type: <byte-vector>)
  instructions
  (plan-literals type: <vector>)
  (result-columns type: <vector>))

