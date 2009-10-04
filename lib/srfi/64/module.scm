(define-module srfi.64 ()
  (&module
   (implements SRFI-64 srfi-64)
   ;;
   (import usual-inlines
           syscalls
           paths
           srfi.39
           rs.util.properties)
   ;;
   (load "tester.scm")
   (load "xmlrunner.scm")
   ;;
   (export xml-test-runner-create)
   ;;
   (export test-begin
           test-end

           print-test-summary
           
           test-runner-factory
           test-runner-current

           test-runner-on-test
           test-runner-on-test!
           test-runner-create
           test-runner-test-name
           test-with-runner
           test-assert
           test-equal
           test-eqv
           test-eq
           test-error
           test-thunk
           test-group
           test-group-with-cleanup

           test-match-name
           test-match-all
           test-match-any
           test-match-nth
           test-skip
           test-expect-fail
           
           test-runner-group-path

           test-runner-pass-count
           test-runner-fail-count
           test-runner-xpass-count
           test-runner-xfail-count
           test-runner-skip-count

           test-runner-null
           test-runner-simple
           test-on-test-simple
           test-on-final-simple
           test-runner-on-test
           test-runner-on-test!
           test-runner-on-final
           test-runner-on-final!
           test-runner-aux-value
           test-runner-aux-value!

           test-runner-factory
           test-apply
           ;;
           test-result-alist
           test-result-ref
           test-result-set!
           test-result-clear
           )))
