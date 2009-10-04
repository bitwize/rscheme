;;;
;;;  Content Model
;;;

(define-class <content> (<object>) :abstract)

;;;

(define-class <flow> (<content>)
  (flow-tag init-value: 'a)
  content)

(define-class <flow-content> (<content>) :abstract) ; appears in a <flow>

(define-class <vbreak> (<flow-content>)
  section)

(define-class <flow-table> (<flow-content>)
  (align init-value: 'center)
  (column-widths init-value: '())
  (row-heights init-value: '())
  (num-header-rows init-value: 0)
  (style init-value: #f)
  content)                              ; list of list of <table-cell>'s

(define-class <table-cell> (<object>)
  (owner type: <flow-table>)
  (content type: <flow>)
  (vspan init-value: 1)
  (hspan init-value: 1))

(define-method cell-margin ((self <table-cell>))
  2)

(define-method cell-posn ((self <table-cell>))
  (let oloop ((r 0)
              (rows (content (owner self))))
    (if (null? rows)
        (values -1 -1)
        (let iloop ((c 0)
                    (cols (car rows)))
          (if (null? cols)
              (oloop (+ r 1) (cdr rows))
              (if (eq? (car cols) self)
                  (values r c)
                  (iloop (+ c 1) (cdr cols))))))))
        
    

(define-class <flow-vbox> (<flow-content>)
  (properties init-value: '#())
  (align init-value: 'left)
  height
  render-proc)          ; 3 args: ((self <flow-vbox>) 
                        ;          (width <real>)
                        ;          (dev <graphic-device>))

(define-class <para> (<flow-content>)
  style                                 ; a <symbol> or a <paragraph-style>
  content)

(define-class <inline-item> (<content>) :abstract
  (properties init-value: '#()))

(define *debug-pi* #f)
(define-thread-var *sticky-properties* '())

(define-method initialize ((self <inline-item>))
  (if (pair? *sticky-properties*)
      (for-each (lambda (p)
                  (format #t "SETTING STICKY ~s on ~s\n" p self)
                  (set-property! self (car p) (cdr p)))
                *sticky-properties*))
  (if *debug-pi*
      (set-property! self 'debug #t)))

(define-class <text-run> (<inline-item>)
  style                                 ; <symbol> or <style>
  (content type: <string>))

(define-method annotation ((self <text-run>))
  #f)

(define-class <annotated-text-run> (<text-run>)
  (annotation init-value: #f))

(define-class <anchor-scope> (<object>)
  (index init-function: make-table))

(define-class <anchor> (<inline-item>)
  (scope type: <anchor-scope>)
  (name type: <object>)
  ;; #f => unknown (anchor has not been laid out yet)
  (relative-x init-value: #f)           ; relative to the <hlist>'s x
  (owner init-value: #f)                ; a <vlist>
  (relative-y init-value: #f))          ; within the owning <vlist>

(define-class <computed-inline> (<inline-item>)
  (properties init-value: '#())
  inline-computer)          ; self para char-style => (text-run ...) 
                            ;                         char-style 


