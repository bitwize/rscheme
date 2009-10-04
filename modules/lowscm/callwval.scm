#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/lowscm/callwval.scm
 |
 |          Copyright (C) 2002 Joerg.Wittenberger@pobox.com
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.6
 | File mod date:    2005-02-25 13:25:42
 | System build:     v0.7.3.4-b7u, 2007-05-30
 |
 `------------------------------------------------------------------------|#

;;; R5RS

(define-safe-glue (call-with-values (producer <function>) 
                                    (consumer <function>))
{
  obj next = consumer;
  REG0 = consumer;
  SAVE_CONT1(done_with_producer);
  APPLY(0, producer);
}
("done_with_producer" {
  obj consumer = PARTCONT_REG(0);
  RESTORE_CONT_REG();
  APPLY(arg_count_reg, consumer);
}))

;;;
;;;  procedural composition
;;;

(define-glue (%composite) :template
{
  USE_FUNCTION_ENVT();
  {
    PUSH_PARTCONT( composite_1, 1 );
    SET_PARTCONT_REG( 0, LEXREF0(0) );
  }
  if (arg_count_reg == 0) {
    RETURN0();
  } else {
    RETURN(arg_count_reg);
  }
}
("composite_1" {
  obj lst = PARTCONT_REG(0);
  RESTORE_CONT_REG();

  if (NULL_P( lst )) {
    /* if arg_count_reg == 0, then REG0 was already set to #f 
     * by whoever returned into the composite_1 continuation
     */
    RETURN( arg_count_reg );
  } else {
    PUSH_PARTCONT( composite_1, 1 );
    SET_PARTCONT_REG( 0, pair_cdr(lst) );
    APPLY( arg_count_reg, pair_car(lst) );
  }
}))

(define (compose (fn1 <function>) . frest)
  (if (null? frest)
      fn1
      (make-gvec <closure>
                 %composite
                 (make-gvec <binding-envt> '() (reverse! (cons fn1 frest))))))
