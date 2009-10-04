#|------------------------------------------------------------*-Scheme-*--|
 | File:    pg/gettuple.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rosette.com>
 |          as part of the RScheme project, licensed for free use
 |
 | Version: 1.2
 | Date:    1999-01-12 12:28:45
 | Build:   v0.7.3.4-b7u, 2007-05-30
 |
 | Purpose: Binary cursor tuple extraction to RScheme instance
 `------------------------------------------------------------------------|#

;;;
;;;  code to support extracting an entire tuple as an instance
;;;  of a given class
;;;
;;;  it works by interpreting an EXTRACTION PLAN against a given
;;;  tuple number of a result
;;;
;;;  the plan is stored as a byte vector of extraction instructions,
;;;  one per slot of the target class (gen_class).  Each instruction
;;;  is two bytes, the first byte being the extraction type, the
;;;  second byte being the field number

(define-pg-glue (extract-tuple (result <pg-result>) 
				 (tuple_num <raw-int>)
				 gen_class 
				 plan 
				 proto)
  literals: ((& <time>))
{
  REG0 = rspg_extract_tuple( result, tuple_num, gen_class, plan, proto, 
				     TLREF(0) );
  RETURN1();
})
