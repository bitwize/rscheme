#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/objsys/getnset.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.8
 | File mod date:    1997-11-29 23:10:38
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  objsys
 |
 | Purpose:          Accessor and setter code templates
 `------------------------------------------------------------------------|#

;;
;;  getter and setter templates
;;

(define-glue (getter-template self)  :template
{
obj slotnum;

    USE_FUNCTION_ENVT();
    COUNT_ARGS(1);
    slotnum = LEXREF0(0);
    assert( OBJ_ISA_PTR(self) 
	    && FXWORDS_TO_RIBYTES(slotnum) < SIZEOF_PTR(self) );
    REG0 = gvec_read( self, FXWORDS_TO_RIBYTES(slotnum) );
    RETURN1();
})

(define-glue (setter-template self)  :template
{
obj slotnum;

    USE_FUNCTION_ENVT();
    COUNT_ARGS(2);
    slotnum = LEXREF0(0);
    assert( OBJ_ISA_PTR(self) 
	    && FXWORDS_TO_RIBYTES(slotnum) < SIZEOF_PTR(self) );
    gvec_write( self, FXWORDS_TO_RIBYTES(slotnum), REG1 );
    REG0 = REG1;
    RETURN1();
})

(define-glue (restricted-setter-template self)  :template
  literals: ((& invalid-setter-arg))
{
obj slotnum;
obj setter;

    setter = envt_reg;
    USE_FUNCTION_ENVT();
    COUNT_ARGS(2);
    slotnum = LEXREF0(0);
    assert( OBJ_ISA_PTR(self) 
	    && FXWORDS_TO_RIBYTES(slotnum) < SIZEOF_PTR(self) );
    if (instance_p( REG1, LEXREF0(1) ))
     {
       gvec_write( self, FXWORDS_TO_RIBYTES(slotnum), REG1 );
       REG0 = REG1;
       RETURN1();
     }
    else
     {
       REG2 = REG0;
       REG0 = setter;
       APPLYF(3,TLREF(0));
     }
})

(define-class <invalid-slot-set> (<error>)
  (invalid-slot-set-slot type: <slot-descriptor>)
  (invalid-slot-set-object type: <object>)
  (invalid-slot-set-arg type: <object>))


(define (invalid-setter-arg (setter <setter>) arg rcvr)
  (error (make <invalid-slot-set>
	       invalid-slot-set-object: rcvr
	       invalid-slot-set-slot: (slot-descriptor setter)
	       invalid-slot-set-arg: arg)))

(define-method display-object ((self <invalid-slot-set>) port)
  (__format port "invalid argument to setter for slot ~s of a ~s\n"
	    (name (invalid-slot-set-slot self))
	    (class-name (object-class (invalid-slot-set-object self))))
  (__format port ">> argument is: ~#*@60s\n" (invalid-slot-set-arg self))
  (__format port ">> required type is: ~s\n" 
	    (class-name (type-restriction (invalid-slot-set-slot self))))
  (__format port ">> receiver object is: ~#*@60s\n"
	    (invalid-slot-set-object self)))
