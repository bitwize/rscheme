(define-module rs.util.quantity ()
  (&module
   (import usual-inlines)
   (import regex)
   (import tables)
   (import rs.sys.generic-math)
   ;;
   (load "dimen.scm")
   ;;
   (export define-unit
           quantity-unit
           quantity-value
           quantity/
           quantity*
           string->quantity
           string->unit)
   ;;
   ;;  What are the apropriate primitives for working with units
   ;;  themselves?  Should they be in a separate module, and "quantity"
   ;;  is simply the composition of dimensionless numbers with units
   ;;
   ))
