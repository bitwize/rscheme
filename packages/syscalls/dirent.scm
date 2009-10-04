
(define-safe-glue (scandir (path <raw-string>))
{
  REG0 = rs_scandir( path );
  RETURN1();
})
