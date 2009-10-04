(define-safe-glue (parse (str <raw-string>))
{
  int left;
  extern int did_read_eof;
  extern obj try_to_parse( obj, int * );

  did_read_eof = 0;
  REG0 = try_to_parse( raw_str, &left );
  REG1 = int2fx(left);
  REG2 = rb_to_bo( did_read_eof );
  RETURN(3);
})

(define-safe-glue (set-yy-debug! flag)
{
  extern int yydebug;

  yydebug = truish(flag);
  RETURN0();
})
