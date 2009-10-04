
(define-class <bit-board> (<object>) :bvec)

(define-safe-glue (new-bit-board)
  literals: ((& <bit-board>))
{
  obj x = bvec_alloc( sizeof(unsigned long long), TLREF(0) );
  *((unsigned long long *)PTR_TO_DATAPTR( x )) = 0;
  REG0 = x;
  RETURN1();
})

(define-safe-glue (bbset (self <bit-board>) (file <symbol>) (rank <raw-int>))
{
  int f = ((symbol_text( file )[0] | 0x20) - 'a') & 0x7;
  int r = (rank - 1) & 0x7;

  obj n = bvec_alloc( sizeof(unsigned long long), CLASSOF_PTR(self) );
  unsigned long long s, *d;

  s = *(unsigned long long *)PTR_TO_DATAPTR( self );
  d = (unsigned long long *)PTR_TO_DATAPTR( n );
  s |= ((unsigned long long)(0x80 >> f)) << (r*8);
  *d = s;
  REG0 = n;
  RETURN1();
})

(define-safe-glue (bb-is-set? (self <bit-board>) (file <symbol>) (rank <raw-int>))
{
  int f = ((symbol_text( file )[0] | 0x20) - 'a') & 0x7;
  int r = (rank - 1) & 0x7;
  unsigned long long s;

  s = *(unsigned long long *)PTR_TO_DATAPTR( self );
  s &= ((unsigned long long)(0x80 >> f)) << (r*8);
  REG0 = (s == 0) ? FALSE_OBJ : TRUE_OBJ;
  RETURN1();
})

(define-safe-glue (bexec (self <bit-board>) (cmd <string>) #rest)
{
  unsigned long long tmp, stack[10], regset[10], *sp;
  int r, f, i, ni = string_length( cmd );
  char *p = string_text( cmd );
  obj t;
  int argc = arg_count_reg - 2;

  sp = &stack[10];
  *--sp = *(unsigned long long *)PTR_TO_DATAPTR( self );
  printf( "init => %016llx\n", *sp );

  for (i=0; i<ni; i++) {
    printf( "  OP '%c', stack depth %d\n", p[i], &stack[10] - sp );
    switch (p[i]) {

      case '\t':
      case '\n':
      case ' ':
        break;  /* skip whitespace */

      default:
        scheme_error( "bexec: bad opcode 0x~02x", 1, int2fx( p[i] ) );
        break;

      case '0': case '1': case '2':
      case '3': case '4': case '5':
      case '6': case '7': case '8':
      case '9':
        if ((p[i] - '0') >= argc) {
          scheme_error( "bexec: arg[~a] out of range", 1, MAKE_ASCII_CHAR(p[i]) );
        }
        t = reg_ref( (p[i] - '0' ) + 2 );
        *--sp = *(unsigned long long *)PTR_TO_DATAPTR(t);
        printf( "push(arg[%d]) => %016llx\n", p[i]-'0', *sp );
        break;

      case 'S':         /* set */
        r = p[++i];
        f = p[++i];
        if ((r < 1) || (r > 8)) {
          scheme_error( "bexec: SET rank ~d out of range 1..8", 1, int2fx(r) );
        }
        if ((f < 1) || (f > 8)) {
          scheme_error( "bexec: SET file ~d out of range 1..8", 1, int2fx(f) );
        }
        *sp |= ((unsigned long long)(0x80 >> (f-1))) << ((r-1)*8);
        printf( "set(%c,%d) => %016llx\n", "?abcdefgh"[f], r, *sp );
        break;

      case '!':         /* new */
        *--sp = 0;
        break;

      case '=':         /* dup */
        sp[-1] = sp[0];
        sp--;
        break;

      case '<':         /* load */
        *--sp = regset[p[++i]-'0'];
        break;

      case '>':         /* store */
        regset[p[++i]-'0'] = *sp++;
        break;

      case 'x':         /* exch */
        tmp = sp[0];
        sp[0] = sp[1];
        sp[1] = tmp;
        break;

      case '&':         /* and */
        sp[1] = sp[0] & sp[1];
        sp++;
        break;

      case '|':         /* or */
        sp[1] = sp[0] | sp[1];
        sp++;
        break;

      case '~':         /* not */
        sp[0] = ~sp[0];
        break;

      case 'w':         /* west */
        sp[0] = (sp[0] & 0x7F7F7F7F7F7F7F7FULL) << 1;
        break;

      case 'e':         /* east */
        sp[0] = (sp[0] & 0xFEFEFEFEFEFEFEFEULL) >> 1;
        break;

      case 'n':
        sp[0] = (sp[0] << 8);
        break;

      case 's':
        sp[0] = (sp[0] >> 8);
        break;
    }
  }
  printf( "stack = %d items, top = %016llx\n", &stack[10] - sp, *sp );
  REG0 = bvec_alloc( sizeof(unsigned long long), CLASSOF_PTR(self) );
  *((unsigned long long *)PTR_TO_DATAPTR( REG0 )) = *sp;
  RETURN1();
})

