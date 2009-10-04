
(define (levenshtein-distance (a <string>) (b <string>))
  (if (< (string-length a) (string-length b))
      (levenshtein-distance* a b)
      (levenshtein-distance* b a)))

;;;
;;;  Our wrapper ensures that len(a) <= len(b)

(define-safe-glue (levenshtein-distance* (a <string>) (b <string>))
{
  unsigned char *ap = string_text( a );
  unsigned char *bp = string_text( b );
  unsigned ia = 0, na = string_length( a );
  unsigned ib = 0, nb = string_length( b );

  assert( na <= nb );  /* our wrapper ensures that len(a) < len(b) */

  /* since na<=nb, we can just watch na to go to zero */

  /* strip any common suffix */

  while ((na > 0) && (ap[na-1] == bp[nb-1])) {
    na--;
    nb--;
  }

  /* strip any common prefix */
  while ((na > 0) && (ap[ia] == bp[ib])) {
    ia++;
    ib++;
    na--;
    nb--;
  }

  /* if there is nothing left of the shorter string,
   * then the edit distance is equal to the length of what's
   * left of the longer string
   */

  if (na == 0) {
    REG0 = int2fx( nb );
    RETURN1();
  } else {
    unsigned i;
    int *v;

    assert( nb > 0 );

    REG2 = int2fx( ia );
    REG3 = int2fx( na );
    REG4 = int2fx( ib );
    REG5 = int2fx( nb );
    REG6 = bvec_alloc( sizeof(int) * (na+1), byte_vector_class );

    v = (int *)PTR_TO_DATAPTR( REG6 );
    for (i=0; i<=na; i++) {
      v[i] = i;
    }

    REG7 = int2fx( 1 ); /* outer (column) loop index */
    JUMP( 8, levenshtein_loop );
  }
}
("levenshtein_loop" {
  int ia = fx2int( REG2 ) - 1;
  int ib = fx2int( REG4 ) - 1;
  int distance = 0;

#define min(x,y) (((x)<(y))?(x):(y))

  unsigned na = fx2int( REG3 );
  unsigned nb = fx2int( REG5 );
  int *v = (int *)PTR_TO_DATAPTR( REG6 );
  unsigned char *ap = string_text( REG0 );
  unsigned char *bp = string_text( REG1 );
  unsigned i = fx2int( REG7 );          /* row in effective "matrix" */
  unsigned j;                           /* column in matrix */
  unsigned spca = string_length( REG0 );
  unsigned spcb = string_length( REG1 );

  /* per monotone (between JUMP's), we process
     exactly one row of the matrix.  Since the shorter
     string (a) is across the matrix, the column (j)
     represents an index into the 'a' string
   */

  if (i <= nb) {
    distance = v[0];
    v[0] = i;
    for (j=1; j<=na; j++) {
      int q = min( v[j-1]+1, v[j]+1 );
      int left = v[j];

      /*
      printf( "na=%d ia=%d j=%d  nb=%d ib=%d i=%d\n",
              na, ia, j,
              nb, ib, i );
      */
      assert( (ia+j) < spca );
      assert( (ib+i) < spcb );

      if (ap[ ia+j ] == bp[ ib+i ]) {
        v[j] = min( q, distance );
      } else {
        v[j] = min( q, distance+1 );
      }
      distance = left;
    }
    REG7 = ADD1( REG7 );
    BJUMP( 8, levenshtein_loop );
  } else {
    REG0 = int2fx( v[na] );
    RETURN1();
  }
}))

(define (t)
  (levenshtein-distance "gumbo" "gambol"))
