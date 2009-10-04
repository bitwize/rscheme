#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/corelib/imageio.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.7
 | File mod date:    1997-11-29 23:10:39
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  corelib
 |
 | Purpose:          Image reading and writing (old 0.5-based format)
 `------------------------------------------------------------------------|#

(define-safe-glue (save-image (path <string>)
			      root
			      (ref_vec <vector>)
			      (ref_names <vector>)
			      rplc_tbl) ;; (union <object-table> #f)
{
extern obj rs_save_image_file( obj root, obj ref_vec, obj ref_names, 
			       obj rplc, obj out_info );

    rs_save_image_file( root, ref_vec, ref_names, rplc_tbl, path );
    RETURN0();
})

(define-glue (load-image path link options)
{
extern obj load_image_file( const char *the_path, 
			    obj the_link_table,
			    obj opt, int *vers );
int v;

    if (arg_count_reg == 2)
      options = FALSE_OBJ;
    else
      COUNT_ARGS(3);

    REG0 = load_image_file( string_text(path), link, options, &v );
    REG1 = int2fx(v);
    RETURN(2);
})

(define-glue (image-write-header path hdr)
{
extern FILE *os_fopen( const char *f_path, const char *mode );
FILE *f = os_fopen( string_text(path), "r+b" );
UINT_32 n;
#define FILE_HDR_OFFSET (128)

    fseek( f, 0L, SEEK_SET );
    n = SIZEOF_PTR(hdr);
    if (n > FILE_HDR_OFFSET)
        n = FILE_HDR_OFFSET;
    fwrite( PTR_TO_DATAPTR(hdr), 1, n, f );
    fclose(f);
    REG0 = int2fx( n );
    RETURN1();
#undef FILE_HDR_OFFSET
})

(define-glue (image-read-header path)
{
extern FILE *os_fopen( const char *f_path, const char *mode );
FILE *f = os_fopen( string_text(path), "r+b" );
long n;
#define FILE_HDR_OFFSET (128)

    fseek( f, 0L, SEEK_SET );
    REG0 = bvec_alloc( FILE_HDR_OFFSET+1, string_class );
    n = fread( PTR_TO_DATAPTR(REG0), 1, FILE_HDR_OFFSET, f );
    fclose(f);
    REG2 = int2fx(n);
    RETURN(2);
#undef FILE_HDR_OFFSET
})
