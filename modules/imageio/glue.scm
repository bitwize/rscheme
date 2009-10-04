#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/imageio/glue.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.4
 | File mod date:    1997-11-29 23:10:30
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  imageio
 |
 `------------------------------------------------------------------------|#

#| the structure of a buffer chain is a list
   of <bvec>'s.  The first word (32 bits)
   of each is the number of bytes used within that
   buffer
|#

(define-safe-glue (compact-buffers (dest_str <raw-string>)
				   (offset <raw-int>)
				   (source <pair>))
{
  UINT_32 n = offset;
  UINT_8 *buf, *dest;
  obj s = source;
  
  dest = (UINT_8 *)dest_str + offset;
  for (s = source; PAIR_P(s); s = pair_cdr(s))
    {
      buf = (UINT_8 *)PTR_TO_DATAPTR(pair_car(s));
      n = *(UINT_32 *)buf;
      memcpy( dest, buf + sizeof(UINT_32), n );
      dest += n;
    }
  REG0 = raw_dest_str;
  REG1 = int2fx( dest - (UINT_8 *)dest_str );
  RETURN(2);
})

;; the input to compress is the same as the source for compact-buffers

(define-safe-glue (compress (source <pair>))
{
   REG0 = rs_compress( source );
   RETURN1();
})

(define-safe-glue (uncompress (source <string>))
{
   REG0 = rs_extract( (UINT_8 *)string_text(source),
		      string_length(source) );
   RETURN1();
})

(define-safe-glue (derender-unmarshalling (source <string>)
					  (refs <vector>))
{
   REG0 = rs_load_image( (UINT_8 *)string_text(source), 
			 string_length(source),
			 refs );
   RETURN1();
})

#|
    render-marshalling

    Convert a marshalled structure to linear-byte form

        sections => a vector of the "sections" of the image.
		    a section is a collection of objects with
		    the same i/o technique.
		    there are three defined sections:
			[0] REF
				NOTE: the REF section is not written
					out by this function
			[1] GVEC
			[2] BVEC
	marshall-table => an <object-table> mapping items to be
			written (elements of the non-REF sections)
			to their ids
	fix-slot-table => an <object-table> mapping from some objects
			  to an ordered list of pairs whose car
			  is the slot to be rewritten, and whose
			  cdr is the new value for the slot
	root => the root of the structure
|#

(define-safe-glue (render-marshalling (sections <vector>)
		   (marshall_table <object-table>)
				      (fix_slot_table <object-table>)
				      root)
{
  REG0 = rs_save_image( sections,
		        marshall_table,
		        fix_slot_table,
		        root );
  RETURN1();
})


(define (missed-classes result brk misslist)
  (error "parse-refs: class dictionary missing: ~s" misslist))
		
(define-safe-glue (parse-refs (src <raw-string>)
			      (offset <raw-int>)
			      (class_table <symbol-table>) 
			      intern_q
			      (class_dict <symbol-dict>)
			      (symbol_dict <symbol-dict>))
 literals: ((& <fn-descr-anchor>)
	    (& <code-ptr-anchor>)
	    (& missed-classes))
{
  obj result, misses;
  UINT_32 n;
  UINT_8 *s;

  /* note:  herein I rely on gvec_write_fresh being a nop
   * (by storing directly into dst) 
   */
   misses = NIL_OBJ;
  s = ((UINT_8 *)src) + offset;
  result = parse_refs( class_table, 
		   &s, 
		   string_length(raw_src) - offset,
		   truish(intern_q) ? YES : NO, 
		   TLREF(0), TLREF(1), 
		   gvec_read( class_dict, SLOT(0) ),
		   gvec_read( symbol_dict, SLOT(0) ),
		   &misses );
  REG0 = result;
  REG1 = int2fx( s - (UINT_8*)src );
  if (EQ(misses,NIL_OBJ))
    RETURN(2);
  else
  {
    REG2 = misses;
    APPLY( 3, TLREF(2) );
  }
})
