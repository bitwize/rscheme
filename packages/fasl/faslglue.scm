
(define-safe-glue (save-fasl-image (path <raw-string>)
				   comment
				   (roots <vector>))
{
  void fasl_save_vec( const char *path, obj vec, const char *comment );

  fasl_save_vec( path,
		 roots, 
		 (STRING_P(comment) ? string_text(comment) : NULL) );
  RETURN0();
})
