
(define (make-fd-open-mode main-mode . options)
  (bitwise-or 
   (case main-mode
     ((read) 0)
     ((write) 1)
     ((read-write) 2)
     (else (error "make-fd-open-mode: ~s is not a valid main mode" main-mode)))
   (bitwise-or
    (if (memq 'append options) 4 0)
    (bitwise-or
     (if (memq 'create options) 8 0)
     (bitwise-or
      (if (memq 'exclusive options) 16 0)
      (if (memq 'truncate options) 32 0))))))

(define-syscall-glue (fd-stat (fd <raw-int>))
  literals: ((& <stat-buf>))
{
  obj x;
  struct stat *sb;

  x = alloc( sizeof(struct stat), TLREF(0) );
  sb = (struct stat *)PTR_TO_DATAPTR(x);

  if (fstat( fd, sb ) < 0)
    REG0 = FALSE_OBJ;
  else
    REG0 = x;
  RETURN1();
})

(define-syscall-glue (rename (old_name <raw-string>) (new_name <raw-string>))
{
  if (rename( old_name, new_name ) < 0)
     os_error( "rename", 2, raw_old_name, raw_new_name );
  RETURN0();
})


(define-syscall-glue (link (existing_path <raw-string>) 
			   (new_path <raw-string>))
{
  if (link(existing_path, new_path) < 0)
     os_error( "link", 2, raw_existing_path, raw_new_path );
  RETURN0();
})

(define-syscall-glue (symlink (existing_path <raw-string>)
			      (new_path <raw-string>))
{
#if HAVE_SYMLINK
  if (symlink(existing_path, new_path) < 0)
     os_error( "symlink", 2, raw_existing_path, raw_new_path );
#else
  errno = ENOSYS;
  os_error( "symlink", 2, raw_existing_path, raw_new_path );
#endif
  RETURN0();
})

(define-syscall-glue (unlink (path <raw-string>))
{
  if (unlink( path ) < 0)
     os_error( "unlink", 1, raw_path );
  RETURN0();
})

(define-syscall-glue (rmdir (path <raw-string>))
{
  if (rmdir( path ) < 0)
     os_error( "rmdir", 1, raw_path );
  RETURN0();
})

;; also useful for decoding `stat-mode' 

(define-class <invalid-mode-spec> (<condition>)
  spec)

(define-method display-object ((self <invalid-mode-spec>) port)
  (format port "`~s' is not a valid mode specifier\n" (spec self))
  (format port ">> (must be one of the symbols mode/{user,group,anyone}-{read,write,execute}\n")
  (format port ">>  or mode/set-{user,group}-id\n"))

(define-syscall-glue (mode-list->bits lst)
   literals: ('mode/user-read
	      'mode/user-write
	      'mode/user-execute
	      'mode/group-read
	      'mode/group-write
	      'mode/group-execute
	      'mode/anyone-read
	      'mode/anyone-write
	      'mode/anyone-execute
              'mode/set-user-id
	      'mode/set-group-id
	      (& <invalid-mode-spec>))
{
  mode_t m = 0;
  unsigned j;
  obj i, k;
  static mode_t mbits[11] = { S_IRUSR, S_IWUSR, S_IXUSR,
			      S_IRGRP, S_IWGRP, S_IXGRP,
			      S_IROTH, S_IWOTH, S_IXOTH,
			      S_ISUID, S_ISGID };

  for (i=lst; PAIR_P(i); i=pair_cdr(i))
   {
     k = pair_car(i);
     for (j=0; j<11; j++)
       {
	 if (EQ(k,LITERAL(j)))
	   {
	     m |= mbits[j];
	     goto ok;
	   }
       }
     raise_error( make2( TLREF(11), NIL_OBJ, k ) );
   ok: /* onward */;
   }
  REG0 = int2fx(m);
  RETURN1();
})

(define-syscall-glue (chmod (path <raw-string>) (mode_bits <raw-int>))
{
   if (chmod( path, mode_bits ) < 0)
      os_error( "chmod", 2, raw_path, raw_mode_bits );
   RETURN0();
})

(define-syscall-glue (chown (path <raw-string>) 
			    (uid <raw-int>) 
			    (gid <raw-int>))
{
   if (chown( path, uid, gid ) < 0)
      os_error( "chown", 3, raw_path, raw_uid, raw_gid );
   RETURN0();
})
