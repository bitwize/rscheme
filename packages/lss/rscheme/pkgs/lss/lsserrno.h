#ifndef _H_LSSERRNO
#define _H_LSSERRNO

/* `fmt' is a short character string defining the additional
   data provided in the following arguments.

   'i' (int)
   'l' (long)
   's' (char *)

   the length of the fmt string is the number of arguments
   to follow.
*/

typedef void lss_error_handler_t( LSS *lss,
				  void *client_info,
				  int code,
				  char *fmt, va_list va );

char *strlsserror( int code );

#define LSSERR_MIN                   (5000)

#define LSSERR_N(n)                  ((n)+LSSERR_MIN)

#define LSSERR_NOT_LSS               LSSERR_N(1)  /* bad magic */
#define LSSERR_LOCKED                LSSERR_N(2)  /* locked already */
#define LSSERR_BAD_VER               LSSERR_N(3)  /* incompatible version */

#define LSSERR_WRITE_COM_FAILED      LSSERR_N(4)
#define LSSERR_WRITE_FAILED          LSSERR_N(5)
#define LSSERR_UNRELEASED_ACCESS     LSSERR_N(6)
#define LSSERR_SHORT_READ            LSSERR_N(7)
#define LSSERR_SHORT_WRITE           LSSERR_N(8)
#define LSSERR_DISK_FULL             LSSERR_N(9)
#define LSSERR_TOO_MANY_ALGORITHMS   LSSERR_N(10)

#define LSSERR_TOO_BIG               LSSERR_N(11)  /* commit info is too big */
#define LSSERR_SYS_ERR               LSSERR_N(12)
#define LSSERR_DEBUG_OPEN_FAILED     LSSERR_N(13) /* couldn't satisfy LSS_KEEP_LOG */

#define LSSERR_NO_RECORD             LSSERR_N(14)  /* record doesn't exist */
#define LSSERR_INVALID_ROLLBACK      LSSERR_N(15) /* attempt to open old version in update mode */
#define LSSERR_READ_ONLY             LSSERR_N(16)
#define LSSERR_ZIP_ALGORITHM_NOT_DEF LSSERR_N(17)
#define LSSERR_INVALID_ZIP_ALGORITHM LSSERR_N(18) /* somehow not compat (eg, v2) */
#define LSSERR_WRONG_RECORD_SIZE     LSSERR_N(19) /* call expected different size */
#define LSSERR_NOT_IMPL              LSSERR_N(20) /* not implemented */

#define LSSERR_NOT_WHERE_EXPECTED    LSSERR_N(21) /* not where expected */
#define LSSERR_BAD_TYPE              LSSERR_N(22) /* bad record type */
#define LSSERR_VOLSER_MISMATCH       LSSERR_N(23) /* volume serial mismatch */
#define LSSERR_BAD_VOLHDR            LSSERR_N(24) /* bad volume header */
#define LSSERR_MAX                   LSSERR_N(24)

#endif /* _H_LSSERNO */
