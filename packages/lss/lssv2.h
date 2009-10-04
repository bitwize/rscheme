#ifndef _H_LSSV2
#define _H_LSSV2

#include <time.h>
#include <rscheme/platform.h>
#include <sys/types.h>

#define LSS_FMT_VERSION (2)

#define MAX_LSS_CLIENT_SIZE  (256)

typedef struct LSS_CommitInfo {

    UINT_32     lss_magic;
    UINT_32     lss_fmt_version;
    time_t	create_time;
    time_t	commit_time;
    UINT_32     commit_version;
    UINT_32     index_count;
    UINT_32     index_capacity;
    off_t       index_offset;
    off_t       prev_commit_at;

    UINT_32     client_info_len;
    char        spare[8];
} commit_info_t;

struct LSS_Record {
    UINT_32	number;
    off_t	offset;
    UINT_32	length;
};

struct LogStructuredStore;

typedef struct LogStructuredStore {
  struct LSS com;
    int			fd;
    int                 lock_held;
    UINT_32		index_capacity;
    UINT_32		index_count;
    struct LSS_Record   *index;
    off_t               spare_commit_at;
    commit_info_t	last_commit;
    int                 num_accesses;
    void               *rec0;
    UINT_32             rec0_len;
} LSS_V2;

typedef struct LSS_Access {
    void	*addr;
    UINT_32	bytes;
    UINT_32	record_num;
} access_t;

struct LSSAccess {
  size_t	       bytes;        /* uncompressed size */
  void	              *addr;
  struct LSS_Record   *rec;
};
#endif /* _H_LSSV2 */
