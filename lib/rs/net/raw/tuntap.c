#include <string.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <linux/if.h>
#include <linux/if_tun.h>

int rs_tun_setup( int fd, char *dev_in, char *dev_out )
{
  struct ifreq ifr;
  int rc;

  memset( &ifr, 0, sizeof(ifr) );
  ifr.ifr_flags = IFF_TUN;
  if (dev_in) {
    strncpy( ifr.ifr_name, dev_in, IFNAMSIZ );
  }

  rc = ioctl( fd, TUNSETIFF, (void *)&ifr );
  if (rc < 0) {
    return -1;
  }
  if (dev_out) {
    strncpy( dev_out, ifr.ifr_name, IFNAMSIZ );
  }
  return 0;
}
