#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

int open_for_read(const char* pathname, int flags)
{
  return open(pathname,O_WRONLY | O_CREAT | O_TRUNC, S_IRWXU);
}
