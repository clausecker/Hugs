#include "FileIO_aux.h"
#include <errno.h>

int open_for_read(const char* pathname)
{
  return open(pathname,O_WRONLY | O_CREAT | O_TRUNC, 
#ifdef S_IRWXU
	      S_IRWXU
#else
	      /* try doing it directly instead */
	      S_IREAD | S_IWRITE | S_IEXEC
#endif
	     );
}

int getErrno() { return errno; }
