/*
 * Platform independent wrapper
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#ifdef _MSC_VER
/* MSVC6 */
#include <io.h>
#else
#include <unistd.h>
#endif

extern int open_for_read(const char* pathname);
extern int getErrno();
