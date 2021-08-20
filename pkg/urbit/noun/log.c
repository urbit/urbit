/* noun/log.c
**
*/
#include "all.h"

#include <stdarg.h>

void
u3l_log(const char* format, ...)
{
  va_list myargs;
  va_start(myargs, format);

  if (u3C.stderr_log_f) {
    //  the user set their own logging function. render the line and redirect
    //  to them.
    //
    char msg[4096];
    vsnprintf(msg, 4096, format, myargs);
    u3C.stderr_log_f(msg);
  } else  {
    //  this process did not set a logging function, fallback to stderr
    //
    vfprintf(stderr, format, myargs);
    fprintf(stderr, "\r\n");
    fflush(stderr);
  }

  va_end(myargs);
}

u3_weak
u3l_punt(const char* name, u3_weak pro)
{
  if ( u3_none == pro ) {
    u3l_log("%s-punt\r\n", name);
  }
  return pro;
}
