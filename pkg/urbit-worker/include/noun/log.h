/* noun/log.h
**
*/

/* u3_log(): logs to stderr or redirects to configured function.
*/
  void
  u3l_log(const char* format, ...)
    __attribute__ ((format (printf, 1, 2)));
