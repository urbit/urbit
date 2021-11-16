/* noun/log.h
**
*/

/* u3l_log(): logs to stderr or redirects to configured function.
*/
  void
  u3l_log(const char* format, ...)
    __attribute__ ((format (printf, 1, 2)));

/* u3l_punt(): condtionally logs a named punt
 *             (e.g. "mint-punt" for the `name` "mint")
 *             when `pro` is u3_none, and returns pro.
 *             For use when a jet driver declines to handle
 *             a core, when the user should be somehow notified
 *             (e.g. in a cryptographic jet).
 */
  u3_weak
  u3l_punt(const char* name, u3_weak pro);
