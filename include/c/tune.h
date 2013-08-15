/* include/c/tune.h
**
** This file is in the public domain.
*/
  /** Tuning ifdefs.  Comment out to disable.
  **/
    /** Profiling.
    **/
      /* Profile at all.
      */
#       define U2_PROFILE

      /* Measure memory usage.
      */
#       define U2_PROFILE_MEMORY

      /* Measure execution time.
      */
#       define U2_PROFILE_SPEED

      /* Describe execution patterns.
      */
#       define U2_PROFILE_SHAPE

