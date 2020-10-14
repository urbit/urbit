#ifndef AES_SIV_CONFIG_H_
#define AES_SIV_CONFIG_H_

/* Enable ct_poison() and ct_unpoison() hooks for testing with
   ctgrind. */
/* #undef ENABLE_CTGRIND */

/* Enable this to get test coverage for the portable versions of
   putword() and getword() when you don't happen to have a PDP-11
   in your test farm.
*/
/* #undef ENABLE_DEBUG_WEIRD_ENDIAN */

/* Enable this to get test coverage for the while loop in do_encrypt()
   without having to have a multi-gigabyte test case that'll take
   forever for Valgrind to crunch through
*/
/* #undef ENABLE_DEBUG_TINY_CHUNK_SIZE */
#endif
