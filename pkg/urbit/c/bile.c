//! @file bile.c

#include "c/bile.h"

#include <errno.h>
#include <fcntl.h>
#include <stddef.h>
#include <sys/stat.h>

//==============================================================================
// Macros
//==============================================================================

//! Error-handling wrapper for system calls that return >=0 on success
//! and <0 on failure.
//!
//! It's unclear whether `strerror(errno)` is a portable solution for reporting
//! error messages.
//!
//! @param[in] system_call     System call to execute.
//! @param[in] failure_action  Statement to execute after logging failure.
#define try_syscall(system_call, failure_action, ...)                          \
  do {                                                                         \
    if ( 0 > (system_call) ) {                                                 \
      fprintf(stderr, "syscall: " __VA_ARGS__);                                \
      c3_c* msg_c = strerror(errno);                                           \
      if ( !msg_c ) {                                                          \
        msg_c = "Unknown error";                                               \
      };                                                                       \
      fprintf(stderr,                                                          \
              " (%s) [%s:%s():%d]\r\n",                                        \
              msg_c,                                                           \
              __FILE__,                                                        \
              __func__,                                                        \
              __LINE__);                                                       \
      failure_action;                                                          \
    }                                                                          \
  } while ( 0 )

//==============================================================================
// Functions
//==============================================================================

c3_bile*
c3_bile_open(const c3_path* const pax_u)
{
  if ( !pax_u ) {
    goto fail;
  }

  c3_bile* bil_u = c3_calloc(sizeof(*bil_u));

  bil_u->pax_u = c3_path_fp(pax_u);

  try_syscall(bil_u->fid_i = open(bil_u->pax_u->str_c, O_RDWR | O_CREAT, 0666),
              goto free_file,
              "failed to open %s",
              bil_u->pax_u->str_c);

  struct stat buf_u;
  try_syscall(fstat(bil_u->fid_i, &buf_u),
              goto close_file,
              "failed to get %s stats",
              bil_u->pax_u->str_c);

  bil_u->len_i = buf_u.st_size;
  goto succeed;

close_file:
  close(bil_u->fid_i);
free_file:
  c3_free(bil_u->pax_u);
  c3_free(bil_u);
fail:
  return NULL;

succeed:
  return bil_u;
}

//! @n (1) Seek to the end of the file to ensure append-only behavior.
//! @n (2) Attempt the write again if we were interrupted by a signal.
//! @n (3) Writing failed partway through, so clear clear the file of any data
//!        from the partial write.
c3_t
c3_bile_put_raw(c3_bile* const    bil_u,
                const void* const dat_v,
                const size_t      len_i)
{
  if ( !dat_v ) {
    goto fail;
  }

  if ( bil_u->off_i != bil_u->len_i ) { // (1)
    try_syscall(lseek(bil_u->fid_i, bil_u->len_i, SEEK_SET), goto fail);
    bil_u->off_i = bil_u->len_i;
  }

  const c3_y* dat_y = dat_v;
  ssize_t     lef_i = len_i;
  ssize_t     wri_i;
  while ( 0 < lef_i ) {
    if ( -1 == (wri_i = write(bil_u->fid_i, dat_y, lef_i)) ) {
      if ( EINTR == errno ) { // (2)
        continue;
      }
      else {
        goto abandon_write;
      }
    }
    else {
      dat_y += wri_i;
      lef_i -= wri_i;
    }
  }
  bil_u->len_i += len_i;
  bil_u->off_i = bil_u->len_i;
  goto succeed;

abandon_write: // (3)
  try_syscall(truncate(bil_u->pax_u->str_c, bil_u->len_i), goto fail);
  try_syscall(lseek(bil_u->fid_i, bil_u->len_i, SEEK_SET), goto fail);
  bil_u->off_i = bil_u->len_i;
fail:
  return 0;

succeed:
  return 1;
}

//! @n (1) If we're at the end of the file (or past it), seek back to the
//!        beginning.
//! @n (2) Attempt the read again if we were interrupted by a signal.
c3_t
c3_bile_get_raw(c3_bile* const bil_u, void* const dat_v, const size_t len_i)
{
  if ( !dat_v ) {
    goto fail;
  }

  if ( bil_u->len_i <= bil_u->off_i ) { // (1)
    try_syscall(lseek(bil_u->fid_i, 0, SEEK_SET), goto fail);
    bil_u->off_i = 0;
  }

  c3_y*   dat_y = dat_v;
  ssize_t lef_i = len_i;
  ssize_t rea_i;
  while ( 0 < lef_i ) {
    rea_i = read(bil_u->fid_i, dat_y, lef_i);
    if ( -1 == rea_i ) {
      if ( EINTR == errno ) { // (2)
        continue;
      }
      else {
        goto abandon_read;
      }
    }
    else if ( 0 == rea_i ) {
      break;
    }
    else {
      dat_y += rea_i;
      lef_i -= rea_i;
    }
  }
  bil_u->off_i += len_i;
  goto succeed;

abandon_read:
  try_syscall(lseek(bil_u->fid_i, bil_u->off_i, SEEK_SET), goto fail);
fail:
  return 0;

succeed:
  return 1;
}

void
c3_bile_close(c3_bile* const bil_u)
{
  if ( !bil_u ) {
    return;
  }

  close(bil_u->fid_i);
  c3_free(bil_u->pax_u);
}

#undef try_syscall
