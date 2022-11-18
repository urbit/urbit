/// @file defs.c

#include "c/defs.h"

/* c3_pread(): full positioned read(), up to eof, retrying errors.
*/
ssize_t
c3_pread(c3_i fid_i, void* buf_v, size_t len_i, off_t off_i)
{
  c3_w    max_w = 128;
  c3_w    try_w = 0;
  size_t  rem_i = len_i;
  ssize_t ret_i;

  do {
    if (  (0 > (ret_i = pread(fid_i, buf_v, rem_i, off_i)))
       && (  (++try_w == max_w)
          || (  (EINTR       != errno)
             && (EAGAIN      != errno)
             && (EWOULDBLOCK != errno) )))
    {
      return -1;
    }
    else if ( 0 == ret_i ) {
      break;
    }
    else {
      buf_v  = (void*)((c3_c*)buf_v + ret_i);
      rem_i -= ret_i;
      off_i += ret_i;
    }

  }
  while ( rem_i );

  return len_i - rem_i;
}

/* c3_pwrite(): full positioned write(), retrying errors.
*/
ssize_t
c3_pwrite(c3_i fid_i, const void* buf_v, size_t len_i, off_t off_i)
{
  c3_w    max_w = 128;
  c3_w    try_w = 0;
  size_t  rem_i = len_i;
  ssize_t ret_i;

  do {
    if (  (0 > (ret_i = pwrite(fid_i, buf_v, rem_i, off_i)))
       && (  (++try_w == max_w)
          || (  (EINTR       != errno)
             && (EAGAIN      != errno)
             && (EWOULDBLOCK != errno) )))
    {
      return -1;
    }
    else {
      buf_v  = (void*)((c3_c*)buf_v + ret_i);
      rem_i -= ret_i;
      off_i += ret_i;
    }
  }
  while ( rem_i );

  return len_i;
}

/* c3_write(): full write(), retrying errors.
*/
ssize_t
c3_write(c3_i fid_i, const void* buf_v, size_t len_i)
{
  c3_w    max_w = 128;
  c3_w    try_w = 0;
  size_t  rem_i = len_i;
  ssize_t ret_i;

  do {
    if (  (0 > (ret_i = write(fid_i, buf_v, rem_i)))
       && (  (++try_w == max_w)
          || (  (EINTR       != errno)
             && (EAGAIN      != errno)
             && (EWOULDBLOCK != errno) )))
    {
      return -1;
    }
    else {
      buf_v  = (void*)((c3_c*)buf_v + ret_i);
      rem_i -= ret_i;
    }
  }
  while ( rem_i );

  return len_i;
}
