/// @file prim.c

#include "c/prim.h"

#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>

//==============================================================================
// Functions
//==============================================================================

c3_t
c3_prim_get(const c3_path* const path_u,
            const c3_prim_type   type_e,
            void*                data_v)
{
  c3_t suc_t = 0;

  if ( !path_u || type_e >= c3_prim_last || !data_v ) {
    goto end;
  }

  c3_i fd_i = open(c3_path_str(path_u), O_RDONLY);
  if ( fd_i == -1 ) {
    fprintf(stderr,
            "prim: failed to open %s as read-only: %s\r\n",
            c3_path_str(path_u),
            strerror(errno));
    goto end;
  }

  struct stat stat_u;
  if ( fstat(fd_i, &stat_u) == -1 ) {
    fprintf(stderr,
            "prim: failed to stat %s: %s\r\n",
            c3_path_str(path_u),
            strerror(errno));
    goto close_file;
  }

  c3_c* const buf_c = c3_calloc(stat_u.st_size + 1);
  { // Read file contents as a null-terminated string.
    c3_c*  ptr_c             = buf_c;
    size_t bytes_remaining_i = stat_u.st_size;
    do {
      ssize_t bytes_read_i = read(fd_i, ptr_c, bytes_remaining_i);
      if ( bytes_read_i == -1 ) {
        // Attempt to read again if we were interrupted by a signal.
        if ( errno == EINTR ) {
          continue;
        }
        fprintf(stderr,
                "prim: failed to read %lu bytes from %s: %s\r\n",
                bytes_remaining_i,
                c3_path_str(path_u),
                strerror(errno));
        goto free_buf;
      }
      ptr_c += bytes_read_i;
      bytes_remaining_i -= bytes_read_i;
    } while ( bytes_remaining_i > 0 );
  }

#define buf_to_num(type, conversion_specifier)                                 \
  do {                                                                         \
    if ( 1 != sscanf(buf_c, "%" conversion_specifier, (type*)data_v) ) {       \
      goto free_buf;                                                           \
    }                                                                          \
  } while ( 0 )

  switch ( type_e ) {
    case c3_prim_str:
      *(c3_c**)data_v = buf_c;
      break;
    case c3_prim_uint8:
      buf_to_num(c3_y, SCNu8);
      break;
    case c3_prim_uint16:
      buf_to_num(c3_s, SCNu16);
      break;
    case c3_prim_uint32:
      buf_to_num(c3_w, SCNu32);
      break;
    case c3_prim_uint64:
      buf_to_num(c3_d, SCNu64);
      break;
    case c3_prim_int8:
      buf_to_num(c3_ys, SCNi8);
      break;
    case c3_prim_int16:
      buf_to_num(c3_ss, SCNi16);
      break;
    case c3_prim_int32:
      buf_to_num(c3_ws, SCNi32);
      break;
    case c3_prim_int64:
      buf_to_num(c3_ds, SCNi64);
      break;
    // This is dead code, but it's needed to appease macOS.
    case c3_prim_last:
      break;
  }

#undef buf_to_num

  suc_t = 1;

free_buf:
  if ( type_e != c3_prim_str ) {
    c3_free(buf_c);
  }
close_file:
  close(fd_i);
end:
  return suc_t;
}

c3_t
c3_prim_put(const c3_path* const path_u,
            const c3_prim_type   type_e,
            void* const          data_v)
{
  c3_t suc_t = 0;

  if ( !path_u || type_e >= c3_prim_last || !data_v ) {
    goto end;
  }

  c3_i fd_i = open(c3_path_str(path_u), O_CREAT | O_TRUNC | O_WRONLY, 0644);
  if ( fd_i == -1 ) {
    fprintf(stderr,
            "prim: failed to open %s as write-only: %s\r\n",
            c3_path_str(path_u),
            strerror(errno));
    goto end;
  }

  c3_c* buf_c = NULL;
#define num_to_buf(type, conversion_specifier)                                 \
  do {                                                                         \
    if ( -1 == asprintf(&buf_c, "%" conversion_specifier, *(type*)data_v) ) {  \
      goto close_file;                                                         \
    }                                                                          \
  } while ( 0 )

  switch ( type_e ) {
    case c3_prim_str:
      buf_c = *(c3_c**)data_v;
      break;
    case c3_prim_uint8:
      num_to_buf(c3_y, SCNu8);
      break;
    case c3_prim_uint16:
      num_to_buf(c3_s, SCNu16);
      break;
    case c3_prim_uint32:
      num_to_buf(c3_w, SCNu32);
      break;
    case c3_prim_uint64:
      num_to_buf(c3_d, SCNu64);
      break;
    case c3_prim_int8:
      num_to_buf(c3_ys, SCNi8);
      break;
    case c3_prim_int16:
      num_to_buf(c3_ss, SCNi16);
      break;
    case c3_prim_int32:
      num_to_buf(c3_ws, SCNi32);
      break;
    case c3_prim_int64:
      num_to_buf(c3_ds, SCNi64);
      break;
    // This is dead code, but it's needed to appease macOS.
    case c3_prim_last:
      break;
  }
#undef num_to_buf

  { // Write null-terminated string to file.
    c3_c*  ptr_c             = buf_c;
    size_t bytes_remaining_i = strlen(buf_c);
    do {
      ssize_t bytes_written_i = write(fd_i, ptr_c, bytes_remaining_i);
      if ( bytes_written_i == -1 ) {
        // Attempt to write again if we were interrupted by a signal.
        if ( errno == EINTR ) {
          continue;
        }
        fprintf(stderr,
                "prim: failed to read %lu bytes from %s: %s\r\n",
                bytes_remaining_i,
                c3_path_str(path_u),
                strerror(errno));
        goto free_buf;
      }
      ptr_c += bytes_written_i;
      bytes_remaining_i -= bytes_written_i;
    } while ( bytes_remaining_i > 0 );
  }

  suc_t = 1;

free_buf:
  if ( type_e != c3_prim_str ) {
    c3_free(buf_c);
  }
close_file:
  close(fd_i);
end:
  return suc_t;
}
