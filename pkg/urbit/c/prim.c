/// @file

#include "c/prim.h"

#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>

//==============================================================================
// Static functions
//==============================================================================

#define define_put_num(type, type_suffix, format_specifier)                    \
  c3_t c3_prim_put_##type(const c3_path* const path_u,                         \
                          c3_##type_suffix     num_##type_suffix)              \
  {                                                                            \
    c3_t  suc_t = 0;                                                           \
    c3_c* str_c;                                                               \
    if ( -1 == asprintf(&str_c, "%" format_specifier, num_##type_suffix) ) {   \
      goto end;                                                                \
    }                                                                          \
                                                                               \
    suc_t = _write(path_u, str_c);                                             \
free_str:                                                                      \
    c3_free(str_c);                                                            \
end:                                                                           \
    return suc_t;                                                              \
  }

#define define_get_num(type, type_suffix, format_specifier)                    \
  c3_t c3_prim_get_##type(const c3_path* const    path_u,                      \
                          c3_##type_suffix* const num_##type_suffix)           \
  {                                                                            \
    c3_t suc_t = 0;                                                            \
                                                                               \
    c3_c* str_c = _read(path_u);                                         \
    if ( !str_c ) {                                                            \
      goto end;                                                                \
    }                                                                          \
    if ( 1 != sscanf(str_c, "%" format_specifier, num_##type_suffix) ) {       \
      goto free_str;                                                           \
    }                                                                          \
    suc_t = 1;                                                                 \
                                                                               \
free_str:                                                                      \
    c3_free(str_c);                                                            \
end:                                                                           \
    return suc_t;                                                              \
  }

//==============================================================================
// Static functions
//==============================================================================

/// Read a null-terminated string from a file.
///
/// @param[in] path_u  Path to backing file.
///
/// @return NULL    Error occurred.
/// @return string  Otherwise.
c3_c*
_read(const c3_path* const path_u);

/// Write null-terminated string to file.
///
/// @param[in] path_u  Path to backing file.
/// @param[in] str_c   String.
///
/// @return 1  String successfully written to backing file.
/// @return 0  Otherwise.
c3_t
_write(const c3_path* const path_u, const c3_c* const str_c);

c3_c*
_read(const c3_path* const path_u)
{
  c3_c* str_c = NULL;

  if ( !path_u ) {
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

  str_c                 = c3_malloc(stat_u.st_size + 1);
  str_c[stat_u.st_size] = '\0';

  c3_c*  ptr_c             = str_c;
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
      c3_free(str_c);
      str_c = NULL;
      goto close_file;
    }
    ptr_c += bytes_read_i;
    bytes_remaining_i -= bytes_read_i;
  } while ( bytes_remaining_i > 0 );

close_file:
  close(fd_i);
end:
  return str_c;
}

c3_t
_write(const c3_path* const path_u, const c3_c* const str_c)
{
  c3_t suc_t = 0;

  if ( !path_u || !str_c ) {
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

  const c3_c* ptr_c             = str_c;
  size_t      bytes_remaining_i = strlen(str_c);
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
      goto close_file;
    }
    ptr_c += bytes_written_i;
    bytes_remaining_i -= bytes_written_i;
  } while ( bytes_remaining_i > 0 );

  if ( -1 == c3_sync(fd_i) ) {
    fprintf(stderr,
            "prim: failed to fsync %s: %s\r\n",
            c3_path_str(path_u),
            strerror(errno));
    goto close_file;
  }

  suc_t = 1;

close_file:
  close(fd_i);
end:
  return suc_t;
}

//==============================================================================
// Functions
//==============================================================================

c3_t
c3_prim_put_str(const c3_path* const path_u, const c3_c* const str_c)
{
  return _write(path_u, str_c);
}

define_put_num(uint8, y, SCNu8);
define_put_num(uint16, s, SCNu16);
define_put_num(uint32, w, SCNu32);
define_put_num(uint64, d, SCNu64);

define_put_num(int8, ys, SCNi8);
define_put_num(int16, ss, SCNi16);
define_put_num(int32, ws, SCNi32);
define_put_num(int64, ds, SCNi64);

c3_t
c3_prim_get_str(const c3_path* const path_u, c3_c** str_c)
{
  if ( !str_c ) {
    return 0;
  }
  c3_c* res_c = _read(path_u);
  if ( !res_c ) {
    return 0;
  }
  *str_c = res_c;
  return 1;
}

define_get_num(uint8, y, SCNu8);
define_get_num(uint16, s, SCNu16);
define_get_num(uint32, w, SCNu32);
define_get_num(uint64, d, SCNu64);

define_get_num(int8, ys, SCNi8);
define_get_num(int16, ss, SCNi16);
define_get_num(int32, ws, SCNi32);
define_get_num(int64, ds, SCNi64);

#undef define_put_num
#undef define_get_num
