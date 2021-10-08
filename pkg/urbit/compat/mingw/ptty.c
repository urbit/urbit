/* compat/mingw/ptty.c
**
*/
#include "all.h"
#include "vere/vere.h"
#include <winternl.h>

/*  _ptty_get_type(): detects tty type.
*/
static DWORD
_ptty_get_type(int fd)
{
  HANDLE h = (HANDLE)_get_osfhandle(fd);
  if (h == INVALID_HANDLE_VALUE)
    return FILE_TYPE_UNKNOWN;

  DWORD t = GetFileType(h);
  if (t != FILE_TYPE_PIPE)
    return t ;

  // https://github.com/fusesource/jansi-native/commit/461068c67a38647d2890e96250636fc0117074f5
  ULONG result;
  BYTE buffer[1024];
  POBJECT_NAME_INFORMATION nameinfo = (POBJECT_NAME_INFORMATION) buffer;
  PWSTR name;

  /* get pipe name */
  if (!NT_SUCCESS(NtQueryObject(h, ObjectNameInformation, buffer, sizeof(buffer) - sizeof(WCHAR), &result)))
    return FILE_TYPE_UNKNOWN;

  name = nameinfo->Name.Buffer;
  name[nameinfo->Name.Length] = 0;

  // check for popular terminal emulators
  // that use named pipes to communicate with subprocesses
  if (wcsstr(name, L"\\ConEmu") ||
     (wcsstr(name, L"msys-")    || wcsstr(name, L"cygwin-")) && wcsstr(name, L"-pty"))
    return FILE_TYPE_PIPE;

  return FILE_TYPE_UNKNOWN;
}

/*  _ttyf_nop(): stub function.
*/
static c3_o
_ttyf_nop(u3_utty* uty_u)
{
  return c3y;
}

/*  _ttyf_start_raw_input(): ends raw input on the tty.
*/
static c3_o
_ttyf_set_normal(u3_utty* uty_u)
{
  c3_i e;
  if ( 0 != (e = uv_tty_set_mode(&uty_u->pin_u.tty_u, UV_TTY_MODE_NORMAL)) ) {
    fprintf(stderr, "uv_tty_set_mode(UV_TTY_MODE_NORMAL) -> %d\r\n", e);
    return c3n;
  }
  return c3y;
}

/*  _ttyf_start_raw_input(): sets the tty to raw input.
*/
static c3_o
_ttyf_set_raw(u3_utty* uty_u)
{
  c3_i e;
  if ( 0 != (e = uv_tty_set_mode(&uty_u->pin_u.tty_u, UV_TTY_MODE_RAW)) ) {
    fprintf(stderr, "uv_tty_set_mode(UV_TTY_MODE_RAW) -> %d\r\n", e);
    return c3n;
  }
  return c3y;
}

/*  _ttyf_get_winsize(): gets the tty window size.
*/
static c3_o
_ttyf_get_winsize(u3_utty* uty_u, c3_l* col_l, c3_l* row_l)
{
  c3_i col_i, row_i;
  if ( 0 != uv_tty_get_winsize(&uty_u->pop_u.tty_u, &col_i, &row_i) ) {
    return c3n;
  }

  *col_l = col_i;
  *row_l = row_i;
  return c3y;
}

/*  _ttyf_get_winsize(): gets the tty window size.
*/
static c3_o
_ttyf_nop_winsize(u3_utty* uty_u, c3_l* col_l, c3_l* row_l)
{
  return c3n;
}

/* u3_ptty_init(): initialize platform-specific tty.
*/
u3_utty*
u3_ptty_init(uv_loop_t* lup_u, const c3_c** err_c)
{
  DWORD pip_l = _ptty_get_type(0);
  DWORD pop_l = _ptty_get_type(1);
  if ( pip_l == FILE_TYPE_UNKNOWN || pop_l == FILE_TYPE_UNKNOWN) {
    *err_c = "not a tty";
    return NULL;
  }

  if ( pip_l != pop_l ) {
    *err_c = "partly redirected";
    return NULL;
  }

  c3_i e;
  u3_utty* uty_u = c3_calloc(sizeof(u3_utty));
  if ( pip_l == FILE_TYPE_CHAR ) {
    if ( 0 == (e = uv_tty_init(lup_u, &uty_u->pin_u.tty_u, 0, 0)) &&
         0 == (e = uv_tty_init(lup_u, &uty_u->pop_u.tty_u, 1, 0)) )
    {
      SetConsoleOutputCP(CP_UTF8);
      uty_u->sta_f = _ttyf_set_raw;
      uty_u->sto_f = _ttyf_set_normal;
      uty_u->wsz_f = _ttyf_get_winsize;
    }
  } else {
    if ( 0 == (e = uv_pipe_init(lup_u, &uty_u->pin_u.pip_u, 0)) &&
         0 == (e = uv_pipe_init(lup_u, &uty_u->pop_u.pip_u, 0)) &&
         0 == (e = uv_pipe_open(&uty_u->pin_u.pip_u, 0)) &&
         0 == (e = uv_pipe_open(&uty_u->pop_u.pip_u, 1)) )
    {
      fprintf(stderr, "vere: running interactive in a terminal emulator is experimental\r\n"
                      "      use -t to disable interactivity or use native Windows console\r\n") ;
      uty_u->sta_f = _ttyf_nop;
      uty_u->sto_f = _ttyf_nop;
      uty_u->wsz_f = _ttyf_nop_winsize;
    }
  }

  if ( e ) {
    *err_c = uv_strerror(e);
    c3_free(uty_u);
    return NULL;
  }

  uty_u->fid_i = 1;
  uty_u->hij_f = _ttyf_nop;
  uty_u->loj_f = _ttyf_nop;
  return uty_u;
}
