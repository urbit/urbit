#include "all.h"
#include "vere/vere.h"

/* _fix_std_handle(): replaces given stdio fd with a handle to null.
*/
static void
_fix_std_handle(c3_i fid_i, DWORD typ_u)
{
  dup2(open(c3_dev_null, O_RDWR, 0), fid_i); // this closes the system-provided handle
  SetStdHandle(typ_u, (HANDLE)_get_osfhandle(fid_i));
}

/* u3_daemon_init(): platform-specific daemon mode initialization.
*/
void
u3_daemon_init()
{
  // detect stdout/stderr redirection
  DWORD dum_u;
  BOOL do1_u = GetConsoleMode(GetStdHandle(STD_OUTPUT_HANDLE), &dum_u);
  BOOL do2_u = GetConsoleMode(GetStdHandle(STD_ERROR_HANDLE), &dum_u);
  FreeConsole();
  if ( do1_u ) _fix_std_handle(1, STD_OUTPUT_HANDLE);
  if ( do2_u ) _fix_std_handle(2, STD_ERROR_HANDLE);
}
