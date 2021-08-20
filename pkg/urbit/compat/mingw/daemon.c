#include "all.h"
#include "vere/vere.h"

/* _dup_std_handle(): creates an inheritable duplicate of a standard handle.
*/
static BOOL
_dup_std_handle(HANDLE* new_u, DWORD typ_u)
{
  DWORD dum_u;
  HANDLE han_u = GetStdHandle(typ_u);
  BOOL con_u = GetConsoleMode(han_u, &dum_u);
  if ( con_u ) {
    han_u = (HANDLE)_get_osfhandle(open(c3_dev_null, O_RDWR, 0));
  }

  if ( !DuplicateHandle(GetCurrentProcess(), han_u, GetCurrentProcess(), new_u, 0, TRUE, DUPLICATE_SAME_ACCESS) ) {
    fprintf(stderr, "vere: DuplicateHandle(%d): %d\r\n", typ_u, GetLastError());
    exit(1);
  }

  return con_u;
}

/* _on_boot_completed_cb: invoked when the ship has finished booting.
*/
static void _on_boot_completed_cb() {
  HANDLE hin_u = GetStdHandle(STD_INPUT_HANDLE);
  SetEvent(hin_u);
  CloseHandle(hin_u);
}

/* u3_daemon_init(): platform-specific daemon mode initialization.
*/
void
u3_daemon_init()
{
  //  detect if this process is the child daemon process
  //
  if ( ResetEvent(GetStdHandle(STD_INPUT_HANDLE)) ) {
    u3_Host.bot_f = _on_boot_completed_cb;
    return;
  }

  STARTUPINFOW psi_u;
  ZeroMemory(&psi_u, sizeof(psi_u));
  psi_u.cb = sizeof(psi_u);
  psi_u.dwFlags = STARTF_USESTDHANDLES;

  //  duplicate standard output and error handles for the child process,
  //  replacing any raw console handles with handles to /dev/null
  //  print a warning if raw console output detected
  //
  //  On Windows, console handles become invalid once the console is
  //  detached. This will cause urbit terminal output to fail. libuv
  //  provides no way of changing the handle of an open uv_pipe_handle,
  //  and Windows has no equivalent of dup2() for handles, so I cannot
  //  substitute a /dev/null handle once the terminal is initialized.
  //  It is possible to create an anonymous pipe and have the child
  //  process take over its drain end after it signals that the ship
  //  has booted, but -d is intended for background operation anyway
  //  and does not seem to warrant the added complexity.
  //
  if ( _dup_std_handle(&psi_u.hStdOutput, STD_OUTPUT_HANDLE) |
       _dup_std_handle(&psi_u.hStdError,  STD_ERROR_HANDLE) )
  {
    fprintf(stderr, "vere: -d used from a Windows console without redirection\r\n"
                    "      no output from the daemon process will be visible\r\n");
    fflush(stderr);
  }

  //  create an event for the child process to signal
  //  the parent that the ship has finished booting
  //  pass the handle as "stdin" (otherwise unused with -d)
  //
  SECURITY_ATTRIBUTES sa_u = {sizeof (SECURITY_ATTRIBUTES), NULL, TRUE};
  if ( !(psi_u.hStdInput = CreateEvent(&sa_u, TRUE, FALSE, NULL)) ) {
    fprintf(stderr, "vere: CreateEvent: %d\r\n", GetLastError());
    exit(1);
  }

  //  create the child process with the same command line as parent
  //  it will start, re-parse the command line, and call u3_daemon_init
  //
  PROCESS_INFORMATION ppi_u;
  if ( !CreateProcessW(NULL, _wcsdup(GetCommandLineW()), NULL, NULL, TRUE, DETACHED_PROCESS, NULL, NULL, &psi_u, &ppi_u) ) {
    fprintf(stderr, "vere: CreateProcess: %d\r\n", GetLastError());
    exit(1);
  }

  CloseHandle(ppi_u.hThread);

  //  wait for the child process to exit or to signal the event
  //
  DWORD exi_u;
  HANDLE han_u[2] = {ppi_u.hProcess, psi_u.hStdInput};
  switch ( WaitForMultipleObjects(2, han_u, FALSE, INFINITE) ) {
  case WAIT_OBJECT_0:
    //  the child process exited prematurely, propagate its exit code
    //
    if ( GetExitCodeProcess(ppi_u.hProcess, &exi_u) ) {
      exit(exi_u);
    }

    fprintf(stderr, "vere: GetExitCodeProcess: %d\r\n", GetLastError());
    exit(1);

  case WAIT_OBJECT_0 + 1:
    //  the child process has finished booting, exit normally
    //
    exit(0);

  default:
    fprintf(stderr, "vere: WaitForMultipleObjects: %d\r\n", GetLastError());
    exit(1);
  }
}
