#include "all.h"
#include "rsignal.h"
#include <windows.h>

int err_win_to_posix(DWORD winerr);

//  The current implementation of rsignal_ is single-threaded,
//  but it can be extended to multi-threaded by replacing these
//  static variables with a thread id-based hash map.
//
static __p_sig_fn_t   _fns[SIG_COUNT];
static volatile DWORD _tid;
static HANDLE _hvt;

void rsignal_install_handler(int sig, __p_sig_fn_t fn)
{
  if (sig < 0 || sig >= SIG_COUNT)
    return;

  DWORD newtid = GetCurrentThreadId();
  DWORD oldtid = InterlockedExchange(&_tid, newtid);
  if (oldtid != 0 && oldtid != newtid) {
    fprintf(stderr, "\r\nrsignal_install_handler: %u -> %u\r\n", oldtid, newtid);
    return;
  }

  __p_sig_fn_t oldfn = InterlockedExchangePointer((PVOID*)&_fns[sig], fn);
  if (fn != 0 && oldfn != 0 && oldfn != fn) {
    fprintf(stderr, "\r\nrsignal_install_handler: %p -> %p\r\n", oldfn, fn);
  }
}

void rsignal_deinstall_handler(int sig)
{
    rsignal_install_handler(sig, 0);
}

void rsignal_raise(int sig)
{
  if (sig < 0 || sig >= SIG_COUNT)
    return;

  __p_sig_fn_t oldfn = InterlockedExchangePointer((PVOID*)&_fns[sig], 0);
  if (oldfn == 0)
    return;

  if (_tid == GetCurrentThreadId()) {
    oldfn(sig);
    return;
  }

  HANDLE hthread = OpenThread(THREAD_ALL_ACCESS, FALSE, _tid);
  if (!hthread) {
    fprintf(stderr, "\r\nrsignal_raise: OpenThread(%u): %d\r\n", _tid, GetLastError());
    return;
  }

  if (SuspendThread(hthread) < 0) {
    fprintf(stderr, "\r\nrsignal_raise: SuspendThread(%u): %d\r\n", _tid, GetLastError());
    goto cleanup;
  }

  oldfn(sig);

  if (!ResumeThread(hthread)) {
    fprintf(stderr, "\r\nrsignal_raise: ResumeThread(%u): %d\r\n", _tid, GetLastError());

    // abort because the main thread is stuck
    abort();
  }

cleanup:
  CloseHandle(hthread);
}

static void _rsignal_vt_cb(PVOID param, BOOLEAN timedOut)
{
  rsignal_raise(SIGVTALRM);
}

int rsignal_setitimer(int type, struct itimerval *in, struct itimerval *out)
{
  if (in == 0) {
    errno = EFAULT;
    return -1;
  }

  if (type != ITIMER_VIRTUAL || out != 0) {
    errno = ENOTSUP;
    return -1;
  }

  if (_hvt != NULL) {
    DeleteTimerQueueTimer(NULL, _hvt, NULL);
    _hvt = NULL;
  }

  if (timerisset(&in->it_value) && !CreateTimerQueueTimer(&_hvt, NULL, _rsignal_vt_cb, NULL,
    in->it_value.tv_sec    * 1000 + in->it_value.tv_usec    / 1000,
    in->it_interval.tv_sec * 1000 + in->it_interval.tv_usec / 1000, 0))
  {
    errno = err_win_to_posix(GetLastError());
    return -1;
  } else {
    return 0;
  }
}

// direct import from ntdll.dll
extern DWORD64 __imp_KiUserExceptionDispatcher;

static void _rsignal_longjmp(intptr_t* builtin_jb)
{
  __builtin_longjmp(builtin_jb, 1);
}

void rsignal_post_longjmp(DWORD tid, intptr_t* builtin_jb)
{
  HANDLE hthread = OpenThread(THREAD_ALL_ACCESS, FALSE, tid);
  if (!hthread) {
    fprintf(stderr, "\r\nrsignal: OpenThread(%u): %d\r\n", tid, GetLastError());
    return;
  }

  CONTEXT context;
  context.ContextFlags = CONTEXT_CONTROL | CONTEXT_INTEGER;
  if (!GetThreadContext(hthread, &context)) {
    fprintf(stderr, "\r\nrsignal: GetThreadContext(%u): %d\r\n", tid, GetLastError());
    goto cleanup;
  }

  //  see if the thread is currently handling a structured exception
  //  if so, let the handler (usually the libsigsegv handler) finish
  //  and set up the the signal to run at the exception resume point
  //  otherwise, passing a parameter to fn is completely unreliable
  //
  DWORD64 kibase;
  PRUNTIME_FUNCTION ki = RtlLookupFunctionEntry(__imp_KiUserExceptionDispatcher, &kibase, NULL);
  CONTEXT c = context;
  while (1)
  {
    DWORD64 base, frame;
    PRUNTIME_FUNCTION f = RtlLookupFunctionEntry(c.Rip, &base, NULL);
    if (!f) break;
    if (f == ki)
    {
      //  KiUserExceptionDispatcher has a "bare" frame
      //  with $rsp pointing to the CONTEXT structure
      //
      ((PCONTEXT)c.Rsp)->Rip = (DWORD64)_rsignal_longjmp;
      ((PCONTEXT)c.Rsp)->Rcx = (DWORD64)builtin_jb;
      goto cleanup;
    }
    PVOID handler_data;
    RtlVirtualUnwind(0, base, c.Rip, f, &c, &handler_data, &frame, NULL);
  }

  context.Rip = (DWORD64)_rsignal_longjmp;
  context.Rcx = (DWORD64)builtin_jb;
  if (!SetThreadContext(hthread, &context)) {
    fprintf(stderr, "\r\nrsignal: SetThreadContext(%u): %d\r\n", tid, GetLastError());
    goto cleanup;
  }

cleanup:
  CloseHandle(hthread);
}
