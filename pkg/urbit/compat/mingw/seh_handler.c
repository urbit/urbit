#include "all.h"
#include "rsignal.h"
#include "vere/vere.h"

/* _mingw_exception_filter: replaces libsigsegv on MingW
*/
EXCEPTION_DISPOSITION _mingw_exception_filter(
    IN PEXCEPTION_RECORD ExceptionRecord,
    IN ULONG64 EstablisherFrame,
    IN OUT PCONTEXT ContextRecord,
    IN OUT PDISPATCHER_CONTEXT DispatcherContext)
{
    if (ExceptionRecord->ExceptionCode == EXCEPTION_ACCESS_VIOLATION &&
        ExceptionRecord->ExceptionInformation[0] == 1 &&
        u3e_fault((void*)ExceptionRecord->ExceptionInformation[1], 1))
    {
        return ExceptionContinueExecution;
    }

    if (ExceptionRecord->ExceptionCode == EXCEPTION_STACK_OVERFLOW) {
        rsignal_raise(SIGSTK);
    }

    return ExceptionContinueSearch;
}
