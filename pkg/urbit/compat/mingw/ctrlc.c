#include <signal.h>

// initialize msvcrt signals early, otherwise Ctrl-C does nothing
static void __attribute__ ((constructor)) _init_crt_signals()
{
    signal(SIGINT, SIG_DFL);
}
