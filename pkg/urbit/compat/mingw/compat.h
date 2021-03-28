#ifndef _MINGW_IO_H
#define _MINGW_IO_H

// msvcrt setjmp/longjmp are broken on 64-bit systems, use gcc builtins
typedef struct jmp_buf {
    intptr_t buffer[5];
    int retval;
} jmp_buf;

#define _setjmp setjmp
#define _longjmp longjmp
#define longjmp(buf, val) {buf.retval = (val); __builtin_longjmp(buf.buffer, 1);}
#define setjmp(buf) (__builtin_setjmp(buf.buffer) ? (buf.retval) : 0)

// no profiling on MingW means signal masks are not used
#define sigjmp_buf jmp_buf
#define siglongjmp longjmp
#define sigsetjmp(A, B) setjmp(A)

#define mkdir(A, B) mkdir(A)

char *realpath(const char *path, char *resolved_path);
int fdatasync(int fd);
int utimes(const char *path, const struct timeval times[2]);

int kill(pid_t pid, int signum);

#define SIGALRM   1233
#define SIGVTALRM 1234
#define SIGINFO   1235
#define SIGUSR1   1236
#define SIGTSTP   1238

#define ITIMER_REAL    0
#define ITIMER_VIRTUAL 1
struct itimerval {
	struct timeval it_value, it_interval;
};
int setitimer(int type, struct itimerval *in, struct itimerval *out);

#endif//_MINGW_IO_H