#ifndef _MINGW_IO_H
#define _MINGW_IO_H

#define mkdir(A, B) mkdir(A)

char *realpath(const char *path, char *resolved_path);
int fdatasync(int fd);
int utimes(const char *path, const struct timeval times[2]);

int kill(pid_t pid, int signum);

#define SIGALRM   (NSIG+0)
#define SIGVTALRM (NSIG+1)
#define SIGINFO   (NSIG+2)
#define SIGUSR1   (NSIG+3)
#define SIG_COUNT (NSIG+4)

#endif//_MINGW_IO_H