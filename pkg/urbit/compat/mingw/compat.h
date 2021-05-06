#ifndef _MINGW_IO_H
#define _MINGW_IO_H

#define mkdir(A, B) mkdir(A)

char *realpath(const char *path, char *resolved_path);
int fdatasync(int fd);
int utimes(const char *path, const struct timeval times[2]);

int kill(pid_t pid, int signum);

#define SIGUSR1   10
#define SIGALRM   14
#define SIGVTALRM 26
#define SIG_COUNT 32

#endif//_MINGW_IO_H