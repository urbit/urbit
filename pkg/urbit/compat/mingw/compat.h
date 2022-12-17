#ifndef _MINGW_IO_H
#define _MINGW_IO_H

#define mkdir(A, B) mkdir(A)

int link(const char *path1, const char *path2);
ssize_t pread(int fd, void *buf, size_t count, off_t offset);
ssize_t pwrite(int fd, const void *buf, size_t count, off_t offset);
char *realpath(const char *path, char *resolved_path);
int fdatasync(int fd);
int utimes(const char *path, const struct timeval times[2]);
long sysconf(int name);

int kill(pid_t pid, int signum);

#define SIGUSR1       10
#define SIGALRM       14
#define SIGVTALRM     26
#define SIGSTK        31
#define SIG_COUNT     32
#define _SC_PAGESIZE  29

#endif//_MINGW_IO_H
