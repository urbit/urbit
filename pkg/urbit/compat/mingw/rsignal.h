#ifndef _RSIGNAL_H
#define _RSIGNAL_H

typedef struct {
  jmp_buf jb;
  unsigned long tid;
} rsignal_jmpbuf;

#define rsignal_setjmp(buf)        (buf.tid = GetCurrentThreadId(), setjmp(buf.jb))
#define rsignal_longjmp(buf, val)  if (buf.tid != GetCurrentThreadId()) {buf.jb.retval = (val); rsignal_post_longjmp(buf.tid, buf.jb.buffer);} else longjmp(buf.jb, val)

void rsignal_raise(int sig);
void rsignal_install_handler(int sig, __p_sig_fn_t fn);
void rsignal_deinstall_handler(int sig);
void rsignal_post_longjmp(unsigned long tid, intptr_t* builtin_jb);

#define ITIMER_VIRTUAL 1
struct itimerval {
	struct timeval it_value, it_interval;
};

int rsignal_setitimer(int type, struct itimerval *in, struct itimerval *out);

#endif//_RSIGNAL_H
