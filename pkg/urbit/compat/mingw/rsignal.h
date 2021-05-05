#ifndef _RSIGNAL_H
#define _RSIGNAL_H

#define rsignal_jmpbuf                  jmp_buf
#define rsignal_setjmp                  setjmp
#define rsignal_longjmp                 longjmp
#define rsignal_install_handler         signal
#define rsignal_deinstall_handler(sig)  signal((sig), SIG_IGN)
#define rsignal_setitimer(sig,in,out)   0

#define ITIMER_VIRTUAL 1
struct itimerval {
	struct timeval it_value, it_interval;
};

#endif//_RSIGNAL_H
