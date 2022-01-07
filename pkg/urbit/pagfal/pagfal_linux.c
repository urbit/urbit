/* pagfal/pagfal_linux.c
**
*/
#include <errno.h>
#include <stdio.h>

#include "all.h"


#if defined(USE_LIBSIGSEGV)

# include <sigsegv.h>

c3_o
u3f_install_handler(u3f_hand han_f)
{
  if ( 0 != sigsegv_install_handler(han_f) ) {
    fprintf(stderr, "fault: failed to install page fault handler\r\n");
    return c3n;
  }
  return c3y;
}

c3_o
u3f_protect(void* adr_v, c3_w len_w, c3_w pro_w)
{
  switch ( pro_w ) {
    case u3f_read:
      pro_w = PROT_READ;
      break;
    case u3f_write:
      pro_w = PROT_WRITE;
      break;
    case u3f_read | u3f_write:
      pro_w = (PROT_READ | PROT_WRITE);
      break;
    default:
      fprintf(stderr, "fault: invalid access protection 0x%x\r\n", pro_w);
      return c3n;
  }

  if ( -1 == mprotect(adr_v, len_w, pro_w) )
  {
    fprintf(stderr,
            "fault: unable to apply access protection for adr_v=%p, "
            "len_w=0x%x, pro_w=0x%x: %s\r\n",
            adr_v,
            len_w,
            pro_w,
            strerror(errno));
    return c3n;
  }
  return c3y;
}

#else

# include <fcntl.h>
# include <linux/userfaultfd.h>
# include <poll.h>
# include <pthread.h>
# include <sys/ioctl.h>
# include <sys/mman.h>
# include <sys/syscall.h>

/* The definitions of some important userfaultfd macros are missing from the
** version of userfaultfd.h pulled in by nix, but they're present in
** /usr/include/linux/userfaultfd.h on my local machine.
*/
#ifndef UFFD_USER_MODE_ONLY
# define UFFD_USER_MODE_ONLY 1
#endif /* ifndef UFFD_USER_MODE_ONLY */

#ifndef _UFFDIO_WRITEPROTECT
# define _UFFDIO_WRITEPROTECT 6
#endif /* ifndef UFFDIO_WRITEPROTECT */

#if 0
struct uffdio_writeprotect {
	struct uffdio_range range;
/*
 * UFFDIO_WRITEPROTECT_MODE_WP: set the flag to write protect a range,
 * unset the flag to undo protection of a range which was previously
 * write protected.
 *
 * UFFDIO_WRITEPROTECT_MODE_DONTWAKE: set the flag to avoid waking up
 * any wait thread after the operation succeeds.
 *
 * NOTE: Write protecting a region (WP=1) is unrelated to page faults,
 * therefore DONTWAKE flag is meaningless with WP=1.  Removing write
 * protection (WP=0) in response to a page fault wakes the faulting
 * task unless DONTWAKE is set.
 */
#ifndef UFFD_USER_MODE_ONLY
# define UFFDIO_WRITEPROTECT_MODE_WP		((__u64)1<<0)
#endif /* ifndef UFFD_USER_MODE_ONLY */
#ifndef UFFDIO_WRITEPROTECT_MODE_DONTWAKE	
# define UFFDIO_WRITEPROTECT_MODE_DONTWAKE	((__u64)1<<1)
#endif /* ifndef UFFDIO_WRITEPROTECT_MODE_DONTWAK */
	__u64 mode;
};
#endif

#ifndef UFFDIO_WRITEPROTECT
# define UFFDIO_WRITEPROTECT _IOWR(UFFDIO, \
                                   _UFFDIO_WRITEPROTECT, \
                                   struct uffdio_writeprotect)
#endif /* ifndef UFFDIO_WRITEPROTECT */


/* Userfaultfd.
*/
static c3_i uff_i;

/* User-defined page fault handler.
*/
static u3f_hand use_han_f = NULL;

/* _cf_handle_page_fault(): unpack page fault events from userfaultfd and invoke
** u3e_fault.
*/
static void*
_cf_page_fault_handle(void* arg)
{
  struct pollfd pof_st = {
    .fd = uff_i,
    .events = POLLIN,
  };
  struct uffd_msg mes_st;

#define POLL_BLOCK_UNTIL_EVENT -1

  while ( 1 ) {
    // Wait for a page fault event to occur.
    switch ( poll(&pof_st, 1, POLL_BLOCK_UNTIL_EVENT) ) {
      case -1:
        fprintf(stderr,
                "fault: failed to poll events on userfaultfd: %s\r\n",
                strerror(errno));
        c3_assert(0);
        return NULL;
      case 0:
        continue;
      case 1:
        break;
      default:
        fprintf(stderr,
                "fault: received multiple poll events on userfaultfd "
                "but only expected one\r\n");
        c3_assert(0);
        return NULL;
    }
    c3_assert( POLLIN & pof_st.events );

    // Read the page fault event from userfaultfd.
    switch ( read(uff_i, &mes_st, sizeof(mes_st)) ) {
      case -1:
        fprintf(stderr,
                "fault: failed to read from userfaultfd: %s\r\n",
                strerror(errno));
        c3_assert(0);
        return NULL;
      case 0:
        fprintf(stderr,
                "fault: encountered EOF on userfaultfd: %s\r\n",
                strerror(errno));
        c3_assert(0);
        return NULL;
      case sizeof(mes_st):
        break;
      default:
        fprintf(stderr,
                "fault: read unexpected number of bytes from userfaultfd\r\n");
        c3_assert(0);
        return NULL;
    }

    // Verify that event is in fact a page fault.
    if ( UFFD_EVENT_PAGEFAULT != mes_st.event ) {
      fprintf(stderr,
              "fault: encountered unexpected event on userfaultfd\r\n");
      c3_assert(0);
      return NULL;
    }

    c3_d fla_d = mes_st.arg.pagefault.flags;
    c3_assert( UFFD_PAGEFAULT_FLAG_WRITE & fla_d );

    void* adr_v = (void*)(mes_st.arg.pagefault.address);
    if ( !(UFFD_PAGEFAULT_FLAG_WP & fla_d) ) {
      c3_w len_w = 1 << (u3a_page + 2);
      struct uffdio_zeropage ufz_st = {
        .range = {
          .start = c3_rud((c3_d)adr_v, len_w),
          .len = len_w,
        }
      };
      if ( -1 == ioctl(uff_i, UFFDIO_ZEROPAGE, &ufz_st) ) {
        fprintf(stderr,
                "fault: failed to map zero Urbit-sized "
                "page starting at address %p",
                adr_v);
        c3_assert(0);
        return NULL;
      }
      if ( (void*)U3_OS_LoomBase == adr_v ) {
        fprintf(stderr, "peter: missing page at adr_v=%p\r\n", adr_v);
      }
    }
    else {
      fprintf(stderr, "peter: write-protected page at adr_v=%p\r\n", adr_v);
    }

    //fprintf(stderr, "peter: adr_v=%p\r\n", adr_v);
    use_han_f(adr_v, 1);
  }
}

c3_o
u3f_install_handler(u3f_hand han_f)
{
  // Create userfaultfd.
  {
    // From the userfaultfd man page:
    //  If the O_NONBLOCK flag is not enabled, then poll(2) (always) indicates
    //  the file as having a POLLERR condition.
    uff_i = syscall(SYS_userfaultfd, UFFD_USER_MODE_ONLY | O_CLOEXEC | O_NONBLOCK);
    if ( -1 == uff_i ) {
      fprintf(stderr,
              "fault: failed to create userfaultfd: %s\r\n",
              strerror(errno));
      return c3n;
    }
  }

  // Enable userfaultfd.
  {
    struct uffdio_api ufa_st = {
      .api = UFFD_API,
      .features = 0,
    };
    if ( -1 == ioctl(uff_i, UFFDIO_API, &ufa_st) ) {
      fprintf(stderr,
              "fault: failed to enable userfaultfd: %s\r\n",
              strerror(errno));
      return c3n;
    }
    c3_assert( UFFD_FEATURE_PAGEFAULT_FLAG_WP & ufa_st.features );
  }

  // Register the loom.
  // TODO(peter): refactor into u3f_register function so that this
  // implementation knows nothing about the loom.
  {
    // When a page is missing, a missing page event is triggered, not a
    // write-protect event. Unfortunately, handling a missing page event
    // confuses the snapshotting system. To get around this, we need to read
    // from each page to force the OS to swap the page into memory so that
    // write-protect events will be triggered upon subsequent writes.
#if 0
    for ( c3_w off_w = 0; off_w < u3a_bytes; off_w += U3_OS_NativePageBytes ) {
      c3_y bog_y = *(c3_y*)(U3_OS_LoomBase + off_w);
    }
#endif

    struct uffdio_register ufr_st = {
      .range = {
        .start = U3_OS_LoomBase,
        .len = u3a_bytes,
      },
      .mode = UFFDIO_REGISTER_MODE_MISSING | UFFDIO_REGISTER_MODE_WP,
    };
    if ( -1 == ioctl(uff_i, UFFDIO_REGISTER, &ufr_st) ) {
      fprintf(stderr, "fault: failed to register the loom with userfaultfd\r\n");
      return c3n;
    }
  }

  // From the pthread_create man page:
  //  The new thread inherits a copy of the creating thread's signal mask
  //  (pthread_sigmask(3)). The set of pending signals for the new thread is
  //  empty (sigpending(2)).  The new thread does not inherit the creating
  //  thread's alternate signal stack (sigaltstack(2)).
  {
    pthread_t ted_d;
    errno = pthread_create(&ted_d, NULL, _cf_page_fault_handle, NULL);
    if ( -1 == errno ) {
      fprintf(stderr,
              "boot: failed to launch page fault handler thread: %s\r\n",
              strerror(errno));
      return c3n;
    }
  }

  use_han_f = han_f;

  return c3y;
}

c3_o
u3f_protect(void* adr_v, c3_w len_w, c3_w pro_w)
{
  // Use native page granularity to mimic the behavior of mprotect().
  adr_v = (void*)c3_rud((c3_d)adr_v, U3_OS_NativePageBytes);
  len_w = c3_rup(len_w, U3_OS_NativePageBytes);
  struct uffdio_writeprotect ufp_st = {
    .range = {
      .start = (c3_d)adr_v,
      .len = len_w,
    },
    .mode = 0,
  };

  switch (pro_w) {
    case u3f_read:
      ufp_st.mode |= UFFDIO_WRITEPROTECT_MODE_WP;
      break;
    case u3f_write:
    case u3f_read | u3f_write:
      ufp_st.mode &= ~UFFDIO_WRITEPROTECT_MODE_WP;
      break;
    default:
      fprintf(stderr, "fault: invalid access protection 0x%x\r\n", pro_w);
      return c3n;
  }

  if ( -1 == ioctl(uff_i, UFFDIO_WRITEPROTECT, &ufp_st) ) {
    fprintf(stderr,
            "fault: failed to %s write protections on [%p, %p): %s\r\n",
            u3f_read == pro_w ? "enable" : "disable",
            adr_v,
            (c3_y*)adr_v + len_w,
            strerror(errno));
    return c3n;
  }
  return c3y;
}

c3_o
u3f_fetch(void* adr_v, c3_w len_w)
{
  return c3n;
}

#endif /* defined(USE_LIBSIGSEGV) */
