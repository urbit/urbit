//! @file lock.c

#include "vere/lock.h"

#include "c/portable.h"
#include "c/types.h"
#include "c/defs.h"

//==============================================================================
// Constants
//==============================================================================

static const c3_c const lok_nam_c[] = ".vere.lock";

//==============================================================================
// Functions
//==============================================================================

void
u3_lock_acquire(c3_path* const pax_u)
{
  c3_path_push(pax_u, lok_nam_c);
  c3_w        pid_w;
  FILE*       lok_u;
  const c3_c* pax_c = c3_path_str(pax_u);

  if ( lok_u = c3_fopen(pax_c, "r") ) {
    if ( 1 != fscanf(lok_u, "%" SCNu32, &pid_w) ) {
      fprintf(stderr, "lockfile %s is corrupt!\r\n", pax_c);
      kill(getpid(), SIGTERM);
      sleep(1);
      c3_assert(0);
    }
    else if ( getpid() != pid_w ) {
      c3_i       sig_i = SIGTERM;
      c3_w       idx_w;
      const c3_w cnt_w = 32;
      fprintf(stderr,
              "evlo: stopping process %u, live in %s...\r\n",
              pid_w,
              pax_c);
      for ( idx_w = 0; idx_w < cnt_w; idx_w++ ) {
        if ( 0 == kill(pid_w, sig_i) ) {
          break;
        }
        sleep(1);
        if ( cnt_w / 2 == idx_w ) {
          sig_i = SIGKILL;
        }
      }
      if ( cnt_w == idx_w ) {
        fprintf(stderr, "evlo: process %u seems unkillable!\r\n", pid_w);
        c3_assert(0);
      }
      fprintf(stderr, "evlo: stopped old process %u\r\n", pid_w);
    }
    fclose(lok_u);
    c3_unlink(pax_c);
  }

  if ( !(lok_u = c3_fopen(pax_c, "w")) ) {
    fprintf(stderr, "evlo: unable to open %s\r\n", pax_c);
    c3_assert(0);
  }

  fprintf(lok_u, "%u\n", getpid());
  c3_sync(fileno(lok_u));

  fclose(lok_u);
  c3_path_pop(pax_u); 
}

void
u3_lock_release(c3_path* const pax_u)
{
  c3_path_push(pax_u, lok_nam_c);
  const c3_c* const pax_c = c3_path_str(pax_u);
  c3_unlink(pax_c);
  c3_path_pop(pax_u);
}
