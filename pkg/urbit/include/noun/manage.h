#ifndef U3_MANAGE_H
#define U3_MANAGE_H

//! Start the u3 system.
//!
//! @param[in] dir_c  Pier directory.
//! @param[in] sap_f  Function used to load the snapshot. Must be one of
//!                   u3e_live() or u3e_load().
//!
//! @return  Next event number (starting from 1).
c3_d
u3m_boot(const c3_c* dir_c, c3_o (*sap_f)(const c3_c* dir_c, const c3_o map_o));

//! start without checkpointing.
c3_d
u3m_boot_lite(void);

/* u3m_stop(): graceful shutdown cleanup. */
void
u3m_stop(void);

//! Bail out.  Does not return.
//!
//! Bail motes:
//!
//!   %exit               ::  semantic failure
//!   %evil               ::  bad crypto
//!   %intr               ::  interrupt
//!   %fail               ::  execution failure
//!   %foul               ::  assert failure
//!   %need               ::  network block
//!   %meme               ::  out of memory
//!   %time               ::  timed out
//!   %oops               ::  assertion failure
c3_i
u3m_bail(c3_m how_m) __attribute__((noreturn));

//! Start the environment.
void
u3m_init();

//! Instantiate or activate image.
void
u3m_pave(c3_o nuu_o);

//! Load file, as atom, or bail.
u3_noun
u3m_file(c3_c* pas_c);

//! Bail out with %exit, ct_pushing error.
c3_i
u3m_error(c3_c* str_c);

//! New, integrated leap mechanism (enter).
void
u3m_hate(c3_w pad_w);

//! Return product from leap.
u3_noun
u3m_love(u3_noun pro);

//! system soft wrapper.  unifies unix and nock errors.
//!
//! Produces [%$ result] or [%error (list tank)].
u3_noun
u3m_soft(c3_w mil_w, u3_funk fun_f, u3_noun arg);

//! Top-level call.
u3_noun
u3m_soft_slam(u3_noun gat, u3_noun sam);

//! Top-level nock.
u3_noun
u3m_soft_nock(u3_noun bus, u3_noun fol);

//! Top-level call assumed correct.
u3_noun
u3m_soft_sure(u3_funk fun_f, u3_noun arg);

//! Descend into virtualization context.
u3_noun
u3m_soft_run(u3_noun gul,
             u3_funq fun_f,
             u3_noun aga,
             u3_noun agb);

//! Namespace lookup to (unit ,*).
u3_noun
u3m_soft_esc(u3_noun ref, u3_noun sam);

//! Mark all nouns in the road.
c3_w
u3m_mark(FILE* fil_u);

//! Garbage-collect the world, plus extra roots.
void
u3m_grab(u3_noun som, ...);   // terminate with u3_none

//! Produce high and low watermarks.  Asserts u3R == u3H.
void
u3m_water(c3_w *low_w, c3_w *hig_w);

//! Dumb prettyprint to string. RETAIN.
c3_c*
u3m_pretty(u3_noun som);

//! Prettyprint a path to string. RETAIN.
c3_c*
u3m_pretty_path(u3_noun som);

//! Dumb print with caption. RETAIN.
void
u3m_p(const c3_c* cap_c, u3_noun som);

//! Dump a tape to stdout.
void
u3m_tape(u3_noun tep);

//! Dump a wall to stdout.
void
u3m_wall(u3_noun wol);

//! Clear persistent caches to reclaim memory.
void
u3m_reclaim(void);

//! Compact (defragment) memory.
c3_w
u3m_pack(void);

#endif /* ifndef U3_MANAGE_H */
