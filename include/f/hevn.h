/* include/hevn.h
**
** This file is in the public domain.
*/
  /** Data structures.
  **/
    /* u2_pryr: takes a path and produces a unit.
    */
      typedef u2_noun (*u2_pryr)(u2_noun);

    /* u2_loom_hevn: global namespace control structure.
    */
      typedef struct _u2_loom_hevn {
        /* Don't set this unless you know what you're doing.
        */
        u2_pryr god;

        /* Mock gate stack.  Pop the stack when executing.
        **
        ** Each entry in this list is a crash-only gate.  If the
        ** list is empty, we are in kernel mode & god is active.
        */
        u2_noun lad;
      } u2_loom_hevn;

#define   u2_hevx_at(hev_r, wof)        (*u2_at(hev_r, u2_loom_hevn, wof))
#define   u2_hevx_be(hev_r, ite, wof)   (*u2_be(hev_r, u2_loom_hevn, ite, wof))

#define   u2_hevn_at(wof)      u2_hevx_at(u2_wire_hev_r(u2_Wire), wof)
#define   u2_hevn_be(ite, wof) u2_hevx_be(u2_wire_hev_r(u2_Wire), ite, wof)

  /** Functions.
  **/
    /** Lifecycle.
    **/
      /* u2_hv_init(): initialize state.
      */
        u2_ray
        u2_hv_init(u2_ray wir_r);

      /* u2_hv_mark(): mark heaven.
      */
        c3_w
        u2_hv_mark();
