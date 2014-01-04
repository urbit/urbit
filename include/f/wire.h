/* include/wire.h
**
** This file is in the public domain.
*/
  /** Data types.
  **/
    /** Structures - in loom space.
    **/
      /* u2_loom_wire: one per thread.
      */
        typedef struct _u2_loom_wire {
          u2_loom_rail o;

          /* Clean bean.  u2_no when there may be garbage.
          */
          u2_noun lan;

          /* Basket; u2_loom_bask; senior storage (for dashboard).
          */
          u2_ray bas_r;

          /* Dashboard; u2_loom_dash; map battery to chip list.
          */
          u2_cash_slot des_s;

          /* Ray to jet exception buffer, or 0.
          */
          u2_ray kit_r;

          /* Ray to OS state.
          */
          u2_ray arv_r;

          /* Ray to performance log.
          */
          u2_ray bex_r;

          /* Interpreter trace.
          */
          u2_noun tax;

          /* Ray to new tracer.
          */
          u2_ray rac_r;

          /* Heaven.
          */
          u2_ray hev_r;
        } u2_loom_wire;

#         define  u2_wire_bas_r(wir_r)  *u2_at(wir_r, u2_loom_wire, bas_r)
#         define  u2_wire_des_r(wir_r)  u2_aftr(wir_r, u2_loom_wire, des_s) 
#         define  u2_wire_kit_r(wir_r)  *u2_at(wir_r, u2_loom_wire, kit_r)
#         define  u2_wire_bex_r(wir_r)  *u2_at(wir_r, u2_loom_wire, bex_r)
#         define  u2_wire_rac_r(wir_r)  *u2_at(wir_r, u2_loom_wire, rac_r)
#         define  u2_wire_hev_r(wir_r)  *u2_at(wir_r, u2_loom_wire, hev_r)
#         define  u2_wire_arv_r(wir_r)  *u2_at(wir_r, u2_loom_wire, arv_r)

#         define  u2_wire_tax(wir_r)    *u2_at(wir_r, u2_loom_wire, tax)
#         define  u2_wire_lan(wir_r)    *u2_at(wir_r, u2_loom_wire, lan)


    /** Functions.
    **/
      /** Administration.
      **/
        /* u2_wr_init():
        **
        **   Install an empty wire within `hat_r` and `mat_r` in the loom,
        **   with memory model `hip`.
        **
        **   Returns ray to wire, which always equalls the passed `mat_r`.
        */
          u2_ray
          u2_wr_init(c3_m   hip_m,
                     u2_ray hat_r,
                     u2_ray mat_r);

        /* u2_wr_check_init(): initialize checkpoint segments and/or files.
        */
          void
          u2_wr_check_init(c3_c* cpu_c);

        /* u2_wr_check_save(): checkpoint wire in global structure.
        */
          void
          u2_wr_check_save();

        /* u2_wr_ice(): u2_rl_ice(), with u2_bx_copy().
        */
          u2_weak
          u2_wr_ice(u2_ray  wir_r,
                    u2_noun fiz);

        /* u2_wr_gc():
        **
        **   Garbage-collect all current storage in a wire, given
        **   a 0-terminated list of external roots.
        */
          void
          u2_wr_gc(u2_ray wir_r, ...);

        /* u2_wr_mark():
        **
        **   Mark all roots in a wire and return their allocation size.
        */
          c3_w
          u2_wr_mark(u2_ray wir_r);
