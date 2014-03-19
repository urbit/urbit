/* include/unix.h
**
** This file is in the public domain.
*/
  /** Structures.
  **/
    /* u2_life: a monitored file.
    */
      typedef struct _u2_life {
        const c3_c*      src_c;                 //  source path
        const c3_c*      dat_c;                 //  data path
        c3_d             act_d;                 //  tick updated at
        struct timespec  tim_p;                 //  date of source at update
        u2_weak          src;                   //  source noun
        u2_weak          dat;                   //  data noun
        void*            han_v;                 //  dlopen handle
        struct _u2_life* nex_l;
      } *u2_life;

  /** Functions.
  **/
    /* u2_ux_life(): create a monitored file, not loading.
    */
      u2_life
      u2_ux_life(u2_wire     wir_r,
                 const c3_c* src_c,
                 const c3_c* dat_c,
                 const c3_c* oxt_c,
                 u2_life     nex_l);

    /* u2_ux_live(): load/reload a monitored source; u2_yes for change.
    */
      u2_bean
      u2_ux_live(u2_wire wir_r,
                 u2_life lif_l);

    /* u2_ux_lose(): forget a monitored file.
    */
      void
      u2_ux_lose(u2_wire wir_r,
                 u2_life lif_l);

    /* u2_ux_read(): read a path/extension into an atom.
    */
      u2_weak
      u2_ux_read(u2_wire     wir_r,
                 const c3_c* paf_c,
                 const c3_c* ext_c);

    /* u2_ux_read_deep(): read a filesystem path as a generic noun.
    */
      u2_weak
      u2_ux_read_deep(u2_wire     wir_r,
                      const c3_c* paf_c,
                      const c3_c* ext_c);

    /* u2_ux_write(): write a path/extension as an atom.
    */
      u2_bean
      u2_ux_write(u2_wire     wir_r,
                  u2_atom     som,
                  const c3_c* paf_c,
                  const c3_c* ext_c);

    /* u2_ux_write_deep(): write a path/extension as a generic noun.
    */
      u2_bean
      u2_ux_write_deep(u2_wire     wir_r,
                       u2_noun     som,
                       const c3_c* paf_c,
                       const c3_c* ext_c);

    /* u2_ux_fresh(): true iff `oxt` is as fresh as `ext`.
    */
      u2_bean
      u2_ux_fresh(const c3_c* paf_c,
                  const c3_c* ext_c,
                  const c3_c* oxt_c);
