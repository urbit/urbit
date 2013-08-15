/* include/b.h
**
** This file is in the public domain.
**
** Prefixes:
**
**   u3_b   (bunt)
**
** Description:
**
**   Bunt functions, which are implemented with the old (u4) 
**   system.
*/

  /** Functions.
  **/
    /** Impure.
    **/
      /* u3_b_init(): 
      **
      **   Initialize static bunt services.
      */
        void
        u3_b_init(void);

      /* u3_b_print():
      **
      **   Print [piv], with [c_tid] if nonzero as a caption.
      */
        void
        u3_b_print(u3_lv      lv,
                   const c3_c *c_tid,
                   u3_rat     piv);

      /* u3_b_print_type():
      **
      **   Print [piv] as a type, with [c_tid] if nonzero as a caption.
      */
        void
        u3_b_print_type(u3_lv      lv,
                        const c3_c *c_tid,
                        u3_rat     piv);

    /** Pure.
    **/
      /* u3_b_mug()::
      */
        u3_fox
        u3_b_mug(u3_l l,
                 u3_fox vup);

      /* u3_b_nock():
      **
      **   Use bunt routines to execute (nock lan sef).
      */
        u3_rat
        u3_b_nock(u3_l   l,
                  u3_fox lan,
                  u3_fox sef);

      /* u3_b_load():
      **
      **   Use bunt routines to load [rop].
      */
        u3_rat 
        u3_b_load(u3_l,
                  u3_fox rop);

      /* u3_b_read():
      **
      **   Use bunt routines to parse (wek), as watt.
      */
        u3_rat
        u3_b_read(u3_l   l,
                  u3_fox wek);

      /* u3_b_vere():
      **
      **   Use bunt routines to parse (wek), as vere.
      */
        u3_rat
        u3_b_vere(u3_l   l,
                  u3_fox wek);

      /* u3_b_hume():
      **
      **   Use bunt routines to parse (wek), as hume.
      */
        u3_rat
        u3_b_hume(u3_l   l,
                  u3_fox wek);

      /* u3_b_mill(), etc:
      */
        u3_rat u3_b_make(u3_l l, u3_fox sut, u3_fox gen, u3_mote* how);
        u3_rat u3_b_play(u3_l l, u3_fox sut, u3_fox gen, u3_mote* how);
        u3_rat u3_b_show(u3_l l, u3_fox sut, u3_fox gen, u3_mote* how);
        u3_rat u3_b_pass(u3_l l, u3_fox sut, u3_fox gen, u3_mote* how);
        u3_rat u3_b_shop(u3_l l, u3_fox sut, u3_fox gen, u3_mote* how);
        u3_rat u3_b_wish(u3_l l, u3_fox sut, u3_fox gen, u3_mote* how);
        u3_rat u3_b_mill(u3_l l, u3_fox sut, u3_fox gen, u3_mote* how);

