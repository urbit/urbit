/* include/z/c.h
**
** This file is in the public domain.
**
** Prefixes:
**
**   u3_zc  (zeno C interfaces, for jets)
**
** Description:
**
**   These are C functions to be called inside jet optimization code.
*/
  /** Data types.
  **/
    /* zc_trap:
    **
    **   Fault codes for jets.
    */
      enum u3_zc_trap {
        /* zc_trap_fail: computation failure, eg in allocation.
        ** zc_trap_exit: true nock nontermination detected.
        ** zc_trap_punt: jet failure, retry.
        */
        u3_zc_trap_none = 0,

        u3_zc_trap_fail,
        u3_zc_trap_exit,
        u3_zc_trap_punt
      };

    /* C search structures.
    **
    ** While these same structures exist as nouns, the C forms
    ** have less overhead and support direct mutation.
    */
      /* zc_set:
      **
      **   A noun set, as a binary tree mug-ordered on (p).
      */
        struct u3_zc_set {
          /* p: set element
          ** q: null, or element < p
          ** r: null, or element > p
          */
          u3_fox           p;
          struct u3_zc_set *zs_q;
          struct u3_zc_set *zs_r;
        };

      /* zc_tab:
      **
      **   A noun table, as a binary tree mug-ordered on (p).
      */
        struct u3_zc_tab {
          /* p: key
          ** q: value
          ** r: null, or element < p
          ** s: null, or element > p
          */
          u3_fox            p;
          u3_fox            q;
          struct u3_zc_tab  *zt_r;
          struct u3_zc_tab  *zt_s;
        };


  /** Functions.
  **/
    /** Execution and allocation.
    **/
      /* u3_zc_trap():
      **
      **   Trap with longjmp().  Does not return.
      */
        int
        u3_zc_trap(u3_z            z,
                   enum u3_zc_trap trap_a);

      /* u3_zc_alloc_w(): 
      **
      **   Allocate (w_a) words on (z).  Traps if allocation fails.
      */
        void *
        u3_zc_alloc_w(u3_z z,
                      u3_w w_a);
      
      /* u3_zc_alloc_bytes():
      */
#if 0
        void *
        u3_zc_alloc_b(u3_z z,
                      u3_w w_a);
#else
#       define u3_zc_alloc_b(z, w_a) u3_zc_alloc_w(z, ((w_a + 3) >> 2))
#endif


    /** Noun construction.
    ***
    *** These functions match the equivalent u3_ln functions, but use
    *** them instead - they detect allocation failures and trap.
    **/
      /* u3_zc_bytes():
      **
      **   Copy (w_a) bytes from (y_b) into an atom on the hat of (l).
      */
        u3_fox
        u3_zc_bytes(u3_z z,
                    u3_w w_a,
                    u3_y *y_b);

      /* u3_zc_string():
      **
      **   u3_zc_bytes(l, strlen(c_a), (u3_y *)c_a);
      */
        u3_fox
        u3_zc_string(u3_z z,
                     u3_c *c_a);

      /* u3_zc_cell(): 
      **
      **   Produce the cell (a b) on the hat of (l), or u3_l_none if
      **   this would overflow the clam.
      */
        u3_fox
        u3_zc_cell(u3_z   z,
                   u3_fox a,
                   u3_fox b);

      /* u3_zc_ice():
      **
      **   Produce a noun equivalent to (a), which does not reference
      **   any data on the can of (l).
      */
        u3_fox
        u3_zc_ice(u3_z   z,
                  u3_fox a);

      /* u3_zc_mp():
      **
      **   Copy the GMP integer (mp_a) into an atom on the hat of (l).
      */
        u3_fox
        u3_zc_mp(u3_z  z,
                 mpz_t mp_a);

      /* u3_zc_trel(): 
      **
      **   Produce the trel [a b c] on the hat of [l], or u3_l_none if
      **   this would overflow the clam.
      */
        u3_fox
        u3_zc_trel(u3_z   z,
                   u3_fox a,
                   u3_fox b,
                   u3_fox c);

      /* u3_zc_weld():
      **
      **   Weld (b) into a single atom, gluing on (1 << y_a) lines.
      **   Eg, y_a = 3 to concatenate bytes, 0 for bits, 5 for words.
      */
        u3_fox
        u3_zc_weld(u3_z   z,
                   u3_y   y_a,
                   u3_fox b);
        
      /* u3_zc_words():
      **
      **   Copy (w_a) words from (w_b) into an atom on the hat of (l).
      */
        u3_fox
        u3_zc_words(u3_z z,
                    u3_w w_a,
                    u3_w *w_b);
     
  /** Search structures.
  **/
    /* u3_zc_set_new():
    **
    **   Create a set.
    */
      struct u3_zc_set *
      u3_zc_set_new(u3_z z);

    /* u3_zc_set_add():
    **
    **   Add [a] to [set_b].
    */
      struct u3_zc_set *
      u3_zc_set_add(u3_z             z,
                    u3_fox           a,
                    struct u3_zc_set *set_b);

    /* u3_zc_set_in():
    **
    **   & (0) iff [a] is in [set_b].
    */
      u3_flag
      u3_zc_set_in(u3_z             z,
                   u3_fox           a,
                   struct u3_zc_set *set_b);

    /* u3_zc_tab_new():
    **
    **   Create an association table.
    */
      struct u3_zc_tab *
      u3_zc_tab_new(u3_z z);

