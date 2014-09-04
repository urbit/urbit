/* include/g/x.h
**
** This file is in the public domain.
*/
    /** u2_cx_*: read, but bail with c3__exit on a crash.
    **/
#if 1
#     define u2_cx_h(som)  u2_co_h(som)
#     define u2_cx_t(som)  u2_co_t(som)
#else
      /* u2_cx_h (u2h): head.
      */
        u2_noun
        u2_cx_h(u2_noun som);

      /* u2_cx_t (u2t): tail.
      */
        u2_noun
        u2_cx_t(u2_noun som);
#endif
      /* u2_cx_good(): test for u2_none.
      */
        u2_noun
        u2_cx_good(u2_weak som);

      /* u2_cx_at (u2at): fragment.
      */
        u2_noun
        u2_cx_at(u2_noun axe, u2_noun som);

      /* u2_cx_cell():
      **
      **   Divide `a` as a cell `[b c]`.
      */
        void
        u2_cx_cell(u2_noun  a,
                   u2_noun* b,
                   u2_noun* c);

      /* u2_cx_trel():
      **
      **   Divide `a` as a trel `[b c d]`, or bail.
      */
        void
        u2_cx_trel(u2_noun  a,
                   u2_noun* b,
                   u2_noun* c,
                   u2_noun* d);

      /* u2_cx_qual():
      **
      **   Divide `a` as a quadruple `[b c d e]`.
      */
        void
        u2_cx_qual(u2_noun  a,
                   u2_noun* b,
                   u2_noun* c,
                   u2_noun* d,
                   u2_noun* e);
