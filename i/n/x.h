/* i/n/x.h
**
** This file is in the public domain.
*/
    /** u3x_*: read, but bail with c3__exit on a crash.
    **/
#if 1
#     define u3x_h(som)  u3a_h(som)
#     define u3x_t(som)  u3a_t(som)
#else
      /* u3x_h (u3h): head.
      */
        u3_noun
        u3x_h(u3_noun som);

      /* u3x_t (u3t): tail.
      */
        u3_noun
        u3x_t(u3_noun som);
#endif
      /* u3x_good(): test for u3_none.
      */
        u3_noun
        u3x_good(u3_weak som);

      /* u3x_at (u3at): fragment.
      */
        u3_noun
        u3x_at(u3_noun axe, u3_noun som);

      /* u3x_cell():
      **
      **   Divide `a` as a cell `[b c]`.
      */
        void
        u3x_cell(u3_noun  a,
                   u3_noun* b,
                   u3_noun* c);

      /* u3x_trel():
      **
      **   Divide `a` as a trel `[b c d]`, or bail.
      */
        void
        u3x_trel(u3_noun  a,
                   u3_noun* b,
                   u3_noun* c,
                   u3_noun* d);

      /* u3x_qual():
      **
      **   Divide `a` as a quadruple `[b c d e]`.
      */
        void
        u3x_qual(u3_noun  a,
                   u3_noun* b,
                   u3_noun* c,
                   u3_noun* d,
                   u3_noun* e);

      /* u3x_quil():
      **
      **   Divide `a` as a quintuple `[b c d e f]`.
      */
        void
        u3x_quil(u3_noun  a,
                   u3_noun* b,
                   u3_noun* c,
                   u3_noun* d,
                   u3_noun* e,
                   u3_noun* f);
