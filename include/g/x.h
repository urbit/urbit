/* include/g/x.h
**
** This file is in the public domain.
*/
    /** u3_cx_*: read, but bail with c3__exit on a crash.
    **/
#if 1
#     define u3_cx_h(som)  u3_ca_h(som)
#     define u3_cx_t(som)  u3_ca_t(som)
#else
      /* u3_cx_h (u3h): head.
      */
        u3_noun
        u3_cx_h(u3_noun som);

      /* u3_cx_t (u3t): tail.
      */
        u3_noun
        u3_cx_t(u3_noun som);
#endif
      /* u3_cx_good(): test for u3_none.
      */
        u3_noun
        u3_cx_good(u3_weak som);

      /* u3_cx_at (u3at): fragment.
      */
        u3_noun
        u3_cx_at(u3_noun axe, u3_noun som);

      /* u3_cx_cell():
      **
      **   Divide `a` as a cell `[b c]`.
      */
        void
        u3_cx_cell(u3_noun  a,
                   u3_noun* b,
                   u3_noun* c);

      /* u3_cx_trel():
      **
      **   Divide `a` as a trel `[b c d]`, or bail.
      */
        void
        u3_cx_trel(u3_noun  a,
                   u3_noun* b,
                   u3_noun* c,
                   u3_noun* d);

      /* u3_cx_qual():
      **
      **   Divide `a` as a quadruple `[b c d e]`.
      */
        void
        u3_cx_qual(u3_noun  a,
                   u3_noun* b,
                   u3_noun* c,
                   u3_noun* d,
                   u3_noun* e);

      /* u3_cx_quil():
      **
      **   Divide `a` as a quintuple `[b c d e f]`.
      */
        void
        u3_cx_quil(u3_noun  a,
                   u3_noun* b,
                   u3_noun* c,
                   u3_noun* d,
                   u3_noun* e,
                   u3_noun* f);
