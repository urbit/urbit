/* include/f/meze.h
**
** This file is in the public domain.
*/
  /**  Nock-specific typedefs.
  **/
    /* u2_yes, u2_no, u2_nul;
    **
    **   Our Martian booleans and list terminator; empty string; not a noun.
    */
#     define u2_yes   0
#     define u2_no    1
#     define u2_nul   0
#     define u2_blip  0

    /* Tools for Martian booleans.
    */
#     define u2_so(x)      (u2_yes == (x))
#     define u2_ne(x)      (u2_no == (x))
#     define u2_say(x)     ( (x) ? u2_yes : u2_no )
#     define u2_not(x)     ( (x == u2_yes) ? u2_no : u2_yes )
#     define u2_and(x, y)  ( (u2_so(x) && u2_so(y)) ? u2_yes : u2_no )
#     define u2_or(x, y)   ( (u2_so(x) || u2_so(y)) ? u2_yes : u2_no )

#     define u2_assure(x)  if ( u2_ne(x) ) { u2_cm_bail(c3__fail); }
#     define u2_assent(x)  if ( u2_ne(x) ) { u2_cm_bail(c3__exit); }

    /* Word axis macros.  For 31-bit axes only.
    */
      /* u2_ax_dep(): number of axis bits.
      */
#       define u2_ax_dep(a_w)   (c3_bits_word(a_w) - 1)

      /* u2_ax_cap(): root axis, 2 or 3.
      */
#       define u2_ax_cap(a_w)   (0x2 | (a_w >> (u2_ax_dep(a_w) - 1)))

      /* u2_ax_mas(): remainder after cap.
      */
#       define u2_ax_mas(a_w) \
          ( (a_w & ~(1 << u2_ax_dep(a_w))) | (1 << (u2_ax_dep(a_w) - 1)) )

      /* u2_ax_peg(): connect two axes.
      */
#       define u2_ax_peg(a_w, b_w) \
          ( (a_w << u2_ax_dep(b_w)) | (b_w &~ (1 << u2_ax_dep(b_w))) )

    /* Conventional axes for gate call.
    */
#     define u2_cv_pay      3       //  payload
#     define u2_cv_sam      6       //  sample
#       define u2_cv_sam_1  6
#       define u2_cv_sam_2  12
#       define u2_cv_sam_3  13
#       define u2_cv_sam_4  24
#       define u2_cv_sam_5  25
#       define u2_cv_sam_6  26
#       define u2_cv_sam_12 52
#       define u2_cv_sam_13 53
#       define u2_cv_sam_7  27
#     define u2_cv_con      7       //  context
#     define u2_cv_con_2    14      //  context
#     define u2_cv_con_3    15      //  context
#     define u2_cv_con_sam  30      //  sample in gate context
#     define u2_cv_noc      2       //  deprecated
#     define u2_cv_bat      2       //  battery

  /** Aliases - selective and syntactically unique.
  **/
#   define u2h(som)          u2_cx_h(som)
#   define u2t(som)          u2_cx_t(som)
#   define u2at(axe, som)    u2_cx_at(axe, som)

#   define u2nc(a, b)        u2_ci_cell(a, b)
#   define u2nt(a, b, c)     u2_ci_trel(a, b, c)
#   define u2nq(a, b, c, d)  u2_ci_qual(a, b, c, d)

#   define u2du(som)         (u2_cr_du(som))
#   define u2ud(som)         (u2_cr_ud(som))

#   define u2k(som)          u2_ca_gain(som)
#   define u2z(som)          u2_ca_lose(som)

