/* include/f/meze.h
**
** This file is in the public domain.
*/
  /**  Nock-specific typedefs.
  **/
    /* u3_yes, u3_no, u3_nul;
    **
    **   Our Martian booleans and list terminator; empty string; not a noun.
    */
#     define u3_yes   0
#     define u3_no    1
#     define u3_nul   0
#     define u3_blip  0

    /* Tools for Martian booleans.
    */
#     define u3_so(x)      (u3_yes == (x))
#     define u3_ne(x)      (u3_no == (x))
#     define u3_say(x)     ( (x) ? u3_yes : u3_no )
#     define u3_not(x)     ( (x == u3_yes) ? u3_no : u3_yes )
#     define u3_and(x, y)  ( (u3_so(x) && u3_so(y)) ? u3_yes : u3_no )
#     define u3_or(x, y)   ( (u3_so(x) || u3_so(y)) ? u3_yes : u3_no )

#     define u3_assure(x)  if ( u3_ne(x) ) { u3_cm_bail(c3__fail); }
#     define u3_assent(x)  if ( u3_ne(x) ) { u3_cm_bail(c3__exit); }

    /* Word axis macros.  For 31-bit axes only.
    */
      /* u3_ax_dep(): number of axis bits.
      */
#       define u3_ax_dep(a_w)   (c3_bits_word(a_w) - 1)

      /* u3_ax_cap(): root axis, 2 or 3.
      */
#       define u3_ax_cap(a_w)   (0x2 | (a_w >> (u3_ax_dep(a_w) - 1)))

      /* u3_ax_mas(): remainder after cap.
      */
#       define u3_ax_mas(a_w) \
          ( (a_w & ~(1 << u3_ax_dep(a_w))) | (1 << (u3_ax_dep(a_w) - 1)) )

      /* u3_ax_peg(): connect two axes.
      */
#       define u3_ax_peg(a_w, b_w) \
          ( (a_w << u3_ax_dep(b_w)) | (b_w &~ (1 << u3_ax_dep(b_w))) )

    /* Conventional axes for gate call.
    */
#     define u3_cv_pay      3       //  payload
#     define u3_cv_sam      6       //  sample
#       define u3_cv_sam_1  6
#       define u3_cv_sam_2  12
#       define u3_cv_sam_3  13
#       define u3_cv_sam_4  24
#       define u3_cv_sam_5  25
#       define u3_cv_sam_6  26
#       define u3_cv_sam_12 52
#       define u3_cv_sam_13 53
#       define u3_cv_sam_7  27
#     define u3_cv_con      7       //  context
#     define u3_cv_con_2    14      //  context
#     define u3_cv_con_3    15      //  context
#     define u3_cv_con_sam  30      //  sample in gate context
#     define u3_cv_noc      2       //  deprecated
#     define u3_cv_bat      2       //  battery

  /** Aliases - selective and syntactically unique.
  **/
#   define u3h(som)          u3_cx_h(som)
#   define u3t(som)          u3_cx_t(som)
#   define u3at(axe, som)    u3_cx_at(axe, som)

#   define u3nc(a, b)        u3_ci_cell(a, b)
#   define u3nt(a, b, c)     u3_ci_trel(a, b, c)
#   define u3nq(a, b, c, d)  u3_ci_qual(a, b, c, d)

#   define u3du(som)         (u3_cr_du(som))
#   define u3ud(som)         (u3_cr_ud(som))

#   define u3k(som)          u3_ca_gain(som)
#   define u3z(som)          u3_ca_lose(som)

