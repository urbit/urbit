/* i/n/u.h
**
** This file is in the public domain.
*/
  /**  Constants.
  **/
    /* u3_none - u3_noun which is not a noun.
    */
#     define u3_none  (u3_noun)0xffffffff

    /* u3_nul: 0, hoon ~.
    */
#     define u3_nul   0

    /* u3_blip: 0, hoon %$.
    */
#     define u3_blip  0


  /**  Typedefs.
  **/
    /* u3_post: pointer offset into u3_Loom; _p suffix; declare as u3p().
    */
      typedef c3_w      u3_post;
#     define u3p(type)  u3_post

    /* u3_noun: tagged noun pointer.
    **
    **  If bit 31 is 0, a u3_noun is a direct 31-bit atom ("cat").
    **  If bit 31 is 1 and bit 30 0, an indirect atom ("pug").
    **  If bit 31 is 1 and bit 30 1, an indirect cell ("pom").
    **
    ** Bits 0-29 are a word offset against u3_Loom (u3_post).
    */
      typedef c3_d u3_noun;

    /* u3_weak: u3_noun which may be u3_none (not a noun).
    */
      typedef u3_noun u3_weak;

    /* u3_atom: u3_noun which must be an atom.
    */
      typedef u3_noun u3_atom;

    /* u3_term: u3_noun which must be a term (@tas).
    */
      typedef u3_noun u3_term;

    /* u3_cell, u3_trel, u3_qual, u3_quin: cell, triple, quadruple, quintuple.
    */
      typedef u3_noun u3_cell;
      typedef u3_noun u3_trel;
      typedef u3_noun u3_qual;
      typedef u3_noun u3_quin;

    /* u3_funk, u3_funq: unary and binary noun functions.
    */
      typedef u3_noun (*u3_funk)(u3_noun);
      typedef u3_noun (*u3_funq)(u3_noun, u3_noun);


  /**  Macros.
  **/
    /* u3_assure(): loobean assert, bailing with %fail.
    */
#     define u3_assure(x)  if ( !_(x) ) { u3m_bail(c3__fail); }

    /* u3_assert(): loobean assert, bailing with %exit.
    */
#     define u3_assent(x)  if ( !_(x) ) { u3m_bail(c3__exit); }


  /**  Aliases.
  **/
    /* u3h(), u3t(), u3at(): noun fragments.
    */
#     define u3h(som)          u3x_h(som)
#     define u3t(som)          u3x_t(som)
#     define u3at(axe, som)    u3x_at(axe, som)

    /* u3nc(), u3nt(), u3nq(): tuple composition.
    */
#     define u3nc(a, b)        u3i_cell(a, b)
#     define u3nt(a, b, c)     u3i_trel(a, b, c)
#     define u3nq(a, b, c, d)  u3i_qual(a, b, c, d)

    /* u3du(), u3ud(): noun/cell test.
    */
#     define u3du(som)         (u3r_du(som))
#     define u3ud(som)         (u3r_ud(som))

    /* u3k(), u3z(): reference counts.
    */
#     define u3k(som)          u3a_gain(som)
#     define u3z(som)          u3a_lose(som)

    /* u3do(), u3dc(), u3dt(), u3dq(): arvo calls.
    */
#     define  u3do(txt_c, arg)         u3v_do(txt_c, arg)
#     define  u3dc(txt_c, a, b)        u3v_do(txt_c, u3nc(a, b))
#     define  u3dt(txt_c, a, b, c)     u3v_do(txt_c, u3nt(a, b, c))
#     define  u3dq(txt_c, a, b, c, d)  u3v_do(txt_c, u3nq(a, b, c, d))

    /* u3to(), u3of(): offset/pointer conversion.
    */
#     define  u3to(type, x) ((type *) u3a_into(x))
#     define  u3tn(type, x) (x == 0) ? (void *)0 :  ((type *) u3a_into(x))
#     define  u3of(type, x) (u3a_outa((type *)x))

