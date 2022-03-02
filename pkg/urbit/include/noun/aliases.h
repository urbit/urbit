#ifndef U3_ALIASES_H
#define U3_ALIASES_H

  /**  Constants.
  **/
    /* u3_nul: 0, hoon ~.
    */
#     define u3_nul   0

    /* u3_blip: 0, hoon %$.
    */
#     define u3_blip  0


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


    /* u3nl(), u3_none-terminated varargs list
    */
#     define u3nl              u3i_list

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


#endif /* ifndef U3_ALIASES_H */
