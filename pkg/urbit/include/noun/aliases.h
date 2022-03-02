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
    /* u3to(), u3of(): offset/pointer conversion.
    */
#     define  u3to(type, x) ((type *) u3a_into(x))
#     define  u3tn(type, x) (x == 0) ? (void *)0 :  ((type *) u3a_into(x))
#     define  u3of(type, x) (u3a_outa((type *)x))


#endif /* ifndef U3_ALIASES_H */
