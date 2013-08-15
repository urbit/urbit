/* include/cake/noun.h
**
** This file is in the public domain.
*/
  /** Data types.
  **/
    /* A nopt - a noun that may be bull.
    **
    ** Bull always has the C value 0.  Its semantics may vary
    ** according to use, but it often implies a motor stop.
    */
      typedef u4_xw u4_nopt;

    /* A noun.
    **
    ** If bit 31 is 1, bits 0-30 are a 31-bit atom (unsigned integer).
    ** In this case, the noun is a "cod."
    **
    ** If bit 31 is 0, bits 0-30 are a pin.  See pin.h.
    */
      typedef u4_xw u4_noun;
      typedef u4_noun u4_cod;

    /* Like all u4 C types, these are enforced by assertion.
    */
      typedef u4_noun u4_atom;
      typedef u4_noun u4_flag;
      typedef u4_noun u4_cell;

    /* Tuples up to 7.
    */
      typedef u4_noun u4_trel;
      typedef u4_noun u4_qual;
      typedef u4_noun u4_quil;
      typedef u4_noun u4_stil;
      typedef u4_noun u4_spel;

    /* A nub.
    **
    ** This is a 31-bit insecure hash.  If the value is 0, it means
    ** the nub has not yet been computed.
    */
      typedef u4_xw u4_nub;


  /** Macros.
  **/
    /* Bull.
    **
    ** Good style in testing for bull is ( u4_bull == [expr] ).
    */
#     define u4_bull \
        ((u4_nopt) 0)

    /* Noun discrimination.
    */
#     define u4_noun_is_cod(noun) \
        ( 0 != ((noun) & (1 << 31)) )
#     define u4_noun_is_pin(noun) \
        ( 0 == ((noun) & (1 << 31)) )
    
    /* Cod conversion.
    */
#     define u4_cod_out(cod) \
        ( (cod) &~ (1 << 31) )
#     define u4_cod_in(xw) \
        ( ((xw) & (1 << 31)) ? (*(int *)0) : ((xw) | (1 << 31)) )

#     define u4_noun_pin(noun)    (noun)
#     define u4_noun_bar(noun)    u4_pin_bar(u4_noun_pin(noun))

#     define u4_noun_make_bar(bar)  u4_

#     define u4_noun_0  u4_cod_in(0)
#     define u4_noun_1  u4_cod_in(1)
#     define u4_noun_2  u4_cod_in(2)
#     define u4_noun_3  u4_cod_in(3)
#     define u4_noun_4  u4_cod_in(4)
#     define u4_noun_5  u4_cod_in(5)
#     define u4_noun_6  u4_cod_in(6)
#     define u4_noun_7  u4_cod_in(7)
#     define u4_noun_8  u4_cod_in(8)
#     define u4_noun_9  u4_cod_in(9)
#     define u4_noun_10 u4_cod_in(10)

