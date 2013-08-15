/* include/fake/op.h
**
** This file is in the public domain.
*/
  /** Functions.
  **/
    /* u4_op_add(): 
    **
    **   Produce the sum of (a) and (b).
    */
      u4_atom
      u4_op_add(u4_lane lane,
                u4_atom a,
                u4_atom b);

    /* u4_op_con():
    **
    **   Produce (a | b).
    */
      u4_atom
      u4_op_con(u4_lane lane,
                u4_atom a,
                u4_atom b);

    /* u4_op_cut():
    **
    **   Produce (c >> b) &~ (1 << a).
    */
      u4_atom
      u4_op_cut(u4_lane lane,
                u4_atom a,
                u4_atom b,
                u4_atom c);

    /* u4_op_cutb():
    **
    **   Produce (d >> (c << a) &~ (1 << (b << a))).
    */
      u4_atom
      u4_op_cutb(u4_lane lane,
                 u4_atom a,
                 u4_atom b,
                 u4_atom c,
                 u4_atom d);

    /* u4_op_dec():
    **
    **   Produce (atom - 1), or bull if (atom) is 0.
    */
      u4_nopt
      u4_op_dec(u4_lane lane,
                u4_atom atom);

    /* u4_op_dis():
    **
    **   Produce (a & b).
    */
      u4_atom
      u4_op_dis(u4_lane lane,
                u4_atom a,
                u4_atom b);

    /* u4_op_div():
    **
    **   Produce (b / a), or bull if (a) is 0.
    */
      u4_nopt
      u4_op_div(u4_lane lane,
                u4_atom a,
                u4_atom b);

    /* u4_op_end():
    **
    **   Produce (b &~ (1 << a)).
    */
      u4_atom
      u4_op_end(u4_lane lane,
                u4_atom a, 
                u4_atom b);

    /* u4_op_endb():
    **
    **   Produce (c &~ (1 << (b << a))).
    */
      u4_atom
      u4_op_endb(u4_lane lane,
                 u4_atom a, 
                 u4_atom b,
                 u4_atom c);

    /* u4_op_get():
    **
    **   Produce the twig of (noun) at (twig), or bull.
    */
      u4_nopt
      u4_op_get(u4_twig twig,
                u4_noun noun);

    /* u4_op_inc():
    **
    **   Produce (atom + 1).
    */
      u4_atom
      u4_op_inc(u4_lane lane,
                u4_atom atom);

    /* u4_op_log():
    **
    **   Produce the lowest m_log such that (1 << m_log) > m.
    */
      u4_atom
      u4_op_log(u4_lane lane,
                u4_atom atom);

    /* u4_op_lsh():
    **
    **   Produce (b << a).
    */
      u4_atom
      u4_op_lsh(u4_lane lane,
                u4_atom a,
                u4_atom b);

    /* u4_op_lshb():
    **
    **   Produce (c >> (b << a)).
    */
      u4_atom
      u4_op_lshb(u4_lane lane,
                 u4_atom a,
                 u4_atom b,
                 u4_atom c);

    /* u4_op_mix():
    **
    **   Produce (a ^ b).
    */
      u4_atom
      u4_op_mix(u4_lane lane,
                u4_atom a,
                u4_atom b);

    /* u4_op_mod():
    **
    **   Produce (b % a).
    */
      u4_atom
      u4_op_mod(u4_lane lane,
                u4_atom a,
                u4_atom b);

    /* u4_op_mul():
    **
    **   Produce (a * b).
    */
      u4_atom
      u4_op_mul(u4_lane lane,
                u4_atom a,
                u4_atom b);

    /* u4_op_ord():
    **
    **   Produce 0 if (a < b), 1 if (a > b), bull if (a == b).
    */
      u4_nopt
      u4_op_ord(u4_noun a,
                u4_noun b);

    /* u4_op_pan():
    **
    **   Factor out the shared root of (twig_a twig_b).  The result is 
    **   either (1 twig) or (twig 1), or bull if they diverge.
    */
      u4_nopt
      u4_op_pan(u4_lane lane,
                u4_twig twig_a,
                u4_twig twig_b);
    
    /* u4_op_peg():
    **
    **   Concatenate (twig_a) above (twig_b).
    */
      u4_twig
      u4_op_peg(u4_lane lane,
                u4_twig twig_a,
                u4_twig twig_b);

    /* u4_op_put():
    **
    **   Insert (a) at (twig) in (b).  Return bull if there is no
    **   such twig.
    */
      u4_nopt
      u4_op_put(u4_lane lane,
                u4_twig twig,
                u4_noun a,
                u4_noun b);

    /* u4_op_rsh():
    **
    **   Produce (b >> a).
    */
      u4_atom
      u4_op_rsh(u4_lane lane,
                u4_atom a,
                u4_atom b);

    /* u4_op_rshb():
    **
    **   Produce (c >> (b << a)).
    */
      u4_atom
      u4_op_rshb(u4_lane lane,
                 u4_atom a,
                 u4_atom b,
                 u4_atom c);

    /* u4_op_sub():
    **
    **   Produce (b - a), or bull if (a > b).
    */
      u4_nopt
      u4_op_sub(u4_lane lane,
                u4_atom a,
                u4_atom b);

    /* u4_op_tap():
    **
    **   Produce (twig) with the root bit removed, or bull if (twig) is 1.
    */
      u4_nopt
      u4_op_tap(u4_lane lane,
                u4_twig twig);

    /* u4_op_tip():
    **
    **   Produce the root of (twig) - 2 or 3; or bull if (twig) is 1.
    */
      u4_nopt
      u4_op_tip(u4_twig twig);
