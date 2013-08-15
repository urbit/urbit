/* include/cake/make.h
**
** This file is in the public domain.
*/
  /** Functions.
  **/
    /** Weak constructors ("k").
    **/
      /* u4_k_copy():
      **
      **   Unconditionally copy (noun) - for example, from road to road.
      */
        u4_noun
        u4_k_copy(u4_lane lane,
                  u4_noun noun);

      /* u4_k_safe():
      **
      **   Produce a version of (noun) that is safe on (lane).
      */
        u4_noun
        u4_k_safe(u4_lane lane,
                  u4_noun noun);

      /* u4_k_clear():
      **
      **   Test that (luc) does not point into memory from (bar_gad)
      **   to (bar_siv).
      */
        u4_t
        u4_k_clear(u4_road road,
                   u4_noun luc,
                   u4_bar  bar_siv,
                   u4_bar  bar_gad);

      /* u4_k_tamp():
      **
      **   Compact the can of (road), eliding the segment from
      **   (bar_gad) to (bar_siv), moving (luc).
      **
      **   u4_k_clear(road, luc, bar_siv, bar_gad) must be true.
      */
        u4_noun
        u4_k_tamp(u4_road road,
                  u4_noun luc,
                  u4_bar  bar_siv,
                  u4_bar  bar_gad);

      /* u4_k_cell():
      **
      **   On (lane), write the cell (head tail).
      */
        u4_noun
        u4_k_cell(u4_lane lane,
                  u4_noun head,
                  u4_noun tail);

      /* u4_k_trel():
      **
      **   On (lane), write (a b c).
      */
        u4_noun
        u4_k_trel(u4_lane lane,
                  u4_noun a,
                  u4_noun b,
                  u4_noun c);

      /* u4_k_qual():
      **
      **   On (lane), write (a b c d).
      */
        u4_noun
        u4_k_qual(u4_lane lane,
                  u4_noun a,
                  u4_noun b,
                  u4_noun c,
                  u4_noun d);

      /* u4_k_quil():
      **
      **   On (lane), write (a b c d e).
      */
        u4_noun
        u4_k_quil(u4_lane lane,
                  u4_noun a,
                  u4_noun b,
                  u4_noun c,
                  u4_noun d,
                  u4_noun e);

      /* u4_k_list():
      **
      **   On (lane), write a list terminated by u4_bull.
      */
        u4_noun
        u4_k_list(u4_lane lane, ...);

      /* u4_k_atom_copy():
      **
      **   On (lane), create a new atom which is a copy
      **   of (atom).
      */
        u4_noun
        u4_k_atom_copy(u4_lane lane,
                       u4_atom atom);

      /* u4_k_atom_gmp():
      **
      **   On (lane), write (mp) as an atom.  Return
      **   a cod if it fits.
      */
        u4_noun
        u4_k_atom_gmp(u4_lane lane,
                      mpz_t   mp); 

      /* u4_k_atom_xw():
      **
      **   On (lane), write (xw) as an atom.  Return
      **   a cod if it fits.
      */
        u4_noun
        u4_k_atom_xw(u4_lane lane,
                     u4_xw   xw);

      /* u4_k_atom_xd():
      **
      **   On (lane), write (xd) as an atom.  Return
      **   a cod if it fits.
      */
        u4_noun
        u4_k_atom_xd(u4_lane lane,
                     u4_xd   xd);

      /* u4_k_atom_sw():
      **
      **   On (lane), write (kw, sw) as an atom.  Return
      **   a cod if it fits.
      */
        u4_noun
        u4_k_atom_sw(u4_lane     lane,
                     const u4_xw *xw,
                     u4_sw       sw);

      /* u4_k_atom_sb():
      **
      **   On (lane), write (xb, sb) as an atom.  Return
      **   a cod if it fits.
      */
        u4_noun
        u4_k_atom_sb(u4_lane     lane,
                     const u4_xb *xb,
                     u4_sb       sb);

      /* u4_k_atom_c():
      **
      **   On (lane), write (c) as an atom with LSB first.
      */
        u4_noun
        u4_k_atom_c(u4_lane    lane,
                    const u4_c *c);

      /* u4_k_atom_log():
      **
      **   On (lane), write (log), a list of bytes, as a string LSB first.
      */
        u4_noun
        u4_k_atom_log(u4_lane lane,
                      u4_noun log);

      /* u4_k_atom_decimal():
      **
      **   On (lane), write (log), a list of digits, as a decimal.
      */
        u4_noun
        u4_k_atom_decimal(u4_lane lane,
                          u4_noun log);

      /* u4_k_atom_heximal():
      **
      **   On (lane), write (log), a list of digits, as a hexadecimal.
      */
        u4_noun
        u4_k_atom_heximal(u4_lane lane,
                          u4_noun log);

      /* u4_k_atom_cat():
      **
      **   On (lane), byte-concatenate (fuz) and (byr).
      */
        u4_atom
        u4_k_atom_cat(u4_lane lane,
                      u4_noun fuz,
                      u4_noun byr);
