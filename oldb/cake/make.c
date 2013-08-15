/* cake/make.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* u4_k_copy():
**
**   Unconditionally copy (noun) - for example, from road to road.
*/
u4_noun
u4_k_copy(u4_lane lane,
          u4_noun noun)
{
  if ( u4_n_atom(noun) ) {
    return u4_k_atom_copy(lane, noun);
  }
  else return u4_k_cell(lane, u4_k_copy(lane, u4_ch(noun)),
                              u4_k_copy(lane, u4_ct(noun)));
}

/* u4_k_safe():
**
**   Produce a version of (noun) that is safe on (lane).
**
**   XX: needs dag catcher.
*/
u4_noun 
u4_k_safe(u4_lane lane,
          u4_noun noun)
{
  if ( noun == u4_bull ) {
    return u4_bull;
  }
  else if ( u4_noun_is_cod(noun) ) {
    return noun;
  }
  else {
    u4_pin pin = noun;
    u4_bar bar = u4_pin_bar(pin);

    if ( u4_r_safe(lane, bar) ) {
      return noun;
    }
    else {
      if ( u4_pin_is_fort(pin) ) {
        /* Fortifications should be copied as well.
        */
        return u4_stub;
      } 
      else {
        if ( u4_pin_is_atom(noun) ) {
          return u4_k_atom_copy(lane, noun);
        }
        else {
          return u4_k_cell(lane, u4_ch(noun), u4_ct(noun));
        }
      }
    }
  }
}

/* _down_flub()::
*/
static void
_down_flub(u4_road road,
           u4_noun luc,
           u4_bar  bar_siv,
           u4_bar  bar_gad,
           u4_bar  bar_yal,
           u4_t    t_nax[])
{
  u4_xw  xw_pif = (bar_siv - bar_gad);

  if ( u4_n_cell(luc) ) {
    u4_assert(u4_noun_is_pin(luc));

    {
      u4_bar  bar_luc = u4_noun_bar(luc);
      u4_noun fes     = u4_bar_in(bar_luc, open_cell, head);
      u4_noun hoz     = u4_bar_in(bar_luc, open_cell, tail);

      if ( t_nax[bar_luc - (bar_siv + 1)] ) {
        return;
      }
      else t_nax[bar_luc - (bar_siv + 1)] = 1;

      if ( !u4_noun_is_cod(fes) ) {
        u4_bar bar_fes = u4_noun_bar(fes);

        if ( (u4_bar_pole(bar_fes) == u4_bar_pole(bar_gad)) &&
             (bar_fes > bar_siv) )
        {
          u4_bar_in(bar_luc, open_cell, head) = (fes - (xw_pif << 2));
          _down_flub(road, fes, bar_siv, bar_gad, bar_yal, t_nax);
        }
      }

      if ( !u4_noun_is_cod(hoz) ) {
        u4_bar bar_hoz = u4_noun_bar(hoz);

        if ( (u4_bar_pole(bar_hoz) == u4_bar_pole(bar_gad)) &&
             (bar_hoz > bar_siv) )
        {
          u4_bar_in(bar_luc, open_cell, tail) = (hoz - (xw_pif << 2));
          t_nax[bar_hoz - bar_siv] = 1;

          _down_flub(road, hoz, bar_siv, bar_gad, bar_yal, t_nax);
        }
      }
    }
  }
}

/* _down_move():: 
*/
static void
_down_move(u4_road road,
           u4_bar  bar_siv,
           u4_bar  bar_gad,
           u4_bar  bar_yal)
{
  u4_xw  xw_lam = (bar_yal - bar_siv);
  u4_xw  xw_i;

  for ( xw_i = 1; xw_i <= xw_lam; xw_i++ ) {
    u4_bar_xw(bar_gad + xw_i) = u4_bar_xw(bar_siv + xw_i);
  }
}

/* u4_k_clear():
**
**   Test that (luc) does not point into memory from (bar_gad)
**   to (bar_siv).
*/
u4_t
u4_k_clear(u4_road road,
           u4_noun luc,
           u4_bar  bar_siv,
           u4_bar  bar_gad)
{
  if ( u4_noun_is_cod(luc) ) {
    return 1;
  }
  else {
    u4_bar bar_luc = u4_noun_bar(luc);

    if ( u4_bar_pole(bar_luc) != u4_bar_pole(bar_gad) ) {
      return 1;
    }
    else if ( (bar_luc <= bar_siv) && (bar_luc > bar_gad) ) {
      return 0;
    }
    else return
      u4_n_atom(luc) ||
      ( u4_k_clear(road, u4_ch(luc), bar_siv, bar_gad) &&
        u4_k_clear(road, u4_ct(luc), bar_siv, bar_gad) );
  }
}

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
          u4_bar  bar_gad)
{
  u4_bar  bar_yal = u4_road_bar_cap(road);
  u4_xw   xw_pif  = (bar_siv - bar_gad);
  u4_xw   xw_lam  = (bar_yal - bar_siv);
  u4_noun puq;

  {
    u4_t   t_nax[xw_lam];
    u4_xw  xw_i;

    for ( xw_i = 0; xw_i < xw_lam; xw_i++ ) {
      t_nax[xw_i] = 0;
    }

    _down_flub(road, luc, bar_siv, bar_gad, bar_yal, t_nax);
    _down_move(road, bar_siv, bar_gad, bar_yal);

    u4_assert(u4_noun_is_pin(luc));
    puq = (luc - (xw_pif << 2));

    u4_road_bar_cap(road) -= xw_pif;
  }

  return puq;
}

/* u4_k_cell():
**
**   On (lane), write the cell (head tail).
*/
u4_noun
u4_k_cell(u4_lane lane,
          u4_noun head,
          u4_noun tail)
{
  u4_assert(head != u4_bull);
  u4_assert(tail != u4_bull);

  u4_assert(head != 3);
  u4_assert(tail != 3);

  head = u4_k_safe(lane, head);
  tail = u4_k_safe(lane, tail);
  {
    u4_bar bar = u4_r_take(lane, u4_wasp_sw(open_cell));
    u4_pin pin = u4_pin_make(bar, 0, 0);

    u4_pin_in(pin, open_cell, nub) = 0;
    u4_pin_in(pin, open_cell, head) = head;
    u4_pin_in(pin, open_cell, tail) = tail;

    return pin;
  }
}

/* u4_k_trel():
**
**   On (lane), write (a b c).
*/
u4_noun
u4_k_trel(u4_lane lane,
          u4_noun a,
          u4_noun b,
          u4_noun c)
{
  return u4_k_cell(lane, a, u4_k_cell(lane, b, c));
}

/* u4_k_qual():
**
**   On (lane), write (a b c d).
*/
u4_noun
u4_k_qual(u4_lane lane,
          u4_noun a,
          u4_noun b,
          u4_noun c,
          u4_noun d)
{
  return u4_k_cell(lane, a, u4_k_trel(lane, b, c, d));
}

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
          u4_noun e)
{
  return u4_k_cell(lane, a, u4_k_qual(lane, b, c, d, e));
}

/* u4_k_list():
**
**   On (lane), write a list terminated by u4_bull (C 0).
*/
u4_noun
u4_k_list(u4_lane lane, ...)
{
  u4_xw xw_len = 0;
  va_list ap;

  /* Count.
  */
  {
    va_start(ap, lane);
    while ( u4_bull != va_arg(ap, u4_noun) ) {
      xw_len++;
    }
    va_end(ap);
  }

  /* Allocate.
  */
  {
    u4_xw   xw_i;
    u4_noun yit[xw_len];

    va_start(ap, lane);
    for ( xw_i = 0; xw_i < xw_len; xw_i++ ) {
      yit[xw_i] = va_arg(ap, u4_noun);
    }
    va_end(ap);

    /* Construct.
    */
    {
      u4_noun woq = u4_noun_0;

      for ( xw_i = 0; xw_i < xw_len; xw_i++ ) {
        woq = u4_k_cell(lane, yit[xw_len - (xw_i + 1)], woq);
      }
      return woq;
    }
  }
}

/* u4_k_atom_copy():
**
**   On (lane), create a new atom which is a copy
**   of (atom).
*/
u4_noun
u4_k_atom_copy(u4_lane lane,
               u4_atom atom)
{
  if ( u4_noun_is_cod(atom) ) {
    return atom;
  }
  else {
    u4_sw  sw  = u4_a_bin(atom, 5);
    u4_bar bar = u4_r_take(lane, u4_wasp_sw(open_atom) + sw);
    u4_pin pin = u4_pin_make(bar, 0, 1);

    u4_pin_in(pin, open_atom, nub) = 0;
    u4_pin_in(pin, open_atom, sw) = sw;

    /* Fill the fscker.
    **
    ** Efficiency: many cases could be improved.
    */
    {
      u4_pw pw;

      for ( pw=0; pw < sw; pw++ ) {
        u4_open_atom_xw(pin, pw) = u4_a_word(atom, pw);
      }
    }

    return pin;
  }
}

/* u4_k_atom_gmp():
**
**   On (lane), write (mp) as an atom.  Return
**   a cod if it fits.
*/
u4_noun
u4_k_atom_gmp(u4_lane lane,
              mpz_t   mp) 
{
  /* C should really treat sizeof as a preprocessor constant.
  */
  if ( sizeof(mp_limb_t) != 4 ) return u4_trip;

  /* Efficiency: unnecessary copy.
  */
  {
    u4_sw sw = mpz_size(mp);
    u4_xw *xw = alloca(sw * 4);

    mpz_export(xw, 0, -1, 4, 0, 0, mp);
    mpz_clear(mp);

    return u4_k_atom_sw(lane, xw, sw);
  }
}

/* u4_k_atom_xw():
**
**   On (lane), write (xw) as an atom.  Return
**   a cod if it fits.
*/
u4_noun
u4_k_atom_xw(u4_lane lane,
             u4_xw   xw)
{
  return u4_k_atom_sw(lane, &xw, 1);
}

/* u4_k_atom_xd():
**
**   On (lane), write (xw) as an atom.  Return
**   a cod if it fits.
*/
u4_noun
u4_k_atom_xd(u4_lane lane,
             u4_xd   xd)
{
  u4_xw xw[2];

  xw[0] = (u4_xw)xd;
  xw[1] = (xd >> 32ULL);

  return u4_k_atom_sw(lane, xw, 2);
}

/* u4_k_atom_sw():
**
**   On (lane), write (xw, sw) as an atom.  Return
**   a cod if it fits.
*/
u4_noun
u4_k_atom_sw(u4_lane     lane,
             const u4_xw *xw,
             u4_sw       sw)
{
  /* Strip trailing zeroes.
  */
  while ( sw ) {
    if ( xw[sw - 1] ) {
      break;
    }
    sw--;
  }

  /* Check for cod.
  */
  if ( !sw ) {
    return u4_noun_0;
  }
  else if ( (sw == 1) && !(xw[0] & (1 << 31)) ) {
    return u4_cod_in(xw[0]);
  }

  /* Allocate, fill, return.
  */
  {
    u4_bar bar = u4_r_take(lane, u4_wasp_sw(open_atom) + sw);
    u4_pin pin = u4_pin_make(bar, 0, 1);

    u4_pin_in(pin, open_atom, nub) = 0;
    u4_pin_in(pin, open_atom, sw) = sw;

    /* Fill the fscker.
    **
    ** Efficiency: many cases could be improved.
    */
    {
      u4_pw pw;

      for ( pw=0; pw < sw; pw++ ) {
        u4_open_atom_xw(pin, pw) = xw[pw];
      }
    }
    return pin;
  }
}

/* u4_k_atom_sb():
**
**   On (lane), write (xb, sb) as an atom.  Return
**   a cod if it fits.
*/
u4_noun
u4_k_atom_sb(u4_lane     lane,
             const u4_xb *xb,
             u4_sb       sb)
{
  /* Strip trailing zeroes.
  */
  while ( sb ) {
    if ( xb[sb - 1] ) {
      break;
    }
    sb--;
  }

  /* Check for cod.
  */
  if ( sb <= 4 ) {
    if ( !sb ) {
      return u4_noun_0;
    }
    else if ( sb == 1 ) {
      return u4_cod_in(xb[0]);
    }
    else if ( sb == 2 ) {
      return u4_cod_in(xb[0] | (xb[1] << 8));
    }
    else if ( sb == 3 ) {
      return u4_cod_in(xb[0] | (xb[1] << 8) 
                             | (xb[2] << 16));
    }
    else if ( (xb[3] <= 0x7f) ) {
      return u4_cod_in(xb[0] | (xb[1] << 8) 
                             | (xb[2] << 16) 
                             | (xb[3] << 24));
    }
  }

  /* Allocate, fill, return.
  */
  {
    u4_sw  sw  = u4_bblock(sb, 2);
    u4_bar bar = u4_r_take(lane, u4_wasp_sw(open_atom) + sw);
    u4_pin pin = u4_pin_make(bar, 0, 1);

    u4_pin_in(pin, open_atom, nub) = 0;
    u4_pin_in(pin, open_atom, sw) = sw;

    /* Clear the words.
    */
    {
      u4_pw pw;

      for ( pw=0; pw < sw; pw++ ) {
        u4_open_atom_xw(pin, pw) = 0;
      }
    }

    /* Fill the bytes.
    */
    {
      u4_pb pb;

      for ( pb=0; pb < sb; pb++ ) {
        u4_pw  pw  = (pb >> 2);
        u4_hbw hbw = (pb & 3) << 3;

        u4_open_atom_xw(pin, pw) |= xb[pb] << hbw;
      }
    }
    return pin;
  }
} 

/* u4_k_atom_c():
**
**   On (lane), write (c) as an atom with LSB first.
*/
u4_noun
u4_k_atom_c(u4_lane    lane,
            const u4_c *c)
{
  return u4_k_atom_sb(lane, (u4_xb *)c, strlen(c));
}

/* u4_k_atom_log():
**
**   On (lane), write (log), a list of bytes, as a string LSB first.
*/
u4_noun
u4_k_atom_log(u4_lane lane,
              u4_noun log)
{
  u4_sb sb = u4_log_len(log);
  u4_pb pb = 0;
  u4_xb xb[sb];

  while ( !u4_n_zero(log) ) {
    xb[pb++] = u4_a_byte(u4_ch(log), 0);
    log = u4_ct(log);
  }
  return u4_k_atom_sb(lane, xb, sb);
}

/* u4_k_atom_decimal():
**
**   On (lane), write (log), a list of digits, as a decimal.
*/
u4_noun
u4_k_atom_decimal(u4_lane lane,
                  u4_noun log)
{
  mpz_t mp;

  mpz_init(mp);
  while ( !u4_n_zero(log) ) {
    u4_xb xb = u4_a_byte(u4_ch(log), 0);

    mpz_mul_ui(mp, mp, 10);
    mpz_add_ui(mp, mp, (xb - '0'));

    log = u4_ct(log);
  }
  return u4_k_atom_gmp(lane, mp);
}

/* u4_k_atom_heximal():
**
**   On (lane), write (log), a list of digits, as a hexadecimal.
*/
u4_noun
u4_k_atom_heximal(u4_lane lane,
                  u4_noun log)
{
  mpz_t mp;

  mpz_init(mp);
  while ( !u4_n_zero(log) ) {
    u4_xb xb = u4_a_byte(u4_ch(log), 0);

    mpz_mul_ui(mp, mp, 16);
    if ( (xb >= 'a') && (xb <= 'f') ) {
      mpz_add_ui(mp, mp, (xb + 10 - 'a'));
    }
    else {
      mpz_add_ui(mp, mp, (xb - '0'));
    }
    log = u4_ct(log);
  }
  return u4_k_atom_gmp(lane, mp);
}

/* u4_k_atom_cat():
**
**   On (lane), byte-concatenate (fuz) and (byr).
*/
u4_atom
u4_k_atom_cat(u4_lane lane,
              u4_atom fuz,
              u4_atom byr)
{
  u4_sb sb_fuz = u4_a_bin(fuz, 3);
  u4_sb sb_byr = u4_a_bin(byr, 3);
  u4_sb sb_cat = (sb_fuz + sb_byr);
  {
    u4_xb *xb_buf = alloca(sb_cat);

    u4_a_bytes(fuz, xb_buf, 0, sb_fuz);
    u4_a_bytes(byr, xb_buf + sb_fuz, 0, sb_byr);

    return u4_k_atom_sb(lane, xb_buf, sb_cat);
  }
}
