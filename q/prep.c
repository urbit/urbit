/* j/prep.c
**
** This file is in the public domain.
*/
#include "all.h"
#include <stdio.h>

/** Warning: this file contains old code which does not comply
*** with current coding conventions.
**/

/* Return true iff (atom) is an ASCII string of (xb) or more bytes,
** using no characters besides a-z and -.
*/
static c3_t
_test_term(u2_ray  wir_r,
           u2_atom atom,
           c3_y   xb)
{
  c3_w sb = u2_met(3, atom);

  if ( sb >= xb) {
    c3_y *xb = alloca(sb);
    c3_w i;

    u2_bytes(0, sb, xb, atom);

    for ( i=0; i < sb; i++ ) {
      if ( ((xb[i] < 'a') || (xb[i] > 'z')) && (xb[i] != '-') ) {
        return 0;
      }
    }
    return 1;
  }
  else return 0;
}

/* u2_fj_prep_textual():
**
**   Prep with a text bias; fall back to decimal.
*/
u2_prep
u2_fj_prep_textual(u2_ray  wir_r,
                u2_atom atom)
{
  if ( _test_term(wir_r, atom, 1) ) {
    return atom;
  }
  else return u2_fj_prep_decimal(wir_r, atom);
}

/* u2_fj_prep_decimal():
**
**   Prep a decimal value.
*/
u2_prep
u2_fj_prep_decimal(u2_ray  wir_r,
                u2_atom atom)
{
  c3_c *ca;
  mpz_t mp;
  u2_noun text;

  u2_mp(mp, atom);
  ca = mpz_get_str(0, 10, mp);

  text = u2_bn_string(wir_r, ca);

  free(ca);
  mpz_clear(mp);

  return text;
}

/* u2_fj_prep_heximal():
**
**   Prep a hexadecimal value, with 0x.
*/
u2_prep
u2_fj_prep_heximal(u2_ray  wir_r,
                   u2_atom atom)
{
  c3_w gyf_w = u2_met(2, atom);
  c3_w sb_hex = (gyf_w ? gyf_w : 1) + 2;
  c3_c *ca = malloc(sb_hex + 1);
  mpz_t mp;
  u2_noun text;

  u2_mp(mp, atom);

  ca[0] = '0';
  ca[1] = 'x';
  mpz_get_str(ca + 2, 16, mp);

  text = u2_bn_string(wir_r, ca);
  free(ca);
  mpz_clear(mp);

  return text;
}

/* u2_fj_prep_hexinal():
**
**   Prep a heximal value, without 0x.
*/
u2_prep
u2_fj_prep_hexinal(u2_ray  wir_r,
                u2_atom atom)
{
  c3_w gyf_w = u2_met(2, atom);
  c3_w sb_hex = (gyf_w ? gyf_w : 1);
  c3_c *ca = malloc(sb_hex + 1);
  mpz_t mp;
  u2_noun text;

  u2_mp(mp, atom);

  mpz_get_str(ca, 16, mp);

  text = u2_bn_string(wir_r, ca);
  free(ca);
  mpz_clear(mp);

  return text;
}

/* Convert (atom) to string text - by guessing.  Use only as last resort.
*/
static u2_noun
_prep_atom(u2_ray  wir_r, u2_atom atom)
{
  c3_w sb;

  switch ( (sb = u2_met(3, atom)) ) {
    case 0: return ('0');
    case 1: return u2_fj_prep_decimal(wir_r, atom);

    default: {
      if ( _test_term(wir_r, atom, 2) ) {
        return atom;
      }
      else {
        if ( sb > 2 ) {
          return u2_fj_prep_heximal(wir_r, atom);
        }
        else return u2_fj_prep_decimal(wir_r, atom);
      }
    }
  }
}

/* Convert (cell) to a list of preps.
*/
static u2_noun
_prep_tuple(u2_ray  wir_r, u2_noun cell)
{
  u2_noun head = u2_h(cell);
  u2_noun tail = u2_t(cell);

  if ( u2_yes == u2_dust(tail) ) {
    return u2_bc(wir_r, u2_fj_prep_noun(wir_r, head),
                           _prep_tuple(wir_r, tail));
  }
  else {
    u2_prep prep_head = u2_fj_prep_noun(wir_r, head);
    u2_prep prep_tail = u2_fj_prep_noun(wir_r, tail);
 
    return u2_bt(wir_r, prep_head, prep_tail, u2_nul);
  }
}

/* Convert (cell) to a prep.
*/
static u2_noun
_prep_cell(u2_ray  wir_r, 
           u2_noun cell)
{
  return u2_bc
    (wir_r, c3__nail,
            u2_bq
             (wir_r,
             ('['),
             u2_bc(wir_r, c3__glue, _prep_tuple(wir_r, cell)),
             (']'),
             u2_nul));
}

/* u2_fj_prep_noun():
**
**   Convert (noun) to a prep, which is
**
**      (text)
**    | (.glue *prep)
**    | (.nail *prep)
*/
u2_prep
u2_fj_prep_noun(u2_ray  wir_r,
             u2_noun noun)
{
  if ( u2_yes == u2_stud(noun) ) {
    return _prep_atom(wir_r, noun);
  } else {
    return _prep_cell(wir_r, noun);
  }
}

/* u2_fj_prep_close():
**
**   Prep a list of preps, in (xb_a, xb_b).
*/
u2_prep
u2_fj_prep_close(u2_ray  wir_r,
              c3_y   xb_a,
              c3_y   xb_b,
              u2_list  gah)
{
  return u2_bc
    (wir_r, 
     c3__nail,
     u2_bq(wir_r,
           (xb_a),
           u2_bc(wir_r, c3__glue, gah),
           (xb_b),
           u2_nul));
}


/* Prepend (_xw_tab) spaces to (text).
*/
static u2_noun
_c_tab(u2_ray wir_r, 
       c3_w _xw_tab,
       u2_noun  text) 
{
  u2_atom text_pro;

  if ( !_xw_tab ) {
    text_pro = text;
  } else {
    c3_w i;
    mpz_t mp;

    u2_mp(mp, text);

    for ( i=0; i < _xw_tab; i++ ) {
      mpz_mul_2exp(mp, mp, 8);
      mpz_add_ui(mp, mp, 32);
    }
    text_pro = u2_bn_mp(wir_r, mp);
  }

  return text_pro;
}

/* Glue (_n_htext) to (_n_ttext).
*/
static u2_noun 
_c_cat(u2_ray wir_r,
       u2_noun  _n_htext, 
       u2_noun  _n_ttext)
{
  u2_noun n_a, n_b;

  n_a = u2_met(3, _n_htext) * 8;
  n_b = u2_fj_op_lsh(wir_r, n_a, _n_ttext);

  return u2_fj_op_add(wir_r, n_b, _n_htext);
}

/* Dump (xw_cols preps) in nail style.
*/
static u2_noun
_c_dump_nail(u2_ray wir_r, 
             c3_w xw_cols,
             u2_noun  preps)
{
  u2_dump dump_pro;
  c3_w xw_dumps, xw_lines;
  u2_noun *n_dumps, *n_lines;
  c3_w i;

  /* Count the preps and make an array for them.
  */
  {
    u2_noun n = preps;

    xw_dumps = 0;
    while ( u2_yes == u2_dust(n) ) {
      xw_dumps++;
      n = u2_t(n);
    }
    n_dumps = alloca(xw_dumps * sizeof(u2_noun));
  }

  /* Dump the preps.
  */
  {
    u2_noun n = preps;

    for ( i=0; i < xw_dumps; i++ ) {
      n_dumps[i] = u2_fj_pump_dump(wir_r, xw_cols, u2_h(n));
      assert(u2_yes == u2_dust(n_dumps[i]));

      n = u2_t(n);
    }
  }

  /* Count the resulting lines, allocate the line array, and zero it.
  */
  {
    xw_lines = 1;

    for ( i=0; i < xw_dumps; i++ ) {
      u2_noun n_dump = n_dumps[i];
      c3_w j;

      j = 0;
      while ( !(u2_nul == n_dump) ) {
        n_dump = u2_t(n_dump);
        j++;
      }

      assert(j);
      xw_lines += (j - 1);
    }
    n_lines = alloca(xw_lines * sizeof(u2_noun));

    for ( i=0; i < xw_lines; i++ ) {
      n_lines[i] = 0;
    }
  }

  /* Insert and line up the lines.
  */
  {
    c3_w i_line, i_dump;

    i_line = 0;
    for ( i_dump=0; i_dump < xw_dumps; i_dump++ ) {
      u2_noun n_dump = n_dumps[i_dump];
      u2_noun n_first = u2_h(n_dump);
      u2_noun n_rest = u2_t(n_dump);
      c3_w xw_tab = 0;

      /* Append the first line of this dump to the current line.
      */
      {
        if ( !n_lines[i_line] ) {
          n_lines[i_line] = n_first;
        } else {
          n_lines[i_line] = _c_cat(wir_r, n_lines[i_line], n_first);
        }
      }

      /* Apply the following n_lines, tabbing them over appropriately.
      **
      ** Reset the tab to the length of the last line.
      */
      {
        u2_noun n;

        for ( n = n_rest; u2_yes == u2_dust(n); n = u2_t(n) ) {
          u2_noun text = u2_h(n);

          i_line++;
          n_lines[i_line] = _c_tab(wir_r, xw_tab, text);
        }
        xw_tab = u2_met(3, n_lines[i_line]);
      }
    }
    assert(i_line == (xw_lines - 1));
  }

  /* Compose the pan.
  */
  {
    dump_pro = u2_nul;

    for ( i=0; i < xw_lines; i++ ) {
      u2_noun n_line = n_lines[xw_lines - (i + 1)];

      dump_pro = u2_bc(wir_r, n_line, dump_pro);
    }
  }
  return dump_pro;
}

/* Dump (xw_cols preps) in glue style.
*/
static u2_noun
_c_dump_glue(u2_ray   wir_r, 
             c3_w   xw_cols,
             u2_list preps)
{
  u2_dump dump_pro;
  c3_w xw_dumps;
  u2_noun *n_dumps;
  c3_w sb;
  c3_t t_flat;
  c3_w i;

  /* Count the preps and make an array for them.
  */
  {
    u2_noun n = preps;

    xw_dumps = 0;
    while ( u2_yes == u2_dust(n) ) {
      xw_dumps++;
      n = u2_t(n);
    }
    n_dumps = alloca(xw_dumps * sizeof(u2_noun));
  }

  /* Dump the preps, on the yang stack.
  */
  { 
    u2_noun n = preps;

    for ( i=0; i < xw_dumps; i++ ) {
      u2_noun n_prep = u2_h(n);
      u2_noun n_dump = u2_fj_pump_dump(wir_r, xw_cols, n_prep);

      assert(u2_yes == u2_dust(n_dump));
      n_dumps[i] = n_dump;

      n = u2_t(n);
    }
  }

  /* See if we cap fit the result as flat.
  **
  ** Glue is flat if all the n_dumps are flat, and if the total
  ** width with spacing is <= xw_cols.
  */
  t_flat = 1;
  {
    sb = 0;
    for ( i=0; i < xw_dumps; i++ ) {
      u2_noun n_dump = n_dumps[i];
      u2_noun n_text = u2_h(n_dump);

      if ( u2_yes == u2_dust(u2_t(n_dump)) ) {
        t_flat = 0;
        break;
      }
      sb += u2_met(3, n_text);
    }

    if ( t_flat ) {
      if ( (sb + (xw_dumps - 1)) > xw_cols ) {
        t_flat = 0;
      }
    }
  }

  /* Print, in flat mode or not.  Return.
  */
  {
    if ( t_flat ) {
      c3_y *kb = alloca(sb + xw_dumps + 1);
      c3_w pb = 0;

      for ( i=0; i < xw_dumps; i++ ) {
        u2_noun n_dump = n_dumps[i];
        u2_noun n_text = u2_h(n_dump);
        c3_w sb_text = u2_met(3, n_text);

        if ( i ) {
          kb[pb++] = ' ';
        }
        u2_bytes(0, sb_text, (kb + pb), n_text);
        pb += sb_text;
      }
      dump_pro = u2_bc(wir_r, 
                       u2_bn_bytes(wir_r, pb, kb),
                       u2_nul);
    }
    else {
      c3_w xw_lines;
      u2_noun *n_lines;

      xw_lines = 0;
      for ( i=0; i < xw_dumps; i++ ) {
        u2_noun n_dump = n_dumps[i];

        while ( u2_yes == u2_dust(n_dump) ) {
          n_dump = u2_t(n_dump);
          xw_lines++;
        }
      }
      n_lines = alloca(xw_lines * sizeof(u2_noun));

      xw_lines = 0;
      for ( i=0; i < xw_dumps; i++ ) {
        u2_noun n_dump = n_dumps[i];

        while ( u2_yes == u2_dust(n_dump) ) {
          n_lines[xw_lines++] = u2_h(n_dump);
          n_dump = u2_t(n_dump);
        }
      }

      dump_pro = u2_nul;
      for ( i=0; i < xw_lines; i++ ) {
        u2_noun line = n_lines[xw_lines - (i + 1)];

        dump_pro = u2_bc(wir_r, line, dump_pro);
      }
    }
  }
  return dump_pro;
}

/* Dump (xw_cols text).
*/
static u2_noun
_c_dump_rock(u2_ray  wir_r, 
             c3_w   xw_cols,
             u2_noun text)
{
  return u2_bc(wir_r, text, u2_nul);
}

/* u2_fj_pump_dump():
**
**   Convert (prep) to a dump, printing (cols) wide.
*/
u2_dump
u2_fj_pump_dump(u2_ray  wir_r,
                c3_w    xw_cols,
                u2_prep prep)
{
  if ( u2_yes == u2_stud(prep) ) {
    return _c_dump_rock(wir_r, xw_cols, prep);
  } 
  else {
    u2_noun head = u2_h(prep);
    u2_noun tail = u2_t(prep);

    switch ( head ) {
      default: u2_err(wir_r, "head", head); return u2_bl_bail(wir_r, c3__fail);

      case c3__glue: return _c_dump_glue(wir_r, xw_cols, tail);
      case c3__nail: return _c_dump_nail(wir_r, xw_cols, tail);
    }
  }
}

/* print_dump(): print (dump).
*/
static void
_print_dump(u2_dump dump)
{
  while ( !(u2_nul == dump) ) {
    u2_atom line = u2_h(dump);
    c3_w   sb   = u2_met(3, line);
    c3_c   *cl  = alloca(sb + 1);

    u2_bytes(0, sb, (c3_y *)cl, line);
    cl[sb] = 0;
    printf(" %s\r\n", cl);

    dump = u2_t(dump);
  }
}

/* u2_err():
**
**   Print (nopt) with (caption), using (lane).
*/
void
u2_err(u2_ray      wir_r,
       const c3_c* cl_caption,
       u2_weak     noun)
{
  u2_rl_leap(wir_r, c3__rock);
  {
    if ( u2_none == noun ) {
      printf("%s: <none>\r\n", cl_caption);
    }
    else {
      u2_prep prep = u2_fj_prep_noun(wir_r, noun);
      u2_dump dump = u2_fj_pump_dump(wir_r, (75), prep);

      if ( cl_caption ) {
        printf("%s:\r\n", cl_caption);
      }
      _print_dump(dump);
    }
  }
  u2_rl_fall(wir_r);
}

/* u2_burp():
**
**   Print (prep) with (caption), using (lane).
*/
void
u2_burp(u2_ray      wir_r,
        const c3_c* cl_caption,
        u2_prep     prep)
{
  u2_rl_leap(wir_r, c3__rock);
  {
    u2_dump dump = u2_fj_pump_dump(wir_r, (75), prep);

    if ( cl_caption ) {
      printf("%s:\r\n", cl_caption);
    }
    _print_dump(dump);
  }
  u2_rl_fall(wir_r);
}
