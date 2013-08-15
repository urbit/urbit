/* fake/pump/dump.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

  /* Forward declare.
  */
    static u4_noun
    _c_dump(u4_lane lane, u4_xw xw_cols, u4_prep prep);


/* Prepend (_xw_tab) spaces to (text).
*/
static u4_noun
_c_tab(u4_sh lane, 
       u4_xw _xw_tab,
       u4_noun  text) 
{
  u4_atom text_pro;

  if ( !_xw_tab ) {
    text_pro = text;
  } else {
    u4_i i;
    mpz_t mp;

    u4_a_gmp(text, mp);

    for ( i=0; i < _xw_tab; i++ ) {
      mpz_mul_2exp(mp, mp, 8);
      mpz_add_ui(mp, mp, 32);
    }
    text_pro = u4_k_atom_gmp(lane, mp);
  }

  return text_pro;
}

/* Glue (_n_htext) to (_n_ttext).
**
** (Replace with .cat or something.)
*/
static u4_noun 
_c_cat(u4_sh lane,
       u4_noun  _n_htext, 
       u4_noun  _n_ttext)
{
  u4_noun n_a, n_b;

  n_a = u4_k_atom_xw(lane, (u4_a_bin(_n_htext, 3) * 8));
  n_b = u4_op_lsh(lane, n_a, _n_ttext);

  return u4_op_add(lane, n_b, _n_htext);
}

/* Dump (xw_cols preps) in nail style.
*/
static u4_noun
_c_dump_nail(u4_sh lane, 
             u4_xw xw_cols,
             u4_noun  preps)
{
  u4_dump dump_pro;
  u4_xw xw_dumps, xw_lines;
  u4_noun *n_dumps, *n_lines;
  u4_i i;

  /* Count the preps and make an array for them.
  */
  {
    u4_noun n = preps;

    xw_dumps = 0;
    while ( u4_n_cell(n) ) {
      xw_dumps++;
      n = u4_ct(n);
    }
    n_dumps = alloca(xw_dumps * sizeof(u4_noun));
  }

  /* Dump the preps.
  */
  {
    u4_noun n = preps;

    for ( i=0; i < xw_dumps; i++ ) {
      n_dumps[i] = _c_dump(lane, xw_cols, u4_ch(n));
      assert(u4_n_cell(n_dumps[i]));

      n = u4_ct(n);
    }
  }

  /* Count the resulting lines, allocate the line array, and zero it.
  */
  {
    xw_lines = 1;

    for ( i=0; i < xw_dumps; i++ ) {
      u4_noun n_dump = n_dumps[i];
      u4_i j;

      j = 0;
      while ( !u4_n_zero(n_dump) ) {
        n_dump = u4_ct(n_dump);
        j++;
      }

      assert(j);
      xw_lines += (j - 1);
    }
    n_lines = alloca(xw_lines * sizeof(u4_noun));

    for ( i=0; i < xw_lines; i++ ) {
      n_lines[i] = 0;
    }
  }

  /* Insert and line up the lines.
  */
  {
    u4_i i_line, i_dump;

    i_line = 0;
    for ( i_dump=0; i_dump < xw_dumps; i_dump++ ) {
      u4_noun n_dump = n_dumps[i_dump];
      u4_noun n_first = u4_ch(n_dump);
      u4_noun n_rest = u4_ct(n_dump);
      u4_xw xw_tab = 0;

      /* Append the first line of this dump to the current line.
      */
      {
        if ( !n_lines[i_line] ) {
          n_lines[i_line] = n_first;
        } else {
          n_lines[i_line] = _c_cat(lane, n_lines[i_line], n_first);
        }
      }

      /* Apply the following n_lines, tabbing them over appropriately.
      **
      ** Reset the tab to the length of the last line.
      */
      {
        u4_noun n;

        for ( n = n_rest; u4_n_cell(n); n = u4_ct(n) ) {
          u4_noun text = u4_ch(n);

          i_line++;
          n_lines[i_line] = _c_tab(lane, xw_tab, text);
        }
        xw_tab = u4_a_bin(n_lines[i_line], 3);
      }
    }
    assert(i_line == (xw_lines - 1));
  }

  /* Compose the pan.
  */
  {
    dump_pro = u4_noun_0;

    for ( i=0; i < xw_lines; i++ ) {
      u4_noun n_line = n_lines[xw_lines - (i + 1)];

      dump_pro = u4_k_cell(lane, n_line, dump_pro);
    }
  }
  return dump_pro;
}

/* Dump (xw_cols preps) in glue style.
*/
static u4_noun
_c_dump_glue(u4_sh  lane, 
             u4_xw  xw_cols,
             u4_log preps)
{
  u4_dump dump_pro;
  u4_xw xw_dumps;
  u4_noun *n_dumps;
  u4_sb sb;
  u4_t t_flat;
  u4_i i;

  /* Count the preps and make an array for them.
  */
  {
    u4_noun n = preps;

    xw_dumps = 0;
    while ( u4_n_cell(n) ) {
      xw_dumps++;
      n = u4_ct(n);
    }
    n_dumps = alloca(xw_dumps * sizeof(u4_noun));
  }

  /* Dump the preps, on the yang stack.
  */
  { 
    u4_noun n = preps;

    for ( i=0; i < xw_dumps; i++ ) {
      u4_noun n_prep = u4_ch(n);
      u4_noun n_dump = _c_dump(lane, xw_cols, n_prep);

      assert(u4_n_cell(n_dump));
      n_dumps[i] = n_dump;

      n = u4_ct(n);
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
      u4_noun n_dump = n_dumps[i];
      u4_noun n_text = u4_ch(n_dump);

      if ( u4_n_cell(u4_ct(n_dump)) ) {
        t_flat = 0;
        break;
      }
      sb += u4_a_bin(n_text, 3);
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
      u4_kb *kb = alloca(sb + xw_dumps + 1);
      u4_pb pb = 0;

      for ( i=0; i < xw_dumps; i++ ) {
        u4_noun n_dump = n_dumps[i];
        u4_noun n_text = u4_ch(n_dump);
        u4_sb sb_text = u4_a_bin(n_text, 3);

        if ( i ) {
          kb[pb++] = ' ';
        }
        u4_a_bytes(n_text, (kb + pb), 0, sb_text);
        pb += sb_text;
      }
      dump_pro = u4_k_cell(lane, 
                           u4_k_atom_sb(lane, kb, pb),
                           u4_noun_0);
    }
    else {
      u4_xw xw_lines;
      u4_noun *n_lines;

      xw_lines = 0;
      for ( i=0; i < xw_dumps; i++ ) {
        u4_noun n_dump = n_dumps[i];

        while ( u4_n_cell(n_dump) ) {
          n_dump = u4_ct(n_dump);
          xw_lines++;
        }
      }
      n_lines = alloca(xw_lines * sizeof(u4_noun));

      xw_lines = 0;
      for ( i=0; i < xw_dumps; i++ ) {
        u4_noun n_dump = n_dumps[i];

        while ( u4_n_cell(n_dump) ) {
          n_lines[xw_lines++] = u4_ch(n_dump);
          n_dump = u4_ct(n_dump);
        }
      }

      dump_pro = u4_noun_0;
      for ( i=0; i < xw_lines; i++ ) {
        u4_noun line = n_lines[xw_lines - (i + 1)];

        dump_pro = u4_k_cell(lane, line, dump_pro);
      }
    }
  }
  return dump_pro;
}

/* Dump (xw_cols text).
*/
static u4_noun
_c_dump_rock(u4_sh   lane, 
             u4_xw   xw_cols,
             u4_noun text)
{
  return u4_k_cell(lane, text, u4_noun_0);
}

/* Dump (xw_cols prep).
**
** A dump is simply a list of n_lines.
*/
static u4_noun
_c_dump(u4_sh lane,
        u4_xw xw_cols,
        u4_prep  prep)
{
  if ( u4_n_atom(prep) ) {
    return _c_dump_rock(lane, xw_cols, prep);
  } 
  else {
    u4_noun head = u4_ch(prep);
    u4_noun tail = u4_ct(prep);

    switch ( u4_a_wtrip(head) ) {
      default: u4_err(lane, "head", head); return u4_trip;

      case u4__glue: return _c_dump_glue(lane, xw_cols, tail);
      case u4__nail: return _c_dump_nail(lane, xw_cols, tail);
    }
  }
}

/* _pump_dump_leak(): as u4_pump_dump(), leaking.
*/
static u4_dump
_pump_dump_leak(u4_lane lane,
                u4_atom cols,
                u4_prep prep)
{
  return _c_dump(lane, u4_a_wtrip(cols), prep);
}

/* _pump_dump_hat(): as u4_pump_dump(), on the hat.
*/
static u4_dump
_pump_dump_hat(u4_road road,
               u4_atom cols, 
               u4_prep prep)
{
  u4_bar  bar_cap = u4_road_bar_cap(road);
  u4_dump dump_cap;
  u4_dump dump_hat;
  {
    dump_cap = _pump_dump_leak(u4_cap(road), cols, prep);
  }
  dump_hat = u4_k_safe(u4_hat(road), dump_cap);

  u4_road_bar_cap(road) = bar_cap;
  return dump_hat;
}

/* u4_pump_dump():
**
**   Convert (prep) to a dump, printing (cols) wide.
*/
u4_dump
u4_pump_dump(u4_lane lane,
             u4_atom cols,
             u4_prep prep)
{
  u4_road road = u4_lane_road(lane);

  if ( u4_lane_side(lane) == u4_side_hat ) {
    return _pump_dump_hat(road, cols, prep);
  }
  else {
    u4_dump dump_pro;
    u4_road road_nest;

    road_nest = u4_r_nest_in(road);
    dump_pro = _pump_dump_hat(road_nest, cols, prep);
    u4_r_nest_out(road, road_nest);

    return dump_pro;
  }
  return u4_stub;
}
