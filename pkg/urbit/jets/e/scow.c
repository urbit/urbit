/* j/3/scow.c
**
*/
#include "all.h"

#include <ctype.h>

static
c3_y to_digit(u3_atom tig)
{
  if (tig >= 10) {
    return 87 + tig;
  } else {
    return '0' + tig;
  }
}

// gives the characters for a four 'digit' small hex atom.
static
void
_x_co_four(u3_atom src, c3_y* a, c3_y* b, c3_y* c, c3_y* d)
{
  *d = to_digit(src & 0xF);
  src >>= 4;
  *c = to_digit(src & 0xF);
  src >>= 4;
  *b = to_digit(src & 0xF);
  src >>= 4;
  *a = to_digit(src & 0xF);
}

// The parser always prints two digits on 0 in y-co.
static
void
_y_co_two(u3_atom src, c3_y* a, c3_y* b)
{
  *b = to_digit(src % 10);
  *a = to_digit(src / 10);
}

//
static
u3_noun
_add_year(u3_atom year, u3_noun out)
{
  while (year > 0) {
    out = u3nc(to_digit(year % 10), out);
    year = year / 10;
  }

  return out;
}

static
u3_noun
_print_da(u3_atom cor, u3_atom raw_da)
{
  u3_noun hok = u3j_cook("u3we_scow_print_da", u3k(cor), "yore");
  u3_noun yod = u3n_slam_on(hok, u3k(raw_da));

  u3_noun out = 0;
  //  u3m_p("yod", yod);

  u3_atom age, year, month, day, hour, min, sec, f;
  if (c3n == u3r_mean(yod, 4, &age,
                      5, &year,
                      6, &month,
                      14, &day,
                      30, &hour,
                      62, &min,
                      126, &sec,
                      127, &f,
                      0)) {
    return u3m_bail(c3__exit);
  }

  if (c3n == u3r_sing(f, 0)) {
    u3_noun f_list = u3qb_flop(f);

    for (u3_noun cur = f_list;
         _(u3a_is_cell(cur));
         cur = u3t(cur)) {
      if (_(u3a_is_cat(u3h(cur)))) {
        c3_y a, b, c, d;
        _x_co_four(u3h(cur), &a, &b, &c, &d);
        out = u3nq('.', a, b, u3nt(c, d, out));
      } else {
        // No way to deal with big atoms. fall back.
        u3z(yod);
        u3z(out);
        u3z(f_list);
        return u3_none;
      }
    }

    u3z(f_list);
    out = u3nc('.', out);
  }

  // if there isn't a hex list and the h/m/s are all 0, skip printing hours.
  if (c3n == u3r_sing(f, 0) ||
      c3n == u3r_sing(hour, 0) ||
      c3n == u3r_sing(min, 0) ||
      c3n == u3r_sing(sec, 0)) {
    if (!_(u3a_is_cat(hour)) ||
        !_(u3a_is_cat(min)) ||
        !_(u3a_is_cat(sec))) {
      // Input is weird, fallback to nock.
      u3z(yod);
      u3z(out);
      return u3_none;
    }

    c3_y sa, sb, ma, mb, ha, hb;
    _y_co_two(sec, &sa, &sb);
    out = u3nq('.', sa, sb, out);

    _y_co_two(min, &ma, &mb);
    out = u3nq('.', ma, mb, out);

    _y_co_two(hour, &ha, &hb);
    out = u3nq('.', ha, hb, out);

    out = u3nc('.', out);
  }

  // We always print the Y.M.D. Unlike others, these numbers are unconstrained
  // by length, but in practice, the month number and day number can only be up
  // to two digits because of +yore. We still need to remove 0 prefixes,
  // though.
  if (!_(u3a_is_cat(day)) || day > 99 ||
      !_(u3a_is_cat(month)) || month > 99 ||
      !_(u3a_is_cat(year))) {
    // Input is weird, fallback to nock.
    u3z(yod);
    u3z(out);
    return u3_none;
  }

  c3_y da, db;
  _y_co_two(day, &da, &db);
  out = u3nc(db, out);
  if (da != '0')
    out = u3nc(da, out);
  out = u3nc('.', out);

  c3_y ma, mb;
  _y_co_two(month, &ma, &mb);
  out = u3nc(mb, out);
  if (ma != '0')
    out = u3nc(ma, out);
  out = u3nc('.', out);

  // suffix the year with a '-' for BC dates
  if (age == c3n) {
    out = u3nc('-', out);
  }

  // The year part is the only place where we have to explicitly loop over the
  // input because it can be arbitrarily large or small.
  out = _add_year(year, out);

  out = u3nc('~', out);

  u3z(yod);
  return out;
}

static
u3_noun
_print_p(u3_atom p)
{
  // Scramble the raw number to the concealed version.
  u3_atom sxz = u3qc_ob_fein(p);

  // Simple galaxy case
  if (c3y == u3qa_lte(u3k(sxz), 256)) {
    c3_y a, b, c;
    po_to_suffix(sxz, &a, &b, &c);
    u3z(sxz);
    return u3nq('~', a, b, u3nc(c, 0));
  }

  u3_atom dyy = u3qc_met(4, u3k(sxz));
  if (!_(u3a_is_cat(dyy))) {
    return u3_none;
  }

  u3_noun list = 0;
  for (u3_atom imp = 0; imp != dyy; ++imp) {
    u3_noun log = u3qc_end(4, 1, u3k(sxz));
    u3_noun prefix = u3qc_rsh(3, 1, u3k(log));
    u3_noun suffix = u3qc_end(3, 1, log);

    c3_y a, b, c, d, e, f;
    po_to_prefix(prefix, &a, &b, &c);
    po_to_suffix(suffix, &d, &e, &f);

    if (imp % 4 == 0) {
      if (imp != 0) {
        list = u3nt('-', '-', list);
      }
    } else {
      list = u3nc('-', list);
    }

    list = u3nq(a, b, c, u3nq(d, e, f, list));

    sxz = u3qc_rsh(4, 1, sxz);
  }

  u3z(sxz);
  return u3nc('~', list);
}

static
u3_noun
_print_ud(u3_atom ud)
{
  // number of characters printed "between" periods.
  int between = 0;
  u3_atom list = 0;

  u3k(ud);

  do {
    if (between == 3) {
      list = u3nc('.', list);
      between = 0;
    }

    list = u3nc(u3ka_add(u3ka_mod(ud, 10), '0'), list);
    between++;
    ud = u3ka_div(ud, 10);
  } while (c3n == u3r_sing(ud, 0));

  return list;
}

//
u3_noun
u3we_scow(u3_noun cor)
{
  u3_noun mod;
  u3_noun atom;

  if (c3n == u3r_mean(cor, u3x_sam_2, &mod,
                      u3x_sam_3, &atom, 0) ||
      !_(u3a_is_cat(mod))) {
    u3l_log("u3we_scow fail\r\n");
    return u3m_bail(c3__fail);
  }

  switch (mod) {
    case c3__da:
      return _print_da(cor, atom);

    /* case 'p': */
    /*   return _print_p(atom); */

    case c3__ud:
      return _print_ud(atom);

    /*   // %ta is used once in link.hoon. don't bother. */

    /* case c3__tas: */
    /*   return _parse_tas(txt); */

    default:
      return u3_none;
  }
}

u3_noun
u3we_scot(u3_noun cor)
{
  /* u3l_log("u3we_scot"); */
  u3_noun mod;
  u3_noun atom;

  if (c3n == u3r_mean(cor, u3x_sam_2, &mod,
                      u3x_sam_3, &atom, 0) ||
      !_(u3a_is_cat(mod))) {
    u3l_log("u3we_scot fail\r\n");
    return u3m_bail(c3__fail);
  }

  switch (mod) {
    case c3__da: {
      u3_noun x = _print_da(cor, atom);
      if (x == u3_none) {
        return x;
      }
      u3_noun ret = u3qc_rap(3, x);
      u3z(x);
      return ret;
    }

    /* case 'p': */
    /*   return u3qc_rap(3, _print_p(atom)); */

    case c3__ud: {
      u3_atom tape = _print_ud(atom);
      u3_atom ret = u3qc_rap(3, tape);
      u3z(tape);
      return ret;
    }

    default:
      return u3_none;
  }
}
