/* j/3/scow.c
**
*/
#include "all.h"

#include <ctype.h>

static
c3_y to_digit(c3_y tig)
{
  if (tig >= 10) {
    return 87 + tig;
  } else {
    return '0' + tig;
  }
}

static
c3_y to_w_digit(c3_y tig)
{
  if (tig == 63) {
    return '~';
  } else if (tig == 62) {
    return '-';
  } else if (tig >= 36) {
    return 29 + tig;
  } else if (tig >= 10) {
    return 87 + tig;
  } else {
    return '0' + tig;
  }
}

// gives the characters for a four 'digit' small hex atom.
static
void
_x_co_four(c3_w src, c3_y* a, c3_y* b, c3_y* c, c3_y* d)
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
_y_co_two(c3_w src, c3_y* a, c3_y* b)
{
  *b = to_digit(src % 10);
  *a = to_digit(src / 10);
}

//
static
u3_noun
_add_year(c3_w year, u3_noun out)
{
  while (year > 0) {
    out = u3nc(to_digit(year % 10), out);
    year = year / 10;
  }

  return out;
}

static
u3_noun
_print_da(u3_noun cor, u3_atom raw_da)
{
  u3_noun hok = u3j_cook("u3we_scow_print_da", u3k(cor), "yore");
  u3_noun yod = u3n_slam_on(hok, u3k(raw_da));

  u3_noun out = 0;

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

  if (f != 0) {
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
  if (f != 0 || hour != 0 || min != 0 || sec != 0) {
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
  if (da != '0') {
    out = u3nc(da, out);
  }
  out = u3nc('.', out);

  c3_y ma, mb;
  _y_co_two(month, &ma, &mb);
  out = u3nc(mb, out);
  if (ma != '0') {
    out = u3nc(ma, out);
  }
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
_print_p(u3_atom cor, u3_atom p)
{
  // Scramble the raw number to the concealed version.
  u3_noun ob = u3j_cook("u3we_scow_ob_p", u3k(cor), "ob");
  u3_noun hok = u3j_cook("u3we_scow_fein_p", ob, "fein");
  u3_atom sxz = u3n_slam_on(hok, u3k(p));

  // Simple galaxy case
  if (c3y == u3qa_lth(sxz, 256)) {
    c3_y a, b, c;
    u3_po_to_suffix(sxz, &a, &b, &c);
    u3z(sxz);
    return u3nq('~', a, b, u3nc(c, 0));
  }

  u3_atom dyy = u3qc_met(4, sxz);
  if (!_(u3a_is_cat(dyy))) {
    u3z(sxz);
    u3z(dyy);
    return u3_none;
  }

  u3_noun list = 0;
  for (c3_w imp = 0; imp != dyy; ++imp) {
    c3_w log = u3qc_end(4, 1, sxz);
    c3_w prefix = u3qc_rsh(3, 1, log);
    c3_w suffix = u3qc_end(3, 1, log);

    c3_y a, b, c, d, e, f;
    u3_po_to_prefix(prefix, &a, &b, &c);
    u3_po_to_suffix(suffix, &d, &e, &f);

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
  c3_i between = 0;
  u3_noun list = 0;

  // increase input refcount to be consumed in u3ka_div(), which will free each
  // intermediary state.
  u3k(ud);

  do {
    if (between == 3) {
      list = u3nc('.', list);
      between = 0;
    }

    list = u3nc(u3ka_add(u3qa_mod(ud, 10), '0'), list);
    between++;
    ud = u3ka_div(ud, 10);
  } while (ud != 0);

  return list;
}

static
u3_noun
_print_uv(u3_atom uv)
{
  // number of characters printed "between" periods.
  c3_i between = 0;
  u3_noun list = 0;

  // increase input refcount to be consumed in u3ka_div(), which will free each
  // intermediary state.
  u3k(uv);

  do {
    if (between == 5) {
      list = u3nc('.', list);
      between = 0;
    }

    c3_y tig = u3qa_mod(uv, 32);
    list = u3nc(to_digit(tig), list);
    between++;
    uv = u3ka_div(uv, 32);
  } while (uv != 0);

  return u3nt('0', 'v', list);
}

static
u3_noun
_print_uw(u3_atom uw)
{
  // number of characters printed "between" periods.
  c3_i between = 0;
  u3_noun list = 0;

  // increase input refcount to be consumed in u3ka_div(), which will free each
  // intermediary state.
  u3k(uw);

  do {
    if (between == 5) {
      list = u3nc('.', list);
      between = 0;
    }

    c3_y tig = u3qa_mod(uw, 64);
    list = u3nc(to_w_digit(tig), list);
    between++;
    uw = u3ka_div(uw, 64);
  } while (uw != 0);

  return u3nt('0', 'w', list);
}

u3_noun
u3we_scow(u3_noun cor)
{
  u3_atom mod;
  u3_atom atom;

  if (c3n == u3r_mean(cor, u3x_sam_2, &mod,
                      u3x_sam_3, &atom, 0)) {
    return u3m_bail(c3__exit);
  }

  switch (mod) {
    case c3__da:
      return _print_da(cor, atom);

    case 'p':
      return _print_p(cor, atom);

    case c3__ud:
      return _print_ud(atom);

    case c3__uv:
      return _print_uv(atom);

    case c3__uw:
      return _print_uw(atom);

    default:
      return u3_none;
  }
}

u3_noun
u3we_scot(u3_noun cor)
{
  u3_atom mod;
  u3_atom atom;

  if (c3n == u3r_mean(cor, u3x_sam_2, &mod,
                      u3x_sam_3, &atom, 0)) {
    return u3m_bail(c3__exit);
  }

  u3_noun tape;
  switch (mod) {
    case c3__da:
      tape = _print_da(cor, atom);
      break;

    case 'p':
      tape = _print_p(cor, atom);
      break;

    case c3__ud:
      tape = _print_ud(atom);
      break;

    case c3__uv:
      tape = _print_uv(atom);
      break;

    case c3__uw:
      tape = _print_uw(atom);
      break;

    default:
      return u3_none;
  }

  if (tape == u3_none) {
    return tape;
  }
  u3_noun ret = u3qc_rap(3, tape);
  u3z(tape);
  return ret;
}
