/* j/3/scot.c
**
*/
#include "all.h"

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

  {
    u3_noun out = u3qc_rap(3, list);
    u3z(list);
    return out;
  }
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

  list = u3nt('0', 'v', list);
  {
    u3_noun out = u3qc_rap(3, list);
    u3z(list);
    return out;
  }
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

  list = u3nt('0', 'w', list);
  {
    u3_noun out = u3qc_rap(3, list);
    u3z(list);
    return out;
  }
}

u3_atom
u3qe_scot(u3_atom a, u3_atom b)
{
  switch (a) {
    case c3__tas: return u3k(b);
    case c3__ud: return _print_ud(b);
    // case c3__ud:  return u3s_etch_ud(b);
    case c3__ux:  return u3s_etch_ux(b);
    case c3__uv: return _print_uv(b);
    // case c3__uv:  return u3s_etch_uv(b);
    case c3__uw: return _print_uw(b);
    // case c3__uw:  return u3s_etch_uw(b);
    default:      return u3_none;
  }
}

u3_noun
u3we_scot(u3_noun cor)
{
  u3_atom a, b;
  u3x_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0);
  return u3qe_scot(u3x_atom(a), u3x_atom(b));
}
