/* j/3/scow.c
**
*/
#include "all.h"

#include <ctype.h>

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

  do {
    if (between == 3) {
      list = u3nc('.', list);
      between = 0;
    }
    list = u3nc(u3qa_add(u3qa_mod(u3k(ud), 10), '0'), list);
    between++;
    ud = u3qa_div(ud, 10);
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
    return u3m_bail(c3__fail);
  }

  switch (mod) {
    /* case c3__da: */
    /*   return _parse_da(cor, txt); */

    case 'p':
      return _print_p(atom);

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
  u3_noun mod;
  u3_noun atom;

  if (c3n == u3r_mean(cor, u3x_sam_2, &mod,
                      u3x_sam_3, &atom, 0) ||
      !_(u3a_is_cat(mod))) {
    return u3m_bail(c3__fail);
  }

  switch (mod) {
    case 'p':
      return u3qc_rap(3, _print_p(atom));

    case c3__ud:
      return u3qc_rap(3, _print_ud(atom));

    default:
      return u3_none;
  }
}
