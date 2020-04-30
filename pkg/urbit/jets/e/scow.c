/* j/3/scow.c
**
*/
#include "all.h"

#include <ctype.h>

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

    /* case 'p': */
    /*   return _parse_p(txt); */

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
    case c3__ud:
      return u3qc_rap(3, _print_ud(atom));

    default:
      return u3_none;
  }
}
