/* j/3/slaw.c
**
*/
#include "all.h"

#include <ctype.h>

/* functions
*/

  u3_noun
  _parse_ud(u3_noun txt) {
    c3_c* c = u3r_string(txt);

    // First character must represent a digit
    c3_c* cur = c;
    if (cur[0] > '9' || cur[0] < '0') {
      c3_free(c);
      return 0;
    }
    c3_w total = cur[0] - '0';
    cur++;

    int since_last_period = 0;
    while (cur[0] != 0) {
      since_last_period++;
      if (cur[0] == '.') {
        since_last_period = 0;
        cur++;
        continue;
      }

      if (cur[0] > '9' || cur[0] < '0') {
        c3_free(c);
        return 0;
      }

      total = u3qa_mul(total, 10);
      total = u3qa_add(total, cur[0] - '0');
      cur++;

      if (since_last_period > 3) {
        c3_free(c);
        return 0;
      }
    }

    c3_free(c);
    return u3nc(0, total);
  }

  // parsing @p:
  //
  // +slaw calls +fed:ag directly. +fed:ag:
  //
  // - parses the text first into a number.
  // - runs fynd:ob, which is the scrambler restore structure. And +fynd is
  //   unjetted.
  //
  // The actual +po stuff, like +ins:po, is jetted but it's jetted such that it
  // pulls the tables out of the sample, so we can't just reuse it from other
  // jets.

  /* u3_noun */
  /* _parse_p(u3_noun txt) { */
  /*   // The current parsing text code for @p in hoon is kinda nuts. it parses */
  /*   // arbitrary lowercase ascii characters and then does a linear walk through */
  /*   // the  */


  /*   // Run the result through the scrambler to resolve the textual name to the */
  /*   // number. */
  /*   return u3nc(0, u3qc_ob_fynd(raw)); */
  /* } */


  u3_noun
  _parse_tas(u3_noun txt) {
    // For any symbol which matches, txt will return itself as a
    // value. Therefore, this is mostly checking validity.
    c3_c* c = u3r_string(txt);

    // First character must represent a lowercase letter
    c3_c* cur = c;
    if (!islower(cur[0])) {
      c3_free(c);
      return 0;
    }
    cur++;

    while (cur[0] != 0) {
      if (!(islower(cur[0]) || isdigit(cur[0]) || cur[0] == '-')) {
        c3_free(c);
        return 0;
      }

      cur++;
    }

    c3_free(c);
    return u3nc(0, txt);
  }

  u3_noun
  u3we_slaw(u3_noun cor)
  {
    u3_noun mod;
    u3_noun txt;

    if (c3n == u3r_mean(cor, u3x_sam_2, &mod,
                        u3x_sam_3, &txt, 0) ||
        !_(u3a_is_cat(mod))) {
      return u3m_bail(c3__fail);
    }

    switch (mod) {
      /* TODO: case c3__p. Need background jets first. */

      case c3__ud:
        return _parse_ud(txt);

      // %ta is used once in link.hoon. don't bother.

      case c3__tas:
        return _parse_tas(txt);

      default:
        return u3_none;
    }
  }
