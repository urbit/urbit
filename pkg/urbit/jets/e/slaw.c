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

u3_noun get_syllable(c3_c** cur_ptr, c3_c* one, c3_c* two, c3_c* three) {
  if (islower((*cur_ptr)[0]) && islower((*cur_ptr)[1]) &&
      islower((*cur_ptr)[2])) {
    *one = (*cur_ptr)[0];
    *two = (*cur_ptr)[1];
    *three = (*cur_ptr)[2];
    (*cur_ptr) += 3;
    return c3y;
  } else {
    return c3n;
  }
}

static
u3_noun combine(u3_noun p, u3_noun q)
{
  if (_(u3a_is_atom(p))) {
    return 0;
  }

  if (_(u3a_is_atom(q))) {
    return 0;
  }

  u3_noun ret = u3nc(0, u3qa_add(u3k(u3t(p)), u3qa_mul(256, u3k(u3t(q)))));
  u3z(p);
  u3z(q);

  return ret;
}

#define ENSURE_NOT_END()  do {                  \
    if (*cur == 0) {                            \
      c3_free(c);                               \
      return 0;                                 \
    }                                           \
  } while (0)

#define CONSUME_HEP()  do {                     \
    if (*cur != '-') {                          \
      c3_free(c);                               \
      return 0;                                 \
    }                                           \
    cur++;                                      \
  } while (0)

#define TRY_GET_SYLLABLE(prefix)                                        \
  c3_c prefix##_one, prefix##_two, prefix##_three;                      \
  if (c3n == get_syllable(&cur, & prefix##_one, & prefix##_two, & prefix##_three)) { \
    c3_free(c);                                                         \
    return 0;                                                           \
  }

u3_noun
_parse_p(u3_noun txt) {
  c3_c* c = u3r_string(txt);

  c3_c* cur = c;
  if (cur[0] != '~') {
    c3_free(c);
    return 0;
  }
  cur++;

  // We at least have a sig prefix. We're now going to parse tuples of three
  // lowercase letters. Our naming pattern for the pieces we read is [a b c d
  // ...] as we read them.
  TRY_GET_SYLLABLE(a);

  // There was only one syllable. If it's a valid suffix syllable, then
  // it's a galaxy. We don't even have to run this through the scrambler or
  // check for validity since its already a (unit @).
  if (*cur == 0) {
    c3_free(c);
    return po_find_suffix(a_one, a_two, a_three);
  }

  TRY_GET_SYLLABLE(b);

  // There were only two syllables. If they are a valid prefix and suffix, then
  // it's a star.
  if (*cur == 0) {
    u3_noun a_part = po_find_prefix(a_one, a_two, a_three);
    u3_noun b_part = po_find_suffix(b_one, b_two, b_three);
    u3_atom combined = combine(b_part, a_part);
    c3_free(c);
    return combined;
  }

  // There must now be a - or it is invalid
  CONSUME_HEP();

  TRY_GET_SYLLABLE(c);

  ENSURE_NOT_END();

  TRY_GET_SYLLABLE(d);

  if (*cur == 0) {
    u3_noun a_part = po_find_prefix(a_one, a_two, a_three);
    u3_noun b_part = po_find_suffix(b_one, b_two, b_three);
    u3_noun c_part = po_find_prefix(c_one, c_two, c_three);
    u3_noun d_part = po_find_suffix(d_one, d_two, d_three);

    u3_noun m = combine(d_part, combine(c_part, combine(b_part, a_part)));
    c3_free(c);

    if (_(u3a_is_atom(m))) {
      return 0;
    }

    u3_atom raw = u3k(u3t(m));
    u3z(m);
    return u3nc(0, u3qc_ob_fynd(raw));
  }

  // There must now be a - or it is invalid.
  CONSUME_HEP();

  // The next possible case is a "short" moon. (~ab-cd-ef)
  TRY_GET_SYLLABLE(e);

  ENSURE_NOT_END();

  TRY_GET_SYLLABLE(f);

  if (*cur == 0) {
    u3_noun a_part = po_find_prefix(a_one, a_two, a_three);
    u3_noun b_part = po_find_suffix(b_one, b_two, b_three);
    u3_noun c_part = po_find_prefix(c_one, c_two, c_three);
    u3_noun d_part = po_find_suffix(d_one, d_two, d_three);
    u3_noun e_part = po_find_prefix(e_one, e_two, e_three);
    u3_noun f_part = po_find_suffix(f_one, f_two, f_three);

    u3_noun m = combine(f_part, combine(e_part, combine(d_part,
                combine(c_part, combine(b_part, a_part)))));
    c3_free(c);

    if (_(u3a_is_atom(m))) {
      return 0;
    }

    u3_atom raw = u3k(u3t(m));
    u3z(m);
    return u3nc(0, u3qc_ob_fynd(raw));
  }

  // There must now be a - or it is invalid.
  CONSUME_HEP();

  // The next possible case is a "long" moon. (~ab-cd-ef-gh)
  TRY_GET_SYLLABLE(g);

  ENSURE_NOT_END();

  TRY_GET_SYLLABLE(h);

  if (*cur == 0) {
    u3_noun a_part = po_find_prefix(a_one, a_two, a_three);
    u3_noun b_part = po_find_suffix(b_one, b_two, b_three);
    u3_noun c_part = po_find_prefix(c_one, c_two, c_three);
    u3_noun d_part = po_find_suffix(d_one, d_two, d_three);
    u3_noun e_part = po_find_prefix(e_one, e_two, e_three);
    u3_noun f_part = po_find_suffix(f_one, f_two, f_three);
    u3_noun g_part = po_find_prefix(g_one, g_two, g_three);
    u3_noun h_part = po_find_suffix(h_one, h_two, h_three);

    u3_noun m = combine(h_part, combine(g_part, combine(f_part,
                combine(e_part, combine(d_part, combine(c_part,
                combine(b_part, a_part)))))));
    c3_free(c);

    if (_(u3a_is_atom(m))) {
      return 0;
    }

    u3_atom raw = u3k(u3t(m));
    u3z(m);
    return u3nc(0, u3qc_ob_fynd(raw));
  }

  // At this point, the only thing it could be is a long comet, of the form
  // ~ab-cd-ef-gh--ij-kl-mn-op

  CONSUME_HEP();
  CONSUME_HEP();

  TRY_GET_SYLLABLE(i);
  ENSURE_NOT_END();
  TRY_GET_SYLLABLE(j);
  CONSUME_HEP();
  TRY_GET_SYLLABLE(k);
  ENSURE_NOT_END();
  TRY_GET_SYLLABLE(l);
  CONSUME_HEP();
  TRY_GET_SYLLABLE(m);
  ENSURE_NOT_END();
  TRY_GET_SYLLABLE(n);
  CONSUME_HEP();
  TRY_GET_SYLLABLE(o);
  ENSURE_NOT_END();
  TRY_GET_SYLLABLE(p);

  if (*cur != 0) {
    // We've parsed all of a comet shape, and there's still more in the
    // string. Error.
    c3_free(c);
    return 0;
  }

  // We have a long comet. Time to jam it all together. We rely on combine()
  // for the error checking and we don't have to scramble comet names.
  u3_noun a_part = po_find_prefix(a_one, a_two, a_three);
  u3_noun b_part = po_find_suffix(b_one, b_two, b_three);
  u3_noun c_part = po_find_prefix(c_one, c_two, c_three);
  u3_noun d_part = po_find_suffix(d_one, d_two, d_three);
  u3_noun e_part = po_find_prefix(e_one, e_two, e_three);
  u3_noun f_part = po_find_suffix(f_one, f_two, f_three);
  u3_noun g_part = po_find_prefix(g_one, g_two, g_three);
  u3_noun h_part = po_find_suffix(h_one, h_two, h_three);
  u3_noun i_part = po_find_prefix(i_one, i_two, i_three);
  u3_noun j_part = po_find_suffix(j_one, j_two, j_three);
  u3_noun k_part = po_find_prefix(k_one, k_two, k_three);
  u3_noun l_part = po_find_suffix(l_one, l_two, l_three);
  u3_noun m_part = po_find_prefix(m_one, m_two, m_three);
  u3_noun n_part = po_find_suffix(n_one, n_two, n_three);
  u3_noun o_part = po_find_prefix(o_one, o_two, o_three);
  u3_noun p_part = po_find_suffix(p_one, p_two, p_three);

  c3_free(c);

  return combine(p_part, combine(o_part, combine(n_part, combine(m_part,
         combine(l_part, combine(k_part, combine(j_part, combine(i_part,
         combine(h_part, combine(g_part, combine(f_part, combine(e_part,
         combine(d_part, combine(c_part, combine(b_part, a_part)))))))))))))));
}

#undef ENSURE_NOT_END
#undef CONSUME_HEP
#undef TRY_GET_SYLLABLE

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
    return u3nc(0, u3k(txt));
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
      case 'p':
        return _parse_p(txt);

      case c3__ud:
        return _parse_ud(txt);

      // %ta is used once in link.hoon. don't bother.

      case c3__tas:
        return _parse_tas(txt);

      default:
        return u3_none;
    }
  }
