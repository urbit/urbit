/* j/3/slaw.c
**
*/
#include "all.h"

#include <ctype.h>

static inline u3_noun
_parse_ud(u3_noun a)
{
  u3_weak pro;

  if ( u3_none == (pro = u3s_sift_ud(u3x_atom(a))) ) {
    return u3_nul;
  }

  return u3nc(u3_nul, pro);
}

static
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

  u3_noun ret = u3nc(0, u3qa_add(u3t(p), u3qa_mul(256, u3t(q))));
  u3z(p);
  u3z(q);

  return ret;
}

#define ENSURE_NOT_END()  do {                  \
    if (*cur == 0) {                            \
      u3a_free(c);                              \
      return u3_none;                           \
    }                                           \
  } while (0)

#define CONSUME(x)  do {                        \
    if (*cur != x) {                            \
      u3a_free(c);                              \
      return u3_none;                           \
    }                                           \
    cur++;                                      \
  } while (0)

#define TRY_GET_SYLLABLE(prefix)                                        \
  c3_c prefix##_one, prefix##_two, prefix##_three;                      \
  if (c3n == get_syllable(&cur, & prefix##_one, & prefix##_two, & prefix##_three)) { \
    u3a_free(c);                                                        \
    return u3_none;                                                     \
  }

u3_noun
_parse_p(u3_noun cor, u3_noun txt) {
  c3_c* c = u3a_string(txt);

  c3_c* cur = c;
  CONSUME('~');

  // We at least have a sig prefix. We're now going to parse tuples of three
  // lowercase letters. Our naming pattern for the pieces we read is [a b c d
  // ...] as we read them.
  TRY_GET_SYLLABLE(a);

  // There was only one syllable. If it's a valid suffix syllable, then
  // it's a galaxy. We don't even have to run this through the scrambler or
  // check for validity since its already a (unit @).
  if (*cur == 0) {
    u3a_free(c);
    return u3_po_find_suffix(a_one, a_two, a_three);
  }

  TRY_GET_SYLLABLE(b);

  // There were only two syllables. If they are a valid prefix and suffix, then
  // it's a star.
  if (*cur == 0) {
    u3_noun a_part = u3_po_find_prefix(a_one, a_two, a_three);
    u3_noun b_part = u3_po_find_suffix(b_one, b_two, b_three);
    u3_atom combined = combine(b_part, a_part);
    u3a_free(c);
    return combined;
  }

  // There must now be a - or it is invalid
  CONSUME('-');

  TRY_GET_SYLLABLE(c);

  ENSURE_NOT_END();

  TRY_GET_SYLLABLE(d);

  if (*cur == 0) {
    u3_noun a_part = u3_po_find_prefix(a_one, a_two, a_three);
    u3_noun b_part = u3_po_find_suffix(b_one, b_two, b_three);
    u3_noun c_part = u3_po_find_prefix(c_one, c_two, c_three);
    u3_noun d_part = u3_po_find_suffix(d_one, d_two, d_three);

    u3_noun m = combine(d_part, combine(c_part, combine(b_part, a_part)));
    u3a_free(c);

    if (_(u3a_is_atom(m))) {
      return 0;
    }

    u3_atom raw = u3k(u3t(m));
    u3z(m);

    u3_noun ob = u3j_cook("u3we_slaw_ob_p", u3k(cor), "ob");
    u3_noun hok = u3j_cook("u3we_slaw_fynd_p", ob, "fynd");
    return u3nc(0, u3n_slam_on(hok, raw));
  }

  // There must now be a - or it is invalid.
  CONSUME('-');

  // The next possible case is a "short" moon. (~ab-cd-ef)
  TRY_GET_SYLLABLE(e);

  ENSURE_NOT_END();

  TRY_GET_SYLLABLE(f);

  if (*cur == 0) {
    u3_noun a_part = u3_po_find_prefix(a_one, a_two, a_three);
    u3_noun b_part = u3_po_find_suffix(b_one, b_two, b_three);
    u3_noun c_part = u3_po_find_prefix(c_one, c_two, c_three);
    u3_noun d_part = u3_po_find_suffix(d_one, d_two, d_three);
    u3_noun e_part = u3_po_find_prefix(e_one, e_two, e_three);
    u3_noun f_part = u3_po_find_suffix(f_one, f_two, f_three);

    u3_noun m = combine(f_part, combine(e_part, combine(d_part,
                combine(c_part, combine(b_part, a_part)))));
    u3a_free(c);

    if (_(u3a_is_atom(m))) {
      return 0;
    }

    u3_atom raw = u3k(u3t(m));
    u3z(m);
    u3_noun ob = u3j_cook("u3we_slaw_ob_p", u3k(cor), "ob");
    u3_noun hok = u3j_cook("u3we_slaw_fynd_p", ob, "fynd");
    return u3nc(0, u3n_slam_on(hok, raw));
  }

  // There must now be a - or it is invalid.
  CONSUME('-');

  // The next possible case is a "long" moon. (~ab-cd-ef-gh)
  TRY_GET_SYLLABLE(g);

  ENSURE_NOT_END();

  TRY_GET_SYLLABLE(h);

  if (*cur == 0) {
    u3_noun a_part = u3_po_find_prefix(a_one, a_two, a_three);
    u3_noun b_part = u3_po_find_suffix(b_one, b_two, b_three);
    u3_noun c_part = u3_po_find_prefix(c_one, c_two, c_three);
    u3_noun d_part = u3_po_find_suffix(d_one, d_two, d_three);
    u3_noun e_part = u3_po_find_prefix(e_one, e_two, e_three);
    u3_noun f_part = u3_po_find_suffix(f_one, f_two, f_three);
    u3_noun g_part = u3_po_find_prefix(g_one, g_two, g_three);
    u3_noun h_part = u3_po_find_suffix(h_one, h_two, h_three);

    u3_noun m = combine(h_part, combine(g_part, combine(f_part,
                combine(e_part, combine(d_part, combine(c_part,
                combine(b_part, a_part)))))));
    u3a_free(c);

    if (_(u3a_is_atom(m))) {
      return 0;
    }

    u3_atom raw = u3k(u3t(m));
    u3z(m);
    u3_noun ob = u3j_cook("u3we_slaw_ob_p", u3k(cor), "ob");
    u3_noun hok = u3j_cook("u3we_slaw_fynd_p", ob, "fynd");
    return u3nc(0, u3n_slam_on(hok, raw));
  }

  // At this point, the only thing it could be is a long comet, of the form
  // ~ab-cd-ef-gh--ij-kl-mn-op

  CONSUME('-');
  CONSUME('-');

  TRY_GET_SYLLABLE(i);
  ENSURE_NOT_END();
  TRY_GET_SYLLABLE(j);
  CONSUME('-');
  TRY_GET_SYLLABLE(k);
  ENSURE_NOT_END();
  TRY_GET_SYLLABLE(l);
  CONSUME('-');
  TRY_GET_SYLLABLE(m);
  ENSURE_NOT_END();
  TRY_GET_SYLLABLE(n);
  CONSUME('-');
  TRY_GET_SYLLABLE(o);
  ENSURE_NOT_END();
  TRY_GET_SYLLABLE(p);

  if (*cur != 0) {
    // We've parsed all of a comet shape, and there's still more in the
    // string. Bail back to the interpreter.
    u3a_free(c);
    return u3_none;
  }

  // We have a long comet. Time to jam it all together. We rely on combine()
  // for the error checking and we don't have to scramble comet names.
  u3_noun a_part = u3_po_find_prefix(a_one, a_two, a_three);
  u3_noun b_part = u3_po_find_suffix(b_one, b_two, b_three);
  u3_noun c_part = u3_po_find_prefix(c_one, c_two, c_three);
  u3_noun d_part = u3_po_find_suffix(d_one, d_two, d_three);
  u3_noun e_part = u3_po_find_prefix(e_one, e_two, e_three);
  u3_noun f_part = u3_po_find_suffix(f_one, f_two, f_three);
  u3_noun g_part = u3_po_find_prefix(g_one, g_two, g_three);
  u3_noun h_part = u3_po_find_suffix(h_one, h_two, h_three);
  u3_noun i_part = u3_po_find_prefix(i_one, i_two, i_three);
  u3_noun j_part = u3_po_find_suffix(j_one, j_two, j_three);
  u3_noun k_part = u3_po_find_prefix(k_one, k_two, k_three);
  u3_noun l_part = u3_po_find_suffix(l_one, l_two, l_three);
  u3_noun m_part = u3_po_find_prefix(m_one, m_two, m_three);
  u3_noun n_part = u3_po_find_suffix(n_one, n_two, n_three);
  u3_noun o_part = u3_po_find_prefix(o_one, o_two, o_three);
  u3_noun p_part = u3_po_find_suffix(p_one, p_two, p_three);

  u3a_free(c);

  return combine(p_part, combine(o_part, combine(n_part, combine(m_part,
         combine(l_part, combine(k_part, combine(j_part, combine(i_part,
         combine(h_part, combine(g_part, combine(f_part, combine(e_part,
         combine(d_part, combine(c_part, combine(b_part, a_part)))))))))))))));
}

#define PARSE_NONZERO_NUMBER(numname)               \
  c3_w numname = 0;                                 \
  do {                                              \
    if (cur[0] > '9' || cur[0] < '1') {             \
      u3a_free(c);                                  \
      return u3_none;                               \
    }                                               \
    numname = cur[0] - '0';                         \
    cur++;                                          \
    while (isdigit(cur[0])) {                       \
      numname = u3ka_mul(numname, 10);              \
      numname = u3ka_add(numname, cur[0] - '0');    \
      cur++;                                        \
    }                                               \
  } while (0)

#define PARSE_INCLUDING_ZERO_NUMBER(numname)        \
  c3_w numname = 0;                                 \
  do {                                              \
    if (cur[0] > '9' || cur[0] < '0') {             \
      u3a_free(c);                                  \
      return u3_none;                               \
    }                                               \
    numname = cur[0] - '0';                         \
    cur++;                                          \
    while (isdigit(cur[0])) {                       \
      numname = u3ka_mul(numname, 10);              \
      numname = u3ka_add(numname, cur[0] - '0');    \
      cur++;                                        \
    }                                               \
  } while (0)

#define PARSE_HEX_DIGIT(out)                        \
  do {                                              \
    if (cur[0] >= '0' && cur[0] <= '9') {           \
      out = cur[0] - '0';                           \
    } else if (cur[0] >= 'a' && cur[0] <= 'f') {    \
      out = 10 + cur[0] - 'a';                      \
    } else {                                        \
      u3a_free(c);                                  \
      return u3_none;                               \
    }                                               \
    cur++;                                          \
  } while(0)


u3_noun
_parse_da(u3_noun cor, u3_noun txt) {
  c3_c* c = u3a_string(txt);

  c3_c* cur = c;
  CONSUME('~');

  // Parse out an arbitrary year number. Starts with a nonzero digit followed
  // by a series of any digits.
  PARSE_NONZERO_NUMBER(year);

  // Parse the optional negative sign for BC dates.
  u3_noun bc = c3y;
  if (cur[0] == '-') {
    bc = c3n;
    cur++;
  }

  CONSUME('.');

  // Parse out a two digit month (mot:ag). Either a single digit 1-9 or 1[012].
  c3_y month;
  if (cur[0] == '1') {
    if (cur[1] <= '2' && cur[1] >= '0') {
      // This is a two number month.
      month = 10 + cur[1] - '0';
      cur += 2;
    } else {
      // This is January.
      month = 1;
      cur++;
    }
  } else if (cur[0] <= '9' && cur[0] >= '2') {
    month = cur[0] - '0';
    cur++;
  } else {
    u3a_free(c);
    return u3_none;
  }

  CONSUME('.');

  // Parse out a two digit day (dip:ag). This number can be really big, so we
  // can track number of days since September 1993.
  PARSE_NONZERO_NUMBER(day);

  if (cur[0] == 0) {
    u3a_free(c);
    u3_noun hok = u3j_cook("u3we_slaw_parse_da", u3k(cor), "year");
    u3_noun res = u3n_slam_on(hok,
                              u3nt(u3nc(bc, year), month,
                                   u3nc(day, u3nq(0, 0, 0, 0))));
    return u3nc(0, res);
  }

  CONSUME('.');
  CONSUME('.');

  PARSE_INCLUDING_ZERO_NUMBER(hour);
  CONSUME('.');
  PARSE_INCLUDING_ZERO_NUMBER(minute);
  CONSUME('.');
  PARSE_INCLUDING_ZERO_NUMBER(second);

  if (cur[0] == 0) {
    u3a_free(c);
    u3_noun hok = u3j_cook("u3we_slaw_parse_da", u3k(cor), "year");
    u3_noun res = u3n_slam_on(hok,
                              u3nt(u3nc(bc, year), month,
                                   u3nc(day, u3nq(hour, minute, second, 0))));
    return u3nc(0, res);
  }

  CONSUME('.');
  CONSUME('.');

  // Now we have to parse a list of hexidecimal numbers 0-f of length 4 only
  // (zero padded otherwise) separated by dots.
  u3_noun list = 0;
  while (1) {
    // Parse 4 hex digits
    c3_y one, two, three, four;
    PARSE_HEX_DIGIT(one);
    PARSE_HEX_DIGIT(two);
    PARSE_HEX_DIGIT(three);
    PARSE_HEX_DIGIT(four);

    c3_w current = (one << 12) + (two << 8) + (three << 4) + four;
    list = u3nc(u3i_words(1, &current), list);

    if (cur[0] == 0) {
      u3a_free(c);

      u3_noun flopped = u3qb_flop(list);
      u3z(list);

      u3_noun hok = u3j_cook("u3we_slaw_parse_da", u3k(cor), "year");
      u3_noun res = u3n_slam_on(hok,
                                u3nt(u3nc(bc, year), month,
                                     u3nc(day,
                                          u3nq(hour, minute, second, flopped))));
      return u3nc(0, res);
    }

    CONSUME('.');
  }
}

#undef ENSURE_NOT_END
#undef CONSUME
#undef TRY_GET_SYLLABLE
#undef PARSE_NONZERO_NUMBER
#undef PARSE_HEX_DIGIT

u3_noun
_parse_tas(u3_noun txt) {
  // For any symbol which matches, txt will return itself as a
  // value. Therefore, this is mostly checking validity.
  c3_c* c = u3a_string(txt);

  // First character must represent a lowercase letter
  c3_c* cur = c;
  if (!islower(cur[0])) {
    u3a_free(c);
    return 0;
  }
  cur++;

  while (cur[0] != 0) {
    if (!(islower(cur[0]) || isdigit(cur[0]) || cur[0] == '-')) {
      u3a_free(c);
      return 0;
    }

    cur++;
  }

  u3a_free(c);
  return u3nc(0, u3k(txt));
}

u3_noun
u3we_slaw(u3_noun cor)
{
  u3_noun mod;
  u3_noun txt;

  if (c3n == u3r_mean(cor, u3x_sam_2, &mod,
                      u3x_sam_3, &txt, 0)) {
    return u3m_bail(c3__exit);
  }

  switch (mod) {
    case c3__da:
      return _parse_da(cor, txt);

    case 'p':
      return _parse_p(cor, txt);

    case c3__ud:
      return _parse_ud(txt);

      // %ta is used once in link.hoon. don't bother.

    case c3__tas:
      return _parse_tas(txt);

    default:
      return u3_none;
  }
}
