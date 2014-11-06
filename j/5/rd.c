/* j/5/aes.c
**
** This file is in the public domain.
*/
#include "all.h"


union doub {
  double d;
  c3_d c;
};


/* functions
*/
/* sun
*/
  u3_noun
  u3qer_sun(u3_atom a)
  {
    union doub b;
    b.d = (double) u3r_chub(0, a);

    return u3i_chubs(1, &b.c);
  }

  u3_noun
  u3wer_sun(u3_noun cor)
  {
    u3_noun a;

    if (c3n == u3r_mean(cor, u3v_sam, &a, 0)
        || c3n == u3ud(a)) {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qer_sun(a);
    }
  }

/* mul
*/
  u3_noun
  u3qer_mul(u3_atom a, u3_atom b)
  {
    union doub c, d, e;
    c.c = u3r_chub(0, a);
    d.c = u3r_chub(0, b);
    e.d = c.d * d.d;

    return u3i_chubs(1, &e.c);
  }

  u3_noun
  u3wer_mul(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3v_sam_2, &a, u3v_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qer_mul(a, b);
    }
  }

/* div
*/
  u3_noun
  u3qer_div(u3_atom a, u3_atom b)
  {
    union doub c, d, e;
    c.c = u3r_chub(0, a);
    d.c = u3r_chub(0, b);
    e.d = c.d / d.d;

    return u3i_chubs(1, &e.c);
  }

  u3_noun
  u3wer_div(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3v_sam_2, &a, u3v_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qer_div(a, b);
    }
  }

/* add
*/
  u3_noun
  u3qer_add(u3_atom a, u3_atom b)
  {
    union doub c, d, e;
    c.c = u3r_chub(0, a);
    d.c = u3r_chub(0, b);
    e.d = c.d + d.d;

    return u3i_chubs(1, &e.c);
  }

  u3_noun
  u3wer_add(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3v_sam_2, &a, u3v_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qer_add(a, b);
    }
  }

/* sub
*/
  u3_noun
  u3qer_sub(u3_atom a, u3_atom b)
  {
    union doub c, d, e;
    c.c = u3r_chub(0, a);
    d.c = u3r_chub(0, b);
    e.d = c.d - d.d;

    return u3i_chubs(1, &e.c);
  }

  u3_noun
  u3wer_sub(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3v_sam_2, &a, u3v_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qer_sub(a, b);
    }
  }

/* lte
*/
  u3_noun
  u3qer_lte(u3_atom a, u3_atom b)
  {
    union doub c, d;
    c.c = u3r_chub(0, a);
    d.c = u3r_chub(0, b);

    return __(c.d <= d.d);
  }

  u3_noun
  u3wer_lte(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3v_sam_2, &a, u3v_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qer_lte(a, b);
    }
  }

/* lth
*/
  u3_noun
  u3qer_lth(u3_atom a, u3_atom b)
  {
    union doub c, d;
    c.c = u3r_chub(0, a);
    d.c = u3r_chub(0, b);

    return __(c.d < d.d);
  }

  u3_noun
  u3wer_lth(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3v_sam_2, &a, u3v_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qer_lth(a, b);
    }
  }

/* gte
*/
  u3_noun
  u3qer_gte(u3_atom a, u3_atom b)
  {
    union doub c, d;
    c.c = u3r_chub(0, a);
    d.c = u3r_chub(0, b);

    return __(c.d >= d.d);
  }

  u3_noun
  u3wer_gte(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3v_sam_2, &a, u3v_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qer_gte(a, b);
    }
  }

/* gth
*/
  u3_noun
  u3qer_gth(u3_atom a, u3_atom b)
  {
    union doub c, d;
    c.c = u3r_chub(0, a);
    d.c = u3r_chub(0, b);

    return __(c.d > d.d);
  }

  u3_noun
  u3wer_gth(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3v_sam_2, &a, u3v_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qer_gth(a, b);
    }
  }
