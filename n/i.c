/* n/i.c
**
** This file is in the public domain.
*/
#include "all.h"

/* u2_ci_words():
**
**   Copy [a] words from [b] into an atom.
*/
u2_noun
u2_ci_words(c3_w        a_w,
            const c3_w* b_w)
{
  /* Strip trailing zeroes.
  */
  while ( a_w && !b_w[a_w - 1] ) {
    a_w--;
  }

  /* Check for cat.
  */
  if ( !a_w ) {
    return 0;
  }
  else if ( (a_w == 1) && !(b_w[0] >> 31) ) {
    return b_w[0];
  }

  /* Allocate, fill, return.
  */
  {
    c3_w*       nov_w = u2_ca_walloc(a_w + c3_wiseof(u2_cs_atom));
    u2_cs_atom* nov_u = (void*)nov_w;

    nov_u->mug_w = 0;
    nov_u->len_w = a_w;

    /* Fill the words.
    */
    {
      c3_w i_w;

      for ( i_w=0; i_w < a_w; i_w++ ) {
        nov_u->buf_w[i_w] = b_w[i_w];
      }
    }
    return u2_co_to_pug(u2_co_outa(nov_w));
  }
}

/* u2_ci_chubs():
**
**   Construct `a` double-words from `b`, LSD first, as an atom.
*/
u2_atom
u2_ci_chubs(c3_w        a_w,
            const c3_d* b_d)
{
  c3_w *b_w = c3_malloc(a_w * 8);
  c3_w i_w;
  u2_atom p;

  for ( i_w = 0; i_w < a_w; i_w++ ) {
    b_w[(2 * i_w)] = b_d[i_w] & 0xffffffffULL;
    b_w[(2 * i_w) + 1] = b_d[i_w] >> 32ULL;
  }
  p = u2_ci_words((a_w * 2), b_w);
  free(b_w);
  return p;
}

/* u2_ci_bytes():
**
**   Copy `a` bytes from `b` to an LSB first atom.
*/
u2_noun
u2_ci_bytes(c3_w        a_w,
            const c3_y* b_y)
{
  /* Strip trailing zeroes.
  */
  while ( a_w && !b_y[a_w - 1] ) {
    a_w--;
  }

  /* Check for cat.
  */
  if ( a_w <= 4 ) {
    if ( !a_w ) {
      return 0;
    }
    else if ( a_w == 1 ) {
      return b_y[0];
    }
    else if ( a_w == 2 ) {
      return (b_y[0] | (b_y[1] << 8));
    }
    else if ( a_w == 3 ) {
      return (b_y[0] | (b_y[1] << 8) | (b_y[2] << 16));
    }
    else if ( (b_y[3] <= 0x7f) ) {
      return (b_y[0] | (b_y[1] << 8) | (b_y[2] << 16) | (b_y[3] << 24));
    }
  }

  /* Allocate, fill, return.
  */
  {
    c3_w        len_w = (a_w + 3) >> 2;
    c3_w*       nov_w = u2_ca_walloc((len_w + c3_wiseof(u2_cs_atom)));
    u2_cs_atom* nov_u = (void*)nov_w;

    nov_u->mug_w = 0;
    nov_u->len_w = len_w;

    /* Clear the words.
    */
    {
      c3_w i_w;

      for ( i_w=0; i_w < len_w; i_w++ ) {
        nov_u->buf_w[i_w] = 0;
      }
    }

    /* Fill the bytes.
    */
    {
      c3_w i_w;

      for ( i_w=0; i_w < a_w; i_w++ ) {
        nov_u->buf_w[i_w >> 2] |= (b_y[i_w] << ((i_w & 3) * 8));
      }
    }
    return u2_co_to_pug(u2_co_outa(nov_w));
  }
}

/* u2_ci_mp():
**
**   Copy the GMP integer `a` into an atom, and clear it.
*/
u2_noun
u2_ci_mp(mpz_t a_mp)
{
  /* Efficiency: unnecessary copy.
  */
  {
    c3_w pyg_w  = mpz_size(a_mp) * ((sizeof(mp_limb_t)) / 4);
    c3_w *buz_w = alloca(pyg_w * 4);
    c3_w i_w;

    for ( i_w = 0; i_w < pyg_w; i_w++ ) {
      buz_w[i_w] = 0;
    }
    mpz_export(buz_w, 0, -1, 4, 0, 0, a_mp);
    mpz_clear(a_mp);

    return u2_ci_words(pyg_w, buz_w);
  }
}

/* u2_ci_vint():
**
**   Create `a + 1`.
*/
u2_noun
u2_ci_vint(u2_noun a)
{
  c3_assert(u2_none != a);

  if ( u2_so(u2_co_is_cat(a)) ) {
    c3_w vin_w = (a + 1);

    if ( a == 0x7fffffff ) {
      return u2_ci_words(1, &vin_w);
    }
    else return vin_w;
  }
  else if ( u2_so(u2_co_is_cell(a)) ) {
    return u2_cm_bail(c3__exit);
  }
  else {
    mpz_t a_mp;

    u2_cr_mp(a_mp, a);
    u2_ca_lose(a);

    mpz_add_ui(a_mp, a_mp, 1);
    return u2_ci_mp(a_mp);
  }
}

/* u2_ci_cell():
**
**   Produce the cell `[a b]`.
*/
u2_noun
u2_ci_cell(u2_noun a, u2_noun b)
{
  c3_assert(u2_none != a);
  c3_assert(u2_none != b);

  // c3_assert(u2_ne(u2_co_is_junior(a)));
  c3_assert(u2_ne(u2_co_is_junior(b)));

  {
    c3_w*       nov_w = u2_ca_walloc(c3_wiseof(u2_cs_cell));
    u2_cs_cell* nov_u = (void *)nov_w;

    nov_u->mug_w = 0;
    nov_u->hed = a;
    nov_u->tel = b;

    return u2_co_to_pom(u2_co_outa(nov_w));
  }
}

/* u2_ci_trel():
**
**   Produce the triple `[a b c]`.
*/
u2_noun
u2_ci_trel(u2_noun a, u2_noun b, u2_noun c)
{
  return u2_ci_cell(a, u2_ci_cell(b, c));
}

/* u2_ci_qual():
**
**   Produce the cell `[a b c d]`.
*/
u2_noun
u2_ci_qual(u2_noun a, u2_noun b, u2_noun c, u2_noun d)
{
  return u2_ci_cell(a, u2_ci_trel(b, c, d));
}

/* u2_ci_string():
**
**   Produce an LSB-first atom from the C string `a`.
*/
u2_noun
u2_ci_string(const c3_c* a_c)
{
  return u2_ci_bytes(strlen(a_c), (c3_y *)a_c);
}

/* u2_ci_tape(): from a C string, to a list of bytes.
*/
u2_atom
u2_ci_tape(const c3_c* txt_c)
{
  if ( !*txt_c ) {
    return u2_nul;
  } else return u2_ci_cell(*txt_c, u2_ci_tape(txt_c + 1));
}

/* u2_ci_decimal():
**
**   Parse `a` as a list of decimal digits.
*/
u2_atom
u2_ci_decimal(u2_noun a);

/* u2_ci_heximal():
**
**   Parse `a` as a list of hex digits.
*/
u2_noun
u2_ci_heximal(u2_noun a);

/* u2_ci_list():
**
**   Generate a null-terminated list, with `u2_none` as terminator.
*/
u2_noun
u2_ci_list(u2_weak one, ...);


/* u2_ci_molt():
**
**   Mutate `som` with a 0-terminated list of axis, noun pairs.
**   Axes must be cats (31 bit).
*/
  struct _molt_pair {
    c3_w    axe_w;
    u2_noun som;
  };

  static c3_w
  _molt_cut(c3_w               len_w,
            struct _molt_pair* pms_m)
  {
    c3_w i_w, cut_t, cut_w;

    cut_t = c3_false;
    cut_w = 0;
    for ( i_w = 0; i_w < len_w; i_w++ ) {
      c3_w axe_w = pms_m[i_w].axe_w;

      if ( (cut_t == c3_false) && (3 == u2_ax_cap(axe_w)) ) {
        cut_t = c3_true;
        cut_w = i_w;
      }
      pms_m[i_w].axe_w = u2_ax_mas(axe_w);
    }
    return cut_t ? cut_w : i_w;
  }

  static u2_noun                            //  transfer
  _molt_apply(u2_noun            som,       //  retain
              c3_w               len_w,
              struct _molt_pair* pms_m)     //  transfer
  {
    if ( len_w == 0 ) {
      return u2_ca_gain(som);
    }
    else if ( (len_w == 1) && (1 == pms_m[0].axe_w) ) {
      return pms_m[0].som;
    }
    else {
      c3_w cut_w = _molt_cut(len_w, pms_m);

      if ( u2_no == u2_co_is_cell(som) ) {
        return u2_cm_bail(c3__exit);
      } 
      else {
        return u2_ci_cell
           (_molt_apply(u2_co_h(som), cut_w, pms_m),
            _molt_apply(u2_co_t(som), (len_w - cut_w), (pms_m + cut_w)));
      }
    }
  }
u2_noun 
u2_ci_molt(u2_noun som, ...)
{
  va_list            ap;
  c3_w               len_w;
  struct _molt_pair* pms_m;
  u2_noun            pro;

  /* Count.
  */
  len_w = 0;
  {
    va_start(ap, som);
    while ( 1 ) {
      if ( 0 == va_arg(ap, c3_w) ) {
        break;
      }
      va_arg(ap, u2_weak*);
      len_w++;
    }
    va_end(ap);
  }
  pms_m = alloca(len_w * sizeof(struct _molt_pair));

  /* Install.
  */
  {
    c3_w i_w;

    va_start(ap, som);
    for ( i_w = 0; i_w < len_w; i_w++ ) {
      pms_m[i_w].axe_w = va_arg(ap, c3_w);
      pms_m[i_w].som = va_arg(ap, u2_noun);
    }
    va_end(ap);
  }

  /* Apply.
  */
  pro = _molt_apply(som, len_w, pms_m);
  u2_ca_lose(som);
  return pro;
}

