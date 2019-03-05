/* g/i.c
**
*/
#include "all.h"

/* u3i_words():
**
**   Copy [a] words from [b] into an atom.
*/
u3_noun
u3i_words(c3_w        a_w,
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
    c3_w*       nov_w = u3a_walloc(a_w + c3_wiseof(u3a_atom));
    u3a_atom* nov_u = (void*)nov_w;

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
    return u3a_to_pug(u3a_outa(nov_w));
  }
}

/* u3i_chubs():
**
**   Construct `a` double-words from `b`, LSD first, as an atom.
*/
u3_atom
u3i_chubs(c3_w        a_w,
            const c3_d* b_d)
{
  c3_w *b_w = c3_malloc(a_w * 8);
  c3_w i_w;
  u3_atom p;

  for ( i_w = 0; i_w < a_w; i_w++ ) {
    b_w[(2 * i_w)] = b_d[i_w] & 0xffffffffULL;
    b_w[(2 * i_w) + 1] = b_d[i_w] >> 32ULL;
  }
  p = u3i_words((a_w * 2), b_w);
  free(b_w);
  return p;
}

/* u3i_bytes():
**
**   Copy `a` bytes from `b` to an LSB first atom.
*/
u3_noun
u3i_bytes(c3_w        a_w,
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
    c3_w*       nov_w = u3a_walloc((len_w + c3_wiseof(u3a_atom)));
    u3a_atom* nov_u = (void*)nov_w;

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
    return u3a_to_pug(u3a_outa(nov_w));
  }
}

/* u3i_mp():
**
**   Copy the GMP integer `a` into an atom, and clear it.
*/
u3_noun
u3i_mp(mpz_t a_mp)
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

    return u3i_words(pyg_w, buz_w);
  }
}

/* u3i_vint():
**
**   Create `a + 1`.
*/
u3_noun
u3i_vint(u3_noun a)
{
  c3_assert(u3_none != a);

  if ( _(u3a_is_cat(a)) ) {
    c3_w vin_w = (a + 1);

    if ( a == 0x7fffffff ) {
      return u3i_words(1, &vin_w);
    }
    else return vin_w;
  }
  else if ( _(u3a_is_cell(a)) ) {
    return u3m_bail(c3__exit);
  }
  else {
    mpz_t a_mp;

    u3r_mp(a_mp, a);
    u3a_lose(a);

    mpz_add_ui(a_mp, a_mp, 1);
    return u3i_mp(a_mp);
  }
}

c3_w BAD;

/* u3i_cell():
**
**   Produce the cell `[a b]`.
*/
u3_noun
u3i_cell(u3_noun a, u3_noun b)
{
  u3t_on(mal_o);

#ifdef U3_CPU_DEBUG
  u3R->pro.cel_d++;
#endif
  {
    // c3_w*       nov_w = u3a_walloc(c3_wiseof(u3a_cell));
    c3_w*       nov_w = u3a_celloc();
    u3a_cell* nov_u = (void *)nov_w;
    u3_noun     pro;

    nov_u->mug_w = 0;
    nov_u->hed = a;
    nov_u->tel = b;

    pro = u3a_to_pom(u3a_outa(nov_w));
#if 0
    if ( (0x730e66cc == u3r_mug(pro)) &&
         (c3__tssg == u3h(u3t(u3t(pro)))) ) {
      static c3_w xuc_w;
      fprintf(stderr, "BAD %x %p\r\n", pro, u3a_to_ptr(a));
      BAD = pro;
      if ( xuc_w == 1 ) u3m_bail(c3__exit);
      xuc_w++;
    }
#endif
#if 1
    u3t_off(mal_o);
    return pro;
#else
    if ( !FOO ) return u3a_to_pom(u3a_outa(nov_w));
    else {
      u3_noun pro = u3a_to_pom(u3a_outa(nov_w));

      u3m_p("leaked", pro);
      printf("pro %u, %x\r\n", pro, u3r_mug(pro));
      abort();
    }
#endif
  }
}

/* u3i_trel():
**
**   Produce the triple `[a b c]`.
*/
u3_noun
u3i_trel(u3_noun a, u3_noun b, u3_noun c)
{
  return u3i_cell(a, u3i_cell(b, c));
}

/* u3i_qual():
**
**   Produce the cell `[a b c d]`.
*/
u3_noun
u3i_qual(u3_noun a, u3_noun b, u3_noun c, u3_noun d)
{
  return u3i_cell(a, u3i_trel(b, c, d));
}

static u3_noun
_edit_cat(u3_noun big, c3_l axe_l, u3_noun som)
{
  if ( c3n == u3du(big) ) {
    return u3m_bail(c3__exit);
  }
  else {
    u3_noun pro;
    switch ( axe_l ) {
      case 2:
        pro = u3nc(som, u3k(u3t(big)));
        break;
      case 3:
        pro = u3nc(u3k(u3h(big)), som);
        break;
      default: {
        c3_l mor_l = u3x_mas(axe_l);
        pro = ( 2 == u3x_cap(axe_l) )
            ? u3nc(_edit_cat(u3k(u3h(big)), mor_l, som), u3k(u3t(big)))
            : u3nc(u3k(u3h(big)), _edit_cat(u3k(u3t(big)), mor_l, som));
        break;
      }
    }
    u3z(big);
    return pro;
  }
}

static u3_noun
_edit(u3_noun big, u3_noun axe, u3_noun som)
{
  if ( c3y == u3a_is_cat(axe) ) {
    return _edit_cat(big, (c3_l) axe, som);
  }
  else if ( c3n == u3du(big) ) {
    return u3m_bail(c3__exit);
  }
  else {
    u3_noun mor = u3qc_mas(axe),
            pro = ( 2 == u3qc_cap(axe) )
                ? u3nc(_edit(u3k(u3h(big)), mor, som), u3k(u3t(big)))
                : u3nc(u3k(u3h(big)), _edit(u3k(u3t(big)), mor, som));
    u3z(mor);
    u3z(big);
    return pro;
  }
}

static u3_noun _edit_or_mutate_cat(u3_noun, c3_l, u3_noun);
static u3_noun _edit_or_mutate(u3_noun, u3_noun, u3_noun);

static void
_mutate_cat(u3_noun big, c3_l axe_l, u3_noun som)
{
  if ( c3n == u3du(big) ) {
    u3m_bail(c3__exit);
  }
  else {
    u3a_cell* cel_u = (void*) u3a_to_ptr(big);
    switch ( axe_l ) {
      case 2:
        u3z(cel_u->hed);
        cel_u->hed = som;
        break;
      case 3:
        u3z(cel_u->tel);
        cel_u->tel = som;
        break;
      default: {
        u3_noun* tar = ( 2 == u3x_cap(axe_l) )
                     ? &(cel_u->hed)
                     : &(cel_u->tel);
        *tar = _edit_or_mutate_cat(*tar, u3x_mas(axe_l), som);
      }
    }
  }
}

static void
_mutate(u3_noun big, u3_noun axe, u3_noun som)
{
  if ( c3y == u3a_is_cat(axe) ) {
    _mutate_cat(big, (c3_l) axe, som);
  }
  else if ( c3n == u3du(big) ) {
    u3m_bail(c3__exit);
  }
  else {
    u3a_cell* cel_u = (void*) u3a_to_ptr(big);
    u3_noun mor = u3qc_mas(axe);
    u3_noun* tar = ( 2 == u3qc_cap(axe) )
                 ? &(cel_u->hed)
                 : &(cel_u->tel);
    *tar = _edit_or_mutate(*tar, mor, som);
    u3z(mor);
  }
}

static u3_noun
_edit_or_mutate_cat(u3_noun big, c3_l axe_l, u3_noun som)
{
  if ( c3y == u3a_is_mutable(u3R, big) ) {
    _mutate_cat(big, axe_l, som);
    return big;
  }
  else {
    return _edit_cat(big, axe_l, som);
  }
}

static u3_noun
_edit_or_mutate(u3_noun big, u3_noun axe, u3_noun som)
{
  if ( c3y == u3a_is_cat(axe) ) {
    return _edit_or_mutate_cat(big, (c3_l) axe, som);
  }
  else if ( c3y == u3a_is_mutable(u3R, big) ) {
    _mutate(big, axe, som);
    return big;
  }
  else {
    return _edit(big, axe, som);
  }
}

/* u3i_edit():
**
**   Mutate `big` at axis `axe` with new value `som`.
**   `axe` is RETAINED.
*/
u3_noun
u3i_edit(u3_noun big, u3_noun axe, u3_noun som)
{
  switch ( axe ) {
    case 0:
      return u3m_bail(c3__exit);
    case 1:
      u3z(big);
      return som;
    default:
      return _edit_or_mutate(big, axe, som);
  }
}

/* u3i_string():
**
**   Produce an LSB-first atom from the C string `a`.
*/
u3_noun
u3i_string(const c3_c* a_c)
{
  return u3i_bytes(strlen(a_c), (c3_y *)a_c);
}

/* u3i_tape(): from a C string, to a list of bytes.
*/
u3_atom
u3i_tape(const c3_c* txt_c)
{
  if ( !*txt_c ) {
    return u3_nul;
  } else return u3i_cell(*txt_c, u3i_tape(txt_c + 1));
}

/* u3i_decimal():
**
**   Parse `a` as a list of decimal digits.
*/
u3_atom
u3i_decimal(u3_noun a);

/* u3i_heximal():
**
**   Parse `a` as a list of hex digits.
*/
u3_noun
u3i_heximal(u3_noun a);

/* u3i_list():
**
**   Generate a null-terminated list, with `u3_none` as terminator.
*/
u3_noun
u3i_list(u3_weak one, ...);


/* u3i_molt():
**
**   Mutate `som` with a 0-terminated list of axis, noun pairs.
**   Axes must be cats (31 bit).
*/
  struct _molt_pair {
    c3_w    axe_w;
    u3_noun som;
  };

  static c3_w
  _molt_cut(c3_w               len_w,
            struct _molt_pair* pms_m)
  {
    c3_w i_w, cut_t, cut_w;

    cut_t = 0;
    cut_w = 0;
    for ( i_w = 0; i_w < len_w; i_w++ ) {
      c3_w axe_w = pms_m[i_w].axe_w;

      if ( (cut_t == 0) && (3 == u3x_cap(axe_w)) ) {
        cut_t = 1;
        cut_w = i_w;
      }
      pms_m[i_w].axe_w = u3x_mas(axe_w);
    }
    return cut_t ? cut_w : i_w;
  }

  __attribute__((no_sanitize("address")))
  static u3_noun                            //  transfer
  _molt_apply(u3_noun            som,       //  retain
              c3_w               len_w,
              struct _molt_pair* pms_m)     //  transfer
  {
    if ( len_w == 0 ) {
      return u3a_gain(som);
    }
    else if ( (len_w == 1) && (1 == pms_m[0].axe_w) ) {
      return pms_m[0].som;
    }
    else {
      c3_w cut_w = _molt_cut(len_w, pms_m);

      if ( c3n == u3a_is_cell(som) ) {
        return u3m_bail(c3__exit);
      }
      else {
        return u3i_cell
           (_molt_apply(u3a_h(som), cut_w, pms_m),
            _molt_apply(u3a_t(som), (len_w - cut_w), (pms_m + cut_w)));
      }
    }
  }

__attribute__((no_sanitize("address")))
u3_noun
u3i_molt(u3_noun som, ...)
{
  va_list            ap;
  c3_w               len_w;
  struct _molt_pair* pms_m;
  u3_noun            pro;

  /* Count.
  */
  len_w = 0;
  {
    va_start(ap, som);
    while ( 1 ) {
      if ( 0 == va_arg(ap, c3_w) ) {
        break;
      }
      va_arg(ap, u3_weak*);
      len_w++;
    }
    va_end(ap);
  }

  c3_assert( 0 != len_w );
  pms_m = alloca(len_w * sizeof(struct _molt_pair));

  /* Install.
  */
  {
    c3_w i_w;

    va_start(ap, som);
    for ( i_w = 0; i_w < len_w; i_w++ ) {
      pms_m[i_w].axe_w = va_arg(ap, c3_w);
      pms_m[i_w].som = va_arg(ap, u3_noun);
    }
    va_end(ap);
  }

  /* Apply.
  */
  pro = _molt_apply(som, len_w, pms_m);
  u3a_lose(som);
  return pro;
}

