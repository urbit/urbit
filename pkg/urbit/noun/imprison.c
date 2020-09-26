/* noun/imprison.c
**
*/
#include "all.h"

/* _ci_slab_size(): calculate slab bloq-size, checking for overflow.
*/
static c3_w
_ci_slab_size(c3_g met_g, c3_w len_w, c3_d* out_d)
{
  c3_d bit_d = (c3_d)len_w << met_g;
  c3_d byt_d = (bit_d + 0x7) >> 3;
  c3_d wor_d = (byt_d + 0x3) >> 2;
  c3_w wor_w = (c3_d)wor_d;

  if (  (wor_w != wor_d)
     || (len_w != (bit_d >> met_g)) )
  {
    return (c3_w)u3m_bail(c3__fail);
  }
  else {
    *out_d = byt_d;
    return wor_w;
  }
}

/* _ci_slab_init(): initialize slab with heap allocation.
**              NB: [len_w] must be >0
*/
static void
_ci_slab_init(u3i_slab* sab_u, c3_w len_w)
{
  c3_w*     nov_w = u3a_walloc(len_w + c3_wiseof(u3a_atom));
  u3a_atom* vat_u = (void *)nov_w;

  vat_u->mug_w = 0;
  vat_u->len_w = len_w;

  sab_u->_._vat_u = vat_u;
  sab_u->buf_w    = vat_u->buf_w;
  sab_u->wor_w    = len_w;
}

/* _ci_slab_grow(): update slab with heap reallocation.
*/
static void
_ci_slab_grow(u3i_slab* sab_u, c3_w len_w)
{
  c3_w*     old_w = (void*)sab_u->_._vat_u;
  //    XX implement a more efficient u3a_wealloc()
  //
  c3_w*     nov_w = u3a_wealloc(old_w, len_w);
  u3a_atom* vat_u = (void *)nov_w;

  vat_u->len_w = len_w;

  sab_u->_._vat_u = vat_u;
  sab_u->buf_w    = vat_u->buf_w;
  sab_u->wor_w    = len_w;
}

/* _ci_atom_mint(): finalize a heap-allocated atom at specified length.
*/
static u3_atom
_ci_atom_mint(u3a_atom* vat_u, c3_w len_w)
{
  c3_w* nov_w = (void*)vat_u;

  if ( 0 == len_w ) {
    u3a_wfree(nov_w);
    return (u3_atom)0;
  }
  else if ( 1 == len_w ) {
    c3_w dat_w = *vat_u->buf_w;

    if ( c3y == u3a_is_cat(dat_w) ) {
      u3a_wfree(nov_w);
      return (u3_atom)dat_w;
    }
  }

  //  try to strip a block off the end
  //
  {
    c3_w old_w = vat_u->len_w;

    if ( old_w > len_w ) {
      c3_y wiz_y = c3_wiseof(u3a_atom);
      u3a_wtrim(nov_w, old_w + wiz_y, len_w + wiz_y);
    }
  }

  vat_u->len_w = len_w;

  return u3a_to_pug(u3a_outa(nov_w));
}

/* u3i_slab_init(): configure bloq-length slab, zero-initialize.
*/
void
u3i_slab_init(u3i_slab* sab_u, c3_g met_g, c3_w len_w)
{
  u3t_on(mal_o);
  {
    c3_d byt_d;
    c3_w wor_w = _ci_slab_size(met_g, len_w, &byt_d);

    //  if we only need one word, use the static storage in [sab_u]
    //
    if ( 1 == wor_w ) {
      sab_u->_._vat_u = 0;
      sab_u->buf_w    = &sab_u->_._sat_w;
      sab_u->wor_w    = 1;
    }
    //  allocate an indirect atom
    //
    else {
      _ci_slab_init(sab_u, wor_w);
    }

    sab_u->byt_d = byt_d;

    //  XX refactor
    //
    memset(sab_u->buf_y, 0, (size_t)wor_w * 4);
  }
  u3t_off(mal_o);
}

/* u3i_slab_bare(): configure bloq-length slab, uninitialized.
*/
void
u3i_slab_bare(u3i_slab* sab_u, c3_g met_g, c3_w len_w)
{
  u3t_on(mal_o);
  {
    c3_d byt_d;
    c3_w wor_w = _ci_slab_size(met_g, len_w, &byt_d);

    //  if we only need one word, use the static storage in [sab_u]
    //
    if ( 1 == wor_w ) {
      sab_u->_._vat_u = 0;
      sab_u->buf_w    = &sab_u->_._sat_w;
      sab_u->wor_w    = 1;
    }
    //  allocate an indirect atom
    //
    else {
      _ci_slab_init(sab_u, wor_w);
    }

    sab_u->byt_d = byt_d;
  }
  u3t_off(mal_o);
}

/* u3i_slab_grow(): resize slab, reallocating as necessary.
*/
void
u3i_slab_grow(u3i_slab* sab_u, c3_g met_g, c3_w len_w)
{
  u3t_on(mal_o);
  {
    c3_d byt_d;
    c3_w wor_w = _ci_slab_size(met_g, len_w, &byt_d);

    //  XX actually shrink?
    //
    if ( wor_w <= sab_u->wor_w ) {
      sab_u->wor_w = wor_w;
    }
    //  upgrade from static storage
    //
    else if ( 1 == sab_u->wor_w ) {
      c3_w dat_w = *sab_u->buf_w;

      _ci_slab_init(sab_u, wor_w);
      sab_u->buf_w[0] = dat_w;
    }
    //  reallocate
    //
    else {
      _ci_slab_grow(sab_u, wor_w);
    }

    sab_u->byt_d = byt_d;
  }
  u3t_off(mal_o);
}

/* u3i_slab_free(): dispose memory backing slab.
*/
void
u3i_slab_free(u3i_slab* sab_u)
{
  u3t_on(mal_o);
  if ( 1 == sab_u->wor_w ) {
    c3_assert( !sab_u->_._vat_u );
  }
  else {
    u3a_atom* vat_u = sab_u->_._vat_u;

    c3_assert( (sab_u->buf_w - c3_wiseof(u3a_atom)) == (c3_w*)vat_u );

    u3a_wfree(vat_u);
  }
  u3t_off(mal_o);
}

/* u3i_slab_mint(): produce atom from slab, trimming.
*/
u3_atom
u3i_slab_mint(u3i_slab* sab_u)
{
  u3_atom pro;

  u3t_on(mal_o);

  if ( 1 == sab_u->wor_w ) {
    c3_w dat_w = *sab_u->buf_w;

    c3_assert( !sab_u->_._vat_u );

    u3t_off(mal_o);
    pro = u3i_word(dat_w);
    u3t_on(mal_o);
  }
  else {
    u3a_atom* vat_u = sab_u->_._vat_u;
    c3_w      len_w = sab_u->wor_w;

    c3_assert( (sab_u->buf_w - c3_wiseof(u3a_atom)) == (c3_w*)vat_u );

    while ( len_w && !(sab_u->buf_w[len_w - 1]) ) {
      len_w--;
    }

    pro = _ci_atom_mint(vat_u, len_w);
  }

  u3t_off(mal_o);

  return pro;
}

/* u3i_slab_moot(): produce atom from slab, no trimming.
*/
u3_atom
u3i_slab_moot(u3i_slab* sab_u)
{
  u3_atom pro;

  u3t_on(mal_o);

  if ( 1 == sab_u->wor_w ) {
    c3_w dat_w = *sab_u->buf_w;

    c3_assert( !sab_u->_._vat_u );

    u3t_off(mal_o);
    pro = u3i_word(dat_w);
    u3t_on(mal_o);
  }
  else {
    u3a_atom* vat_u = sab_u->_._vat_u;
    c3_w      len_w = sab_u->wor_w;

    c3_assert( (sab_u->buf_w - c3_wiseof(u3a_atom)) == (c3_w*)vat_u );

    pro = _ci_atom_mint(vat_u, len_w);
  }

  u3t_off(mal_o);

  return pro;
}

/* u3i_word(): construct u3_atom from c3_w.
*/
u3_atom
u3i_word(c3_w dat_w)
{
  u3_atom pro;

  u3t_on(mal_o);

  if ( c3y == u3a_is_cat(dat_w) ) {
    pro = (u3_atom)dat_w;
  }
  else {
    c3_w*     nov_w = u3a_walloc(1 + c3_wiseof(u3a_atom));
    u3a_atom* vat_u = (void *)nov_w;

    vat_u->mug_w = 0;
    vat_u->len_w = 1;
    vat_u->buf_w[0] = dat_w;

    pro = u3a_to_pug(u3a_outa(nov_w));
  }

  u3t_off(mal_o);

  return pro;
}

/* u3i_bytes(): Copy [a] bytes from [b] to an LSB first atom.
*/
u3_atom
u3i_bytes(c3_w        a_w,
          const c3_y* b_y)
{
  //  strip trailing zeroes.
  //
  while ( a_w && !b_y[a_w - 1] ) {
    a_w--;
  }

  if ( !a_w ) {
    return (u3_atom)0;
  }
  else {
    u3i_slab sab_u;

    u3i_slab_bare(&sab_u, 3, a_w);

    u3t_on(mal_o);
    {
      //  zero-initialize last word
      //
      sab_u.buf_w[sab_u.wor_w - 1] = 0;
      memcpy(sab_u.buf_y, b_y, a_w);
    }
    u3t_off(mal_o);

    return u3i_slab_moot_bytes(&sab_u);
  }
}

/* u3i_words(): Copy [a] words from [b] into an atom.
*/
u3_atom
u3i_words(c3_w        a_w,
          const c3_w* b_w)
{
  //  strip trailing zeroes.
  //
  while ( a_w && !b_w[a_w - 1] ) {
    a_w--;
  }

  if ( !a_w ) {
    return (u3_atom)0;
  }
  else {
    u3i_slab sab_u;
    u3i_slab_bare(&sab_u, 5, a_w);

    u3t_on(mal_o);
    memcpy(sab_u.buf_w, b_w, (size_t)4 * a_w);
    u3t_off(mal_o);

    return u3i_slab_moot(&sab_u);
  }
}

/* u3i_chubs(): Copy [a] chubs from [b] into an atom.
*/
u3_atom
u3i_chubs(c3_w        a_w,
          const c3_d* b_d)
{
  u3i_slab sab_u;
  u3i_slab_bare(&sab_u, 6, a_w);

  u3t_on(mal_o);
  {
    c3_w* buf_w = sab_u.buf_w;
    c3_w    i_w;
    c3_d    i_d;

    for ( i_w = 0; i_w < a_w; i_w++ ) {
      i_d = b_d[i_w];
      *buf_w++ = i_d & 0xffffffffULL;
      *buf_w++ = i_d >> 32;
    }
  }
  u3t_off(mal_o);

  return u3i_slab_mint(&sab_u);
}

/* u3i_mp(): Copy the GMP integer [a] into an atom, and clear it.
*/
u3_atom
u3i_mp(mpz_t a_mp)
{
  c3_w     pyg_w = mpz_size(a_mp) * (sizeof(mp_limb_t) / sizeof(c3_w));
  u3i_slab sab_u;
  u3i_slab_init(&sab_u, 5, sizeof(c3_w) * pyg_w);

  mpz_export(sab_u.buf_w, 0, -1, sizeof(c3_w), 0, 0, a_mp);
  mpz_clear(a_mp);

  return u3i_slab_mint(&sab_u);
}

/* u3i_vint(): increment [a].
*/
u3_atom
u3i_vint(u3_noun a)
{
  c3_assert(u3_none != a);

  if ( _(u3a_is_cat(a)) ) {
    return ( a == 0x7fffffff ) ? u3i_word(a + 1) : (a + 1);
  }
  else if ( _(u3a_is_cell(a)) ) {
    return u3m_bail(c3__exit);
  }
  else {
    mpz_t a_mp;

    u3r_mp(a_mp, a);
    u3z(a);

    mpz_add_ui(a_mp, a_mp, 1);
    return u3i_mp(a_mp);
  }
}

/* u3i_cell(): Produce the cell `[a b]`.
*/
u3_noun
u3i_cell(u3_noun a, u3_noun b)
{
  u3_noun pro;
  u3t_on(mal_o);

#ifdef U3_CPU_DEBUG
  u3R->pro.cel_d++;
#endif

  {
    c3_w*     nov_w = u3a_celloc();
    u3a_cell* nov_u = (void *)nov_w;

    nov_u->mug_w = 0;
    nov_u->hed = a;
    nov_u->tel = b;

    pro = u3a_to_pom(u3a_outa(nov_w));
  }

  u3t_off(mal_o);
  return pro;
}

/* u3i_trel(): Produce the triple `[a b c]`.
*/
u3_noun
u3i_trel(u3_noun a, u3_noun b, u3_noun c)
{
  return u3i_cell(a, u3i_cell(b, c));
}

/* u3i_qual(): Produce the cell `[a b c d]`.
*/
u3_noun
u3i_qual(u3_noun a, u3_noun b, u3_noun c, u3_noun d)
{
  return u3i_cell(a, u3i_trel(b, c, d));
}

/* u3i_string(): Produce an LSB-first atom from the C string [a].
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

/* u3i_list(): list from `u3_none`-terminated varargs.
*/
u3_noun
u3i_list(u3_weak som, ...)
{
  u3_noun lit = u3_nul;
  va_list  ap;

  if ( u3_none == som ) {
    return lit;
  }
  else {
    lit = u3nc(som, lit);
  }

  {
    u3_noun tem;

    va_start(ap, som);
    while ( 1 ) {
      if ( u3_none == (tem = va_arg(ap, u3_weak)) ) {
        break;
      }
      else {
        lit = u3nc(tem, lit);
      }
    }
    va_end(ap);
  }

  return u3kb_flop(lit);
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

  static u3_noun                            //  transfer
  _molt_apply(u3_noun            som,       //  retain
              c3_w               len_w,
              struct _molt_pair* pms_m)     //  transfer
  {
    if ( len_w == 0 ) {
      return u3k(som);
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

u3_noun
u3i_molt(u3_noun som, ...)
{
  va_list            ap;
  c3_w               len_w;
  struct _molt_pair* pms_m;
  u3_noun            pro;

  //  Count.
  //
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

  //  Install.
  //
  {
    c3_w i_w;

    va_start(ap, som);
    for ( i_w = 0; i_w < len_w; i_w++ ) {
      pms_m[i_w].axe_w = va_arg(ap, c3_w);
      pms_m[i_w].som = va_arg(ap, u3_noun);
    }
    va_end(ap);
  }

  //  Apply.
  //
  pro = _molt_apply(som, len_w, pms_m);
  u3z(som);
  return pro;
}
