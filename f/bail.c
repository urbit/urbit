/* f/bail.c
**
** This file is in the public domain.
*/
#include "all.h"

/* u2_bl_bail(): bail out.
**
**  Bail codes:
**
**    c3__exit for normal exit with correct trace
**    c3__fail for abnormal failure without assumptions
**
**  When in doubt, fail.
**
**  In both cases, a gc is necessary to clean up leaks.
*/
u2_noun
u2_bl_bail(u2_wire wir_r,
           c3_l    how_l)
{
  return u2_cm_bail(how_l);
}

/* u2_bl_push(): push on meaning stack.
*/
void
u2_bl_push(u2_wire wir_r,
           u2_noun mon)                                           //  transfer
{
  u2_noun tax = u2_rc(wir_r, mon, u2_rx(wir_r, u2_wire_tax(wir_r)));

  if ( u2_none != tax ) {
    u2_rz(wir_r, u2_wire_tax(wir_r));
    u2_wire_tax(wir_r) = tax;
  }
  else u2_bl_bail(wir_r, c3__fail);
}

/* u2_bl_mean(): push `[%mean roc]` on trace stack.
*/
void
u2_bl_mean(u2_wire wir_r,
           u2_noun roc)                                          //  transfer
{
  return u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, roc));
}

/* u2_bl_drop(): drop from meaning stack.
*/
void
u2_bl_drop(u2_wire wir_r)
{
  u2_noun tax = u2_wire_tax(wir_r);

  c3_assert(u2_yes == u2_dust(tax));

  u2_wire_tax(wir_r) = u2_rx(wir_r, u2_t(tax));
  u2_rz(wir_r, tax);
}

/* u2_bl_error(): simple string error.
*/
u2_noun
u2_bl_error(u2_wire     wir_r,
            const c3_c* err_c)                                    //  retain
{
  u2_bl_push(wir_r, u2_bc(wir_r, c3__lose, u2_bn_string(wir_r, err_c)));

  return u2_bl_bail(wir_r, c3__exit);
}

/* u2_bl_some(): test for zero ray.
*/
u2_ray
u2_bl_some(u2_wire wir_r,
           u2_ray  ray_r)
{
  if ( 0 == ray_r ) {
    return u2_bl_bail(wir_r, c3__fail);
  }
  else return ray_r;
}

/* u2_bl_good(): test for u2_none.
*/
u2_noun
u2_bl_good(u2_ray  wir_r,
           u2_weak som)
{
  if ( u2_none == som ) {
    return u2_bl_bail(wir_r, c3__exit);
  }
  else return som;
}

/* u2_bl_flat(): test for atom.
*/
u2_atom
u2_bl_flat(u2_ray  wir_r,
           u2_weak som)
{
  if ( u2_none == som ) {
    return u2_bl_bail(wir_r, c3__exit);
  }
  else return som;
}

/* u2_bi_h():
**
**   Return the head of (a).
*/
u2_noun
u2_bi_h(u2_ray  wir_r,
        u2_noun a)
{
  if ( u2_no == u2_dust(a) ) return u2_bl_bail(wir_r, c3__exit);

  return u2_h(a);
}

/* u2_bi_t():
**
**   Return the tail of (a).
*/
u2_noun
u2_bi_t(u2_ray  wir_r,
        u2_noun a)
{
  if ( u2_no == u2_dust(a) ) return u2_bl_bail(wir_r, c3__exit);

  return u2_t(a);
}

/* u2_bi_frag():
**
**   Return fragment (a) of (b).
*/
u2_noun
u2_bi_frag(u2_ray  wir_r,
           u2_atom a,
           u2_noun b)
{
  u2_weak c = u2_frag(a, b);

  if ( u2_none == c ) {
    return u2_bl_bail(wir_r, c3__exit);
  } else return c;
}

/* u2_bi_met():
**
**   Return the size of (b) in bits, rounded up to
**   (1 << a_y).
**
**   For example, (a_y == 3) returns the size in bytes.
*/
c3_w
u2_bi_met(u2_ray  wir_r,
          c3_y    a_y,
          u2_noun b)
{
  if ( u2_no == u2_stud(b) ) return u2_bl_bail(wir_r, c3__exit);

  return u2_met(a_y, b);
}

/* u2_bi_bit():
**
**   Return bit (a_w) of (b).
*/
c3_b
u2_bi_bit(u2_ray  wir_r,
          c3_w    a_w,
          u2_noun b)
{
  if ( u2_no == u2_stud(b) ) return u2_bl_bail(wir_r, c3__exit);

  return u2_bit(a_w, b);
}

/* u2_bi_byte():
**
**   Return byte (a_w) of (b).
*/
c3_y
u2_bi_byte(u2_ray  wir_r,
           c3_w    a_w,
           u2_noun b)
{
  if ( u2_no == u2_stud(b) ) return u2_bl_bail(wir_r, c3__exit);

  return u2_byte(a_w, b);
}

/* u2_bi_bytes():
**
**  Copy bytes (a_w) through (a_w + b_w - 1) from (d) to (c).
*/
void
u2_bi_bytes(u2_ray  wir_r,
            c3_w    a_w,
            c3_w    b_w,
            c3_y*   c_y,
            u2_noun d)
{
  if ( u2_no == u2_stud(d) ) u2_bl_bail(wir_r, c3__exit);

  u2_bytes(a_w, b_w, c_y, d);
}

/* u2_bi_mp():
**
**   Copy (b) into (a_mp).
*/
void
u2_bi_mp(u2_ray  wir_r,
         mpz_t   a_mp,
         u2_noun b)
{
  if ( u2_no == u2_stud(b) ) u2_bl_bail(wir_r, c3__exit);

  u2_mp(a_mp, b);
}

/* u2_bi_word():
**
**   Return word (a_w) of (b).
*/
c3_w
u2_bi_word(u2_ray  wir_r,
           c3_w    a_w,
           u2_noun b)
{
  if ( u2_no == u2_stud(b) ) return u2_bl_bail(wir_r, c3__exit);

  return u2_word(a_w, b);
}

/* u2_bi_words():
**
**  Copy words (a_w) through (a_w + b_w - 1) from (d) to (c).
*/
void
u2_bi_words(u2_ray  wir_r,
            c3_w    a_w,
            c3_w    b_w,
            c3_w*   c_w,
            u2_noun d)
{
  if ( u2_no == u2_stud(d) ) u2_bl_bail(wir_r, c3__exit);

  u2_words(a_w, b_w, c_w, d);
}

/* u2_bn_bytes():
**
**   Copy [a] bytes from [b].
*/
u2_noun
u2_bn_bytes(u2_ray      wir_r,
            c3_w        a_w,
            const c3_y* b_y)
{
  return u2_bl_good(wir_r, u2_rl_bytes(wir_r, a_w, b_y));
}

/* u2_bn_string():
**
**   u2_bn_bytes(wir_r, strlen(a_c), (c3_y *)a_c);
*/
u2_noun
u2_bn_string(u2_ray      wir_r,
             const c3_c* a_c)
{
  return u2_bl_good(wir_r, u2_rl_string(wir_r, a_c));
}

/* u2_bn_cell():
**
**   Produce the cell [a b].
*/
u2_noun
u2_bn_cell(u2_ray  wir_r,
           u2_noun a,
           u2_noun b)
{
  return u2_bl_good(wir_r, u2_rl_cell(wir_r, a, b));
}

/* u2_bn_ice():
**
**   Produce `a`, not referencing the can.  Copy or gain reference.
*/
u2_noun
u2_bn_ice(u2_ray  wir_r,
          u2_weak a)
{
  return u2_bl_good(wir_r, u2_rl_ice(wir_r, u2_bl_good(wir_r, a)));
}

/* u2_bn_list():
**
**   Generate a null-terminated list, with a 0 terminator.
*/
u2_noun
u2_bn_list(u2_ray wir_r, ...)
{
  c3_w    len_w = 0;
  va_list vap;

  /* Count.
  */
  {
    va_start(vap, wir_r);
    while ( u2_none != va_arg(vap, u2_noun) ) {
      len_w++;
    }
    va_end(vap);
  }

  /* Allocate.
  */
  {
    c3_w    i_w;
    u2_noun yit[len_w];

    va_start(vap, wir_r);
    for ( i_w = 0; i_w < len_w; i_w++ ) {
      yit[i_w] = va_arg(vap, u2_noun);
    }
    va_end(vap);

    /* Construct.
    */
    {
      u2_noun woq = u2_nul;

      for ( i_w = 0; i_w < len_w; i_w++ ) {
        woq = u2_bc(wir_r, yit[len_w - (i_w + 1)], woq);
      }
      return woq;
    }
  }
}

/* u2_bn_molt():
**
**   Mutate `som` with a 0-terminated list of axis, noun pairs.
**   Axes must be cats (31 bit).
**
**   Caller retains arguments; function transfers result.
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

  static u2_noun
  _molt_apply(u2_wire            wir_r,
              u2_noun            som,
              c3_w               len_w,
              struct _molt_pair* pms_m)
  {
    if ( len_w == 0 ) {
      return u2_rl_gain(wir_r, som);
    }
    else if ( (len_w == 1) && (1 == pms_m[0].axe_w) ) {
      return u2_rl_gain(wir_r, pms_m[0].som);
    }
    else {
      c3_w cut_w = _molt_cut(len_w, pms_m);

      if ( u2_no == u2_dust(som) ) {
        return u2_bc
          (wir_r,
           _molt_apply(wir_r, u2_nul, cut_w, pms_m),
           _molt_apply(wir_r, u2_nul, (len_w - cut_w), (pms_m + cut_w)));
      } else {
        return u2_bc
          (wir_r,
           _molt_apply(wir_r, u2_h(som), cut_w, pms_m),
           _molt_apply(wir_r, u2_t(som), (len_w - cut_w), (pms_m + cut_w)));
      }
    }
  }

/* u2_bn_molf():
**
**   As u2_bn_molt(), with argument pointer.
*/
u2_noun
u2_bn_molf(u2_wire wir_r,
           u2_noun som,
           va_list vap)
{
  va_list            vaq;
  c3_w               len_w;
  struct _molt_pair* pms_m;

  /* Count.
  */
  len_w = 0;
  {
    va_copy(vaq, vap);
    while ( 1 ) {
      if ( 0 == va_arg(vaq, c3_w) ) {
        break;
      }
      va_arg(vaq, u2_noun*);
      len_w++;
    }
    va_end(vaq);
  }
  pms_m = alloca(len_w * sizeof(struct _molt_pair));

  /* Install.
  */
  {
    c3_w i_w;

    va_copy(vaq, vap);
    for ( i_w = 0; i_w < len_w; i_w++ ) {
      pms_m[i_w].axe_w = va_arg(vaq, c3_w);
      pms_m[i_w].som = va_arg(vaq, u2_noun);
    }
    va_end(vaq);
  }

  /* Apply.
  */
  return _molt_apply(wir_r, som, len_w, pms_m);
}

/* u2_bn_molt():
**
**   Mutate `som` with a 0-terminated list of axis, noun pairs.
**   Axes must be cats (31 bit).
*/
u2_noun
u2_bn_molt(u2_wire wir_r,
           u2_noun som,
           ...)
{
  va_list            ap;
  c3_w               len_w;
  struct _molt_pair* pms_m;

  /* Count.
  */
  len_w = 0;
  {
    va_start(ap, som);
    while ( 1 ) {
      if ( 0 == va_arg(ap, c3_w) ) {
        break;
      }
      va_arg(ap, u2_noun*);
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
  return _molt_apply(wir_r, som, len_w, pms_m);
}

/* u2_bn_mp():
**
**   Copy the GMP integer [a] into an atom.
*/
u2_noun
u2_bn_mp(u2_ray wir_r,
         mpz_t  a_mp)
{
  return u2_bl_good(wir_r, u2_rl_mp(wir_r, a_mp));
}

/* u2_bn_nock():
**
**   Nock or bail.
*/
u2_noun
u2_bn_nock(u2_ray wir_r, u2_noun bus, u2_noun fol)
{
  u2_noun pro;

  if ( (u2_none == bus) || (u2_none == fol) ) {
    return u2_bl_bail(wir_r, c3__fail);
  }
#if 1
  pro = u2_nk_nock(wir_r, u2_rl_gain(wir_r, bus), fol);

  if ( u2_none == pro ) {
    return u2_bl_bail(wir_r, c3__exit);
  }
  else return pro;
#else
  return u2_bl_good(wir_r, u2_nk_nock(wir_r, u2_rl_gain(wir_r, bus), fol));
#endif
}

/* u2_bn_mang():
**
**   Kick a gate, substituting axes with nouns.
**
**   Caller retains arguments; function transfers result.
*/
u2_noun
u2_bn_mang(u2_wire wir_r,
           u2_noun gat,
           ...)
{
  va_list vap;
  u2_noun dur, pro;

  va_start(vap, gat);
  dur = u2_bn_molf(wir_r, gat, vap);
  va_end(vap);

  pro = u2_nk_nock(wir_r, dur, u2_t(dur));

  return pro;
}

/* u2_bn_hook():
**
**   Execute hook from core.
*/
u2_noun
u2_bn_hook(u2_wire     wir_r,
           u2_noun     cor,
           const c3_c* tam_c)
{
  u2_weak vib = u2_ds_look(wir_r, cor, tam_c);

  if ( u2_none == vib ) {
    fprintf(stderr, "no hook: %s\n", tam_c);
    c3_assert(0);
    return u2_bl_bail(wir_r, c3__fail);
  } else {
    if ( u2_nul == u2_h(vib) ) {
      u2_noun rag = u2_frag(u2_t(vib), cor);

      // printf("%s %d\n", tam_c, u2_t(vib));
      u2_rz(wir_r, vib);

      return u2_rx(wir_r, rag);
    }
    else {
      u2_noun ret = u2_bn_nock(wir_r, cor, vib);

      u2_rz(wir_r, vib);
      return ret;
    }
  }
}

/* u2_bn_cook():
**
**   Reverse hook as molt.
*/
u2_noun                                                           //  transfer
u2_bn_cook(u2_wire     wir_r,
           u2_noun     cor,                                       //  retain
           const c3_c* tam_c,
           u2_noun     som)                                       //  transfer
{
  u2_weak vib = u2_ds_look(wir_r, cor, tam_c);
  u2_noun axe;

  if ( (u2_none == vib) ||
       (u2_no == u2_dust(vib)) ||
       (u2_nul != u2_h(vib)) ||
       (u2_no == u2_stud(axe = u2_t(vib)) ) )
  {
    u2_rz(wir_r, vib);

    return u2_bl_bail(wir_r, c3__fail);
  } else {
    u2_noun gon = u2_bn_molt(wir_r, cor, axe, som, 0);

    u2_rz(wir_r, vib);
    u2_rz(wir_r, som);
    return gon;
  }
}

/* u2_bn_mong():
**
**   Call by gate and sample (new convention).
**   Caller retains `gat`, transfers `sam`.
*/
u2_noun
u2_bn_mong(u2_wire wir_r,
           u2_weak gat,
           u2_weak sam)
{
  u2_weak pro = u2_nk_mong(wir_r, gat, sam);

  if ( u2_none == pro ) {
    return u2_bl_bail(wir_r, c3__exit);
  }
  else return pro;
}

/* u2_bn_gort():
**
**  Call by core, depth, hook, molt list.
*/
u2_noun
u2_bn_gort(u2_wire     wir_r,
           u2_noun     cor,
           const c3_c* tam_c,
           ...)
{
  // XX: tested, but leaks.  Check memory protocol.
  //
  u2_noun fol = u2_bl_good(wir_r, u2_ds_look(wir_r, cor, tam_c));
  u2_noun gat = u2_bn_nock(wir_r, cor, fol);
  u2_noun tec;
  va_list vap;

  va_start(vap, tam_c);
  tec = u2_bn_molf(wir_r, gat, vap);
  va_end(vap);

  u2_rz(wir_r, fol);
  return u2_bn_nock(wir_r, tec, u2_t(tec));
}

/* u2_bn_wait():
**
**  Produce the functional equivalent of `|.(~(tam cor sam))`.
*/
u2_noun                                                           //  produce
u2_bn_wait(u2_wire     wir_r,
           u2_noun     cor,                                       //  retain
           u2_noun     sam,                                       //  retain
           const c3_c* tam_c)                                     //  retain
{
  c3_assert(!"not implemented"); return 0;
#if 0
  u2_noun rac = u2_bn_molt(wir_r, cor, u2_cv_sam, sam, 0);
  u2_noun rox = u2_ds_look(wir_r, rac, tam_c);

  if ( u2_none == rox ) {
    return u2_bl_bail(wir_r, c3__fail);
  } else {
    return u2_bc
      (wir_r, rac,
              u2_bt(wir_r,
                    u2_nock_flac,
                    u2_bc(wir_r, 0, 2),
                    rox));
  }
#endif
}

/* u2_bn_qual():
**
**   Produce the quadruple [a b c d].
*/
u2_noun
u2_bn_qual(u2_ray  wir_r,
           u2_noun a,
           u2_noun b,
           u2_noun c,
           u2_noun d)
{
  return u2_bl_good(wir_r, u2_rl_qual(wir_r, a, b, c, d));
}

/* u2_bn_quil():
**
**   Produce the quintuple [a b c d].
*/
u2_noun
u2_bn_quil(u2_ray  wir_r,
           u2_noun a,
           u2_noun b,
           u2_noun c,
           u2_noun d,
           u2_noun e)
{
  return u2_bn_cell(wir_r, a, u2_bn_qual(wir_r, b, c, d, e));
}

/* u2_bn_tape():
**
**   Create an atomic string from a list of bytes.
*/
u2_noun
u2_bn_tape(u2_ray  wir_r,
           u2_list lit)
{
  c3_w len_w = 0;

  {
    u2_noun ilt = lit;

    while ( u2_nul != ilt ) { len_w++; ilt = u2_t(ilt); }
  }
  {
    c3_w lat_w = 0;
    c3_y buf_y[len_w];

    while ( u2_nul != lit ) {
      buf_y[lat_w++] = u2_bi_byte(wir_r, 0, u2_h(lit));
      lit = u2_t(lit);
    }
    return u2_bn_bytes(wir_r, len_w, buf_y);
  }
}

/* u2_bn_decimal():
**
**   On (wir_r), write (list), a list of digits, as a decimal.
*/
u2_noun
u2_bn_decimal(u2_ray  wir_r,
              u2_list lit)
{
  mpz_t mp;

  mpz_init(mp);
  while ( u2_nul != lit ) {
    c3_w byt_w = u2_bi_byte(wir_r, 0, u2_h(lit));

    mpz_mul_ui(mp, mp, 10);
    mpz_add_ui(mp, mp, (byt_w - '0'));

    lit = u2_t(lit);
  }
  return u2_bn_mp(wir_r, mp);
}

/* u2_bn_heximal():
**
**   On (wir_r), write (lit), a list of digits, as a hexadecimal.
*/
u2_noun
u2_bn_heximal(u2_ray  wir_r,
              u2_list lit)
{
  mpz_t mp;

  mpz_init(mp);
  while ( u2_nul != lit ) {
    c3_w byt_w = u2_bi_byte(wir_r, 0, u2_h(lit));

    mpz_mul_ui(mp, mp, 16);
    if ( (byt_w >= 'a') && (byt_w <= 'f') ) {
      mpz_add_ui(mp, mp, (byt_w + 10 - 'a'));
    }
    else {
      mpz_add_ui(mp, mp, (byt_w - '0'));
    }
    lit = u2_t(lit);
  }
  return u2_bn_mp(wir_r, mp);
}

/* u2_bn_trel():
**
**   Produce the triple [a b c].
*/
u2_noun
u2_bn_trel(u2_ray  wir_r,
           u2_noun a,
           u2_noun b,
           u2_noun c)
{
  return u2_bl_good(wir_r, u2_rl_trel(wir_r, a, b, c));
}

/* u2_bn_words():
**
**   Copy [a] words from [b] into an atom.
*/
u2_noun
u2_bn_words(u2_ray      wir_r,
            c3_w        a_w,
            const c3_w* b_w)
{
  return u2_bl_good(wir_r, u2_rl_words(wir_r, a_w, b_w));
}

/* u2_bn_slab():
**
**   Create an atomic slab of `len` words.
*/
u2_ray
u2_bn_slab(u2_wire wir_r,
           c3_w    len_w)
{
  return u2_bl_some(wir_r, u2_rl_slab(wir_r, len_w));
}

/* u2_bn_slaq():
**
**   Create an atomic slab of `len` bloqs of size `met`.
*/
u2_ray
u2_bn_slaq(u2_wire wir_r,
           c3_g    met_g,
           c3_w    len_w)
{
  return u2_bn_slab(wir_r, ((len_w << met_g) + 31) >> 5);
}

/* u2_bi_cell():
**
**   Factor `a` as a cell `[b c].
*/
void
u2_bi_cell(u2_wire  wir_r,
           u2_noun  a,
           u2_noun* b,
           u2_noun* c)
{
  if ( u2_no == u2_as_cell(a, b, c) ) {
    u2_bl_bail(wir_r, c3__exit);
  }
}

/* u2_bi_qual():
**
**   Factor `a` as a quadruple `[b c d e]`.
*/
void
u2_bi_qual(u2_wire  wir_r,
           u2_noun  a,
           u2_noun* b,
           u2_noun* c,
           u2_noun* d,
           u2_noun* e)
{
  if ( u2_no == u2_as_qual(a, b, c, d, e) ) {
    u2_bl_bail(wir_r, c3__exit);
  }
}

/* u2_bi_quil():
**
**   Factor `a` as a quintuple `[b c d e f]`, or bail.
*/
void
u2_bi_quil(u2_wire  wir_r,
           u2_noun  a,
           u2_noun* b,
           u2_noun* c,
           u2_noun* d,
           u2_noun* e,
           u2_noun* f)
{
  if ( u2_no == u2_as_quil(a, b, c, d, e, f) ) {
    u2_bl_bail(wir_r, c3__exit);
  }
}

/* u2_bi_trel():
**
**   Factor `a` as a trel `[b c d]`, or bail.
*/
void
u2_bi_trel(u2_wire  wir_r,
           u2_noun  a,
           u2_noun* b,
           u2_noun* c,
           u2_noun* d)
{
  if ( u2_no == u2_as_trel(a, b, c, d) ) {
    u2_bl_bail(wir_r, c3__exit);
  }
}

