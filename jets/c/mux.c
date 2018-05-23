/* j/3/mux.c
**
*/
#include "all.h"

static u3_atom
_ham(void* has_u, u3_atom a)
{
  u3j_site *sit_u = has_u;
  return u3j_gate_slam(sit_u, a);
}

static u3_atom
_mam(void* mis_u, u3_atom a, u3_atom b)
{
  u3j_site *sit_u = mis_u;
  u3_noun sam = u3nc(a, b);
  return u3j_gate_slam(sit_u, sam);
}

static u3_noun
_mux_in(u3p(u3h_root) cax_p, u3_noun a,
        u3_atom (*hat)(void*, u3_atom), void* has_u,
        u3_atom (*mix)(void*, u3_atom, u3_atom), void* mis_u)
{
  u3_weak got = u3h_get(cax_p, a);
  u3_atom pro;

  if ( u3_none != got ) {
    pro = got;
  }
  else {
    if ( c3y == u3ud(a) ) {
      pro = hat(has_u, u3k(a));
    }
    else {
      u3_atom lef = _mux_in(cax_p, u3k(u3h(a)), hat, has_u, mix, mis_u),
              rit = _mux_in(cax_p, u3k(u3t(a)), hat, has_u, mix, mis_u);
      pro = mix(mis_u, lef, rit);
    }
    u3h_put(cax_p, a, u3k(pro));
  }

  u3z(a);
  return pro;
}

/* functions
*/
  u3_noun
  u3kc_mux_f(u3_noun a,
             u3_atom (*hat)(void*, u3_atom), void* has_u,
             u3_atom (*mix)(void*, u3_atom, u3_atom), void* mis_u)
  {
    u3p(u3h_root) cax_p = u3h_new();
    u3_noun pro = _mux_in(cax_p, a, hat, has_u, mix, mis_u);
    u3h_free(cax_p);
    return pro;
  }
  u3_noun
  u3qc_mux(u3_noun a, u3_noun hat, u3_noun mix)
  {
    u3_noun pro;
    u3j_site hat_u, mix_u;
    u3j_gate_prep(&hat_u, u3k(hat));
    u3j_gate_prep(&mix_u, u3k(mix));
    pro = u3kc_mux_f(u3k(a), _ham, &hat_u, _mam, &mix_u);
    u3j_gate_lose(&mix_u);
    u3j_gate_lose(&hat_u);
    return pro;
  }
  u3_noun
  u3wc_mux(u3_noun cor)
  {
    u3_noun sam;

    if ( u3_none == (sam = u3r_at(u3x_sam, cor)) ) {
      return u3m_bail(c3__exit);
    }
    else {
      u3_noun a, hat, mix;
      u3x_trel(sam, &a, &hat, &mix);
      return u3qc_mux(a, hat, mix);
    }
  }
