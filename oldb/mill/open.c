/* mill/open.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

#define _open_p(flot) \
  if ( u4_b_p(gen, u4_atom_##flot, &p_gen) ) { \
    u4_loaf _mill_q_##flot(u4_milr, u4_noun); \
    return _mill_q_##flot(m, p_gen); \
  }

#define _open_pq(flot) \
  if ( u4_b_pq(gen, u4_atom_##flot, &p_gen, &q_gen) ) { \
    u4_loaf _mill_q_##flot(u4_milr, u4_noun, u4_noun); \
    return _mill_q_##flot(m, p_gen, q_gen); \
  }

#define _open_pqr(flot) \
  if ( u4_b_pqr(gen, u4_atom_##flot, &p_gen, &q_gen, &r_gen) ) {\
    u4_loaf _mill_q_##flot(u4_milr, u4_noun, u4_noun, u4_noun); \
    return _mill_q_##flot(m, p_gen, q_gen, r_gen); \
  }

#define _open_pqrs(flot) \
  if ( u4_b_pqrs(gen, u4_atom_##flot, &p_gen, &q_gen, &r_gen, &s_gen) ) {\
    u4_loaf _mill_q_##flot(u4_milr, u4_noun, u4_noun, u4_noun, u4_noun); \
    return _mill_q_##flot(m, p_gen, q_gen, r_gen, s_gen); \
  }


/* _open_in(): mold expansion, static.
*/
static u4_mold
_open_in(u4_milr m,
         u4_gene gen)
{
  u4_noun p_gen, q_gen, r_gen, s_gen;

  if ( u4_b_fork(gen, &p_gen, &q_gen) ) {
    return u4_stub;
  }
  else {
    _open_p   (bink);  // :^
    _open_p   (blem);  // ?!
    _open_pq  (blin);  // |*
    _open_p   (boce);  // ?&
    _open_pq  (cell);  // +-
    _open_pq  (colb);  // ~<
    _open_p   (crad);
    _open_p   (dant);  // ?|
    _open_pqr (drol);  // ||
    _open_pq  (feng);  // ?>
    _open_pq  (flic);  // |:
    _open_p   (flot);  // |-
    _open_p   (frag);
    _open_pq  (garc);  // :+
    _open_pq  (gleb);  // |~
    _open_pqr (grad);  // :^
    _open_pq  (gril);  // ?-
    _open_pq  (grun);  // :%
    _open_p   (lome);  // |=
    _open_pq  (lorb);  // ^:
    // _open_pq  (mang);  // :~ 
    _open_pqr (marn);  // |.
    _open_p   (palt);
    _open_pq  (parq);  // :|
    _open_pq  (port);
    _open_pqr (pont);  // =>
    _open_pq  (prec);  // -<
    _open_pq  (prox);  // ?<
    _open_pqrs(qual);  // +%
    _open_p   (rack);  // +~
    _open_p   (rald);  // ^*
    _open_p   (read);  // :.
    _open_pqr (rulf);  // =<
    _open_pq  (stam);  // ?+
    _open_pqr (trel);  // +:
    _open_pq  (trop);  // ->

    return gen;
  }
}

/* _mill_open(): mold expansion.
*/
u4_mold 
_mill_open(u4_milr m,
           u4_gene gen)
{
  u4_nopt zax = u4_tab_get(gen, m->pon);

  if ( u4_bull != zax ) {
    return zax;
  }
  else {
    zax = _open_in(m, gen);

    m->pon = u4_tab_add(m->lane, gen, zax, m->pon);
    return zax;
  }
}

