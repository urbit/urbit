/* mill/pike.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_pike: open pike.  *tes is the if test, *bif is the if yes.
*/
void
_mill_pike(u4_milr m,
           u4_rope mox, 
           u4_pike pik,
           u4_gene *tes,
           u4_gene *bif)
{
  u4_lane lane = m->lane;
  u4_form kel;
  u4_gene luf;

  // u4_err(lane, "pik", pik);
  // u4_err(lane, "mox", mox);

  if ( u4_b_pq(pik, u4_atom_lask, &kel, &luf) ) {
    u4_gene mip = _mill_grip(m, kel);
    u4_gene bul = _mill_lump(m, kel);
    u4_gene nek = u4_k_trel(lane, u4_atom_kick, mox, u4_noun_0);

    *tes = u4_k_trel(lane, u4_atom_like, mox, mip);
    *bif = u4_k_qual(lane, u4_atom_sure, bul, nek, luf);
  }
  else if ( u4_b_p(pik, u4_atom_plic, &kel) ) {
    u4_gene luf = u4_k_trel(lane, u4_atom_kick, mox, u4_noun_0);
    u4_gene mip = _mill_grip(m, kel);
    u4_gene bul = _mill_lump(m, kel);

    *tes = u4_k_trel(lane, u4_atom_like, mox, mip);
    *bif = u4_k_qual(lane, u4_atom_sure, bul, luf, luf);
  }
  else if ( u4_b_pq(pik, u4_atom_semp, &kel, &luf) ) {
    u4_gene mip = _mill_grip(m, kel);
    u4_gene hem = _mill_gate(m, kel);
    u4_gene dup = u4_k_trel(lane, u4_atom_kick, mox, u4_noun_0);
    u4_gene pof = u4_k_trel
      (lane, u4_atom_cast, u4_k_cell(lane, u4_atom_crad, u4_noun_0), dup);
    u4_gene vir = u4_k_trel(lane, u4_atom_mang, hem, pof);

    *tes = u4_k_trel(lane, u4_atom_like, mox, mip);
    *bif = u4_k_trel
        (lane,
         u4_atom_grun,
         u4_k_cell(lane, u4_k_cell(lane, mox, vir), u4_noun_0),
         luf);
  }
  else if ( u4_b_p(pik, u4_atom_fing, &kel) ) {
    u4_gene mip = _mill_grip(m, kel);
    u4_gene hem = _mill_gate(m, kel);
    u4_gene dup = u4_k_trel(lane, u4_atom_kick, mox, u4_noun_0);
    u4_gene pof = u4_k_trel
      (lane, u4_atom_cast, u4_k_cell(lane, u4_atom_crad, u4_noun_0), dup);
    u4_gene vir = u4_k_trel(lane, u4_atom_mang, hem, pof);

    *tes = u4_k_trel(lane, u4_atom_like, mox, mip);
    *bif = vir;
  }
  else if ( u4_b_p(pik, u4_atom_homp, &luf) ) {
    *tes = u4_k_cell(lane, u4_atom_rock, u4_noun_0);
    *bif = luf;
  }
  else u4_trip;
}
