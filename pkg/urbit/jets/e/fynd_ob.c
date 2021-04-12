/* j/3/fein.c
**
*/
#include "all.h"
#include <murmur3.h>

//  +tail:ob constant parameters to +fe:ob
//
static const c3_w a_w =     0xffff;
static const c3_w b_w =    0x10000;
static const c3_w k_w = 0xffff0000;

//  (flop raku:ob)
//
static const c3_w kar_w[4] = { 0x4b387af7, 0x85bcae01, 0xee281300, 0xb76d5eed };

/* _fen_ob(): +fen:ob, with constant parameters factored out.
**           correct over the domain [0x0 ... 0xfffe.ffff]
*/
static c3_w
_fen_ob(c3_w m_w)
{
  c3_w l_w = m_w / a_w;
  c3_w r_w = m_w % a_w;
  c3_w f_w, t_w;
  c3_y j_y, k_y[2];

  //  legendary @max19
  //
  if ( a_w == l_w ) {
    t_w = l_w;
    l_w = r_w;
    r_w = t_w;
  }

  for ( j_y = 0; j_y < 4; j_y++ ) {
    k_y[0] = l_w & 0xff;
    k_y[1] = (l_w >> 8) & 0xff;

    MurmurHash3_x86_32(k_y, 2, kar_w[j_y], &f_w);

    t_w = ( j_y & 1 )
          ? ((r_w + a_w) - (f_w % a_w)) % a_w
          : ((r_w + b_w) - (f_w % b_w)) % b_w;
    r_w = l_w;
    l_w = t_w;
  }

  return (r_w * a_w) + l_w;
}

/* _tail_ob(): +feis:ob, also offsetting by 0x1.000 (as in +fynd:ob).
**             correct over the domain [0x1.0000, 0xffff.ffff]
*/
static c3_w
_tail_ob(c3_w m_w)
{
  c3_w c_w = _fen_ob(m_w - b_w);
  return b_w + (( c_w < k_w ) ? c_w : _fen_ob(c_w));
}

u3_atom
u3qe_fynd_ob(u3_atom pyn)
{
  c3_w sor_w = u3r_met(4, pyn);

  if ( (sor_w < 2) || (sor_w > 4) ) {
    return u3k(pyn);
  }

  if ( 2 == sor_w ) {
    return u3i_word(_tail_ob(u3r_word(0, pyn)));
  }
  else {
    c3_w pyn_w[2];
    u3r_words(0, 2, pyn_w, pyn);

    if ( pyn_w[0] < b_w ) {
      return u3k(pyn);
    }
    else {
      pyn_w[0] = _tail_ob(pyn_w[0]);
      return u3i_words(2, pyn_w);
    }
  }
}

u3_noun
u3we_fynd_ob(u3_noun cor)
{
  return u3qe_fynd_ob(u3x_atom(u3x_at(u3x_sam, cor)));
}
