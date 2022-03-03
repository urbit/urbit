/* j/3/fein.c
**
*/
#include "noun/all.h"
#include <murmur3.h>

//  +feis:ob constant parameters to +fe:ob
//
static const c3_w a_w =     0xffff;
static const c3_w b_w =    0x10000;
static const c3_w k_w = 0xffff0000;

//  +raku:ob
//
static const c3_w rak_w[4] = { 0xb76d5eed, 0xee281300, 0x85bcae01, 0x4b387af7 };

/* _fe_ob(): +fe:ob, with constant parameters factored out.
**           correct over the domain [0x0, 0xfffe.ffff]
*/
static c3_w
_fe_ob(c3_w m_w)
{
  c3_w l_w = m_w % a_w;
  c3_w r_w = m_w / a_w;
  c3_w f_w, t_w;
  c3_y j_y, k_y[2];

  for ( j_y = 0; j_y < 4; j_y++ ) {
    k_y[0] = r_w & 0xff;
    k_y[1] = (r_w >> 8) & 0xff;

    MurmurHash3_x86_32(k_y, 2, rak_w[j_y], &f_w);

    //  NB: this addition can overflow a c3_w (before mod)
    //
    t_w = ((c3_d)f_w + l_w) % (!(j_y & 1) ? a_w : b_w);
    l_w = r_w;
    r_w = t_w;
  }

  //  legendary @max19
  //
  return ( a_w == r_w )
         ? (r_w * a_w) + l_w
         : (l_w * a_w) + r_w;
}

/* _feis_ob(): +feis:ob, also offsetting by 0x1.000 (as in +fein:ob).
**             correct over the domain [0x1.0000, 0xffff.ffff]
*/
static c3_w
_feis_ob(c3_w m_w)
{
  c3_w c_w = _fe_ob(m_w - b_w);
  return b_w + (( c_w < k_w ) ? c_w : _fe_ob(c_w));
}

u3_atom
u3qe_fein_ob(u3_atom pyn)
{
  c3_w sor_w = u3r_met(4, pyn);

  if ( (sor_w < 2) || (sor_w > 4) ) {
    return u3k(pyn);
  }

  if ( 2 == sor_w ) {
    return u3i_word(_feis_ob(u3r_word(0, pyn)));
  }
  else {
    c3_w pyn_w[2];
    u3r_words(0, 2, pyn_w, pyn);

    if ( pyn_w[0] < b_w ) {
      return u3k(pyn);
    }
    else {
      pyn_w[0] = _feis_ob(pyn_w[0]);
      return u3i_words(2, pyn_w);
    }
  }
}

u3_noun
u3we_fein_ob(u3_noun cor)
{
  return u3qe_fein_ob(u3x_atom(u3x_at(u3x_sam, cor)));
}
