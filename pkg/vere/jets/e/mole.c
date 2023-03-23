/* j/5/mule.c
**
*/
#include "all.h"

u3_noun
u3we_mole(u3_noun cor)
{
  u3_noun hok = u3j_cook("u3we_mole-mure", u3k(cor), "mure");

  //  just like +mule and +mute, this takes advantage of the fact that
  //  +mure's result is identical to that of +mole, and safely produces
  //  a statically-typed value while only evaluating the trap once.
  //
  return u3n_slam_on(hok, u3k(u3x_at(u3x_sam, cor)));
}
