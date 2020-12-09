/* j/5/mule.c
**
*/
#include "all.h"

u3_noun
u3we_mule(u3_noun cor)
{
  u3_noun hok = u3j_cook("u3we_mule-mute", u3k(cor), "mute");


  //  this takes advantage of the fact that +mute's result is
  //  identical to that of +mule, and safely produces a statically-typed
  //  value while only evaluating the trap once.
  //
  return u3n_slam_on(hok, u3k(u3x_at(u3x_sam, cor)));
}
