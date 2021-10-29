/* vere/reck.c
**
** stub file containing a relic from a bygone era.
**
** convenience function to parse small atoms, used in
** eyre and khan. Should probably die some day.
*/
#include "all.h"
#include "vere/vere.h"

/* _reck_mole(): parse simple atomic mole.
*/
static u3_noun
_reck_mole(u3_noun  fot,
           u3_noun  san,
           c3_d*    ato_d)
{
  u3_noun uco = u3dc("slaw", fot, san);
  u3_noun p_uco, q_uco;

  if ( (c3n == u3r_cell(uco, &p_uco, &q_uco)) ||
       (u3_nul != p_uco) )
  {
    u3l_log("strange mole %s\n", u3r_string(san));

    u3z(fot); u3z(uco); return c3n;
  }
  else {
    *ato_d = u3r_chub(0, q_uco);

    u3z(fot); u3z(uco); return c3y;
  }
}

/* u3_reck_lily(): parse little atom.
*/
u3_noun
u3_reck_lily(u3_noun fot, u3_noun txt, c3_l* tid_l)
{
  c3_d ato_d;

  if ( c3n == _reck_mole(fot, txt, &ato_d) ) {
    return c3n;
  } else {
    if ( ato_d >= 0x80000000ULL ) {
      return c3n;
    } else {
      *tid_l = (c3_l) ato_d;

      return c3y;
    }
  }
}

