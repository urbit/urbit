/* j/6/ut.c
**
*/
#include "all.h"

  extern void
  u3_lo_tank(c3_l tab_l, u3_noun tac);


  //  duck: create a duck core for mean.
  //
  u3_noun
  u3qfu_duck(u3_noun     van,
             u3_noun     typ)
  {
    u3_noun von = u3i_molt(u3k(van), u3x_sam, u3k(typ), 0);
    u3_noun ret = u3j_hook(u3k(von), "dune");

    u3z(von);
    return ret;
  }

  //  dung: create a dunk core for mean (noun caption)
  //
  u3_noun
  u3qfu_dung(u3_noun     van,
             u3_noun     paz,
             u3_noun     typ)
  {
    u3_noun von = u3i_molt(u3k(van), u3x_sam, u3k(typ), 0);
    u3_noun duq = u3j_hook(u3k(von), "dunk");
    u3_noun ret = u3i_molt(u3k(duq), u3x_sam, u3k(paz), 0);

    u3z(duq);
    u3z(von);
    return ret;
  }

  //  dunq: create a dunk core for mean
  //
  u3_noun
  u3qfu_dunq(u3_noun     van,
             const c3_c* paz_c,
             u3_noun     typ)
  {
    u3_noun von = u3i_molt(u3k(van), u3x_sam, u3k(typ), 0);
    u3_noun duq = u3j_hook(u3k(von), "dunk");
    u3_noun paz = u3i_string(paz_c);
    u3_noun ret = u3i_molt(u3k(duq), u3x_sam, u3k(paz), 0);

    u3z(paz);
    u3z(duq);
    u3z(von);
    return ret;
  }

  //  dump: dump a type.
  //
  void
  u3qfu_dump(u3_noun     van,
             const c3_c* paz_c,
             u3_noun     typ)
  {
    c3_c* pfix_c = u3r_string((c3y == u3du(typ)) ? u3h(typ) : typ);
    c3_c ugh_c[1024];

    sprintf(ugh_c, "%s: %s: 0x%8x:", 
            paz_c, pfix_c, u3r_mug(typ));

    u3_lo_tank(0, u3n_kick_on(u3qfu_dunq(van, ugh_c, typ)));
  }

  //  shew: create a show core for mean
  //
  u3_noun
  u3qfu_shew(u3_noun van,
             u3_noun mol)
  {
    u3_noun sho = u3j_hook(u3k(van), "show");
    u3_noun ret = u3i_molt(u3k(sho), u3x_sam, u3k(mol), 0);

    u3z(sho);
    u3z(mol);
    return ret;
  }

  //  shep: show with caption and style
  //
  u3_noun
  u3qfu_shep(u3_noun     van,
             const c3_c* paz_c,
             u3_noun     sty,
             u3_noun     mol)
  {
    return u3qfu_shew(van,
                      u3nc(u3nc('c', u3i_string(paz_c)),
                           u3nc(u3k(sty), mol)));
  }

