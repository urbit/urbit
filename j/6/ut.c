/* j/6/ut.c
**
** This file is in the public domain.
*/
#include "all.h"


  //  duck: create a duck core for mean.
  //
  u2_noun
  u2_cqfu_duck(
                        u2_noun     van,
                        u2_noun     typ)
  {
    u2_noun von = u2_ci_molt(u2k(van), u2_cv_sam, u2k(typ), 0);
    u2_noun ret = u2_cj_hook(u2k(von), "dune");

    u2z(von);
    return ret;
  }

  //  dung: create a dunk core for mean (noun caption)
  //
  u2_noun
  u2_cqfu_dung(
                        u2_noun     van,
                        u2_noun     paz,
                        u2_noun     typ)
  {
    u2_noun von = u2_ci_molt(u2k(van), u2_cv_sam, u2k(typ), 0);
    u2_noun duq = u2_cj_hook(u2k(von), "dunk");
    u2_noun ret = u2_ci_molt(u2k(duq), u2_cv_sam, u2k(paz), 0);

    u2z(duq);
    u2z(von);
    return ret;
  }

  //  dunq: create a dunk core for mean
  //
  u2_noun
  u2_cqfu_dunq(
                        u2_noun     van,
                        const c3_c* paz_c,
                        u2_noun     typ)
  {
    u2_noun von = u2_ci_molt(u2k(van), u2_cv_sam, u2k(typ), 0);
    u2_noun duq = u2_cj_hook(u2k(von), "dunk");
    u2_noun paz = u2_ci_string(paz_c);
    u2_noun ret = u2_ci_molt(u2k(duq), u2_cv_sam, u2k(paz), 0);

    u2z(paz);
    u2z(duq);
    u2z(von);
    return ret;
  }

  //  shew: create a show core for mean
  //
  u2_noun
  u2_cqfu_shew(
                        u2_noun van,
                        u2_noun mol)
  {
    u2_noun sho = u2_cj_hook(u2k(van), "show");
    u2_noun ret = u2_ci_molt(u2k(sho), u2_cv_sam, u2k(mol), 0);

    u2z(sho);
    u2z(mol);
    return ret;
  }

  //  shep: show with caption and style
  //
  u2_noun
  u2_cqfu_shep(
                        u2_noun     van,
                        const c3_c* paz_c,
                        u2_noun     sty,
                        u2_noun     mol)
  {
    return u2_cqfu_shew
      (van,
              u2nc
                (u2nc('c', u2_ci_string(paz_c)),
                        u2nc(u2k(sty), mol)));
  }

