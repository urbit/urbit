/* gen164/5/co_emco.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"


/* functions
*/

/* parameters
   [[bas=@ min=@] [par=$+([? @ tape] tape)]]
 */

#if 0
  u2_weak
  j2_md(Pt5, coco, co, emco)(
                             u2_noun cor)
  {
    u2_atom bas, min;
    u2_noun rex;
    u2_noun par;
    u2_atom hol = 0;
    mpz_t bas_mp, dar_mp, hol_mp, rad_mp;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_4, &bas,
                               u2_cv_sam_5, &min,
                               u2_cv_sam_3, &par,
                               u2_cv_sam_6, &rex,
                               0) )
      {
	return u2_cm_bail(c3__exit);
      }

    while ( 1 ) {
      if ( !hol && !min ) {
	return rex;
      }

      u2_cr_mp(bas_mp, bas);
      u2_cr_mp(hol_mp, hol);

      mpz_tdiv_q(rad_mp, hol_mp, bas_mp);
      mpz_tdiv_r(dar_mp, hol_mp, bas_mp);

      if ( min ) {
	min--;
      }

      u2_ci_mp(hol, dar_mp);

      //      rex  (par =(0 dar) rad rex)
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mdj(Pt5, coco, co, emco)[] = {
    { ".2", c3__lite, j2_md(Pt5, coco, co, emco), u2_jet_dead, u2_none, u2_none },
    { }
  };
#endif
