/* j/6/gain.c
**
** This file is in the public domain.
*/
#include "all.h"


  u3_noun
  u3_cqfu_gain(u3_noun van, u3_noun sut, u3_noun gen)
  {
    u3_noun von = u3_ci_molt(u3k(van), u3_cv_sam, u3k(sut), 0);
    u3_noun gat = u3_cj_hook(u3k(von), "gain");
    u3_noun dun = u3_cqfu_dunq(van, "type", sut);
    u3_noun pro;

    if ( u3_ne(u3_ca_audit(gen)) ){
      printf("bad gene!\r\n");
    }
    u3_ct_push(u3nc(c3__mean, dun));
    pro = u3_cn_kick_on(u3_ci_molt(gat, u3_cv_sam, u3k(gen), 0));

    u3_ct_drop();

    return pro;
  }
