/* j/6/ut_repo.c
**
*/
#include "all.h"

  static u3_noun
  _cqfu_repo(u3_noun van,
             u3_noun sut)
  {
    u3_noun von = u3i_molt(u3k(van), u3x_sam, u3k(sut), 0);

    return u3j_hook(von, "repo");
  }

  u3_noun
  u3qfu_repo(u3_noun van,
             u3_noun sut)
  {
    return _cqfu_repo(van, sut);
  }

  u3_noun
  u3wfu_repo(u3_noun cor)
  {
    u3_noun sut;

    if ( u3_none == (sut = u3r_at(u3x_sam, cor)) ) {
      return u3m_bail(c3__fail);
    } else {
      return u3qfu_repo(cor, sut);
    }
  }
