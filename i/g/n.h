/* include/g/n.h
**
** This file is in the public domain.
*/
  /**  Functions.
  **/
    /* u3_cn_nock_on(): produce .*(bus fol).
    */
      u3_noun
      u3_cn_nock_on(u3_noun bus, u3_noun fol);

    /* u3_cn_slam_on(): produce (gat sam).
    */
      u3_noun
      u3_cn_slam_on(u3_noun gat, u3_noun sam);

    /* u3_cn_kick_on(): fire `gat` without changing the sample.
    */
      u3_noun
      u3_cn_kick_on(u3_noun gat);

    /* u3_cn_nock_un(): produce .*(bus fol), as ++toon.
    */
      u3_noun
      u3_cn_nock_un(u3_noun bus, u3_noun fol);

    /* u3_cn_slam_un(): produce (gat sam), as ++toon.
    */
      u3_noun
      u3_cn_slam_un(u3_noun gat, u3_noun sam);

    /* u3_cn_nock_in(): produce .*(bus fol), as ++toon, in namespace.
    */
      u3_noun
      u3_cn_nock_in(u3_noun fly, u3_noun bus, u3_noun fol);

    /* u3_cn_slam_in(): produce (gat sam), as ++toon, in namespace.
    */
      u3_noun
      u3_cn_slam_in(u3_noun fly, u3_noun gat, u3_noun sam);

    /* u3_cn_nock_an(): as slam_in(), but with empty fly.
    */
      u3_noun
      u3_cn_nock_an(u3_noun bus, u3_noun fol);
