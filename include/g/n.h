/* include/g/n.h
**
** This file is in the public domain.
*/
    /**  Generic computation.
    **/
      /* u2_cn_nock_on(): produce .*(bus fol).
      */
        u2_noun
        u2_cn_nock_on(u2_noun bus, u2_noun fol);

      /* u2_cn_slam_on(): produce (gat sam).
      */
        u2_noun
        u2_cn_slam_on(u2_noun gat, u2_noun sam);

      /* u2_cn_kick_on(): fire `gat` without changing the sample.
      */
        u2_noun
        u2_cn_kick_on(u2_noun gat);

      /* u2_cn_nock_un(): produce .*(bus fol), as ++toon.
      */
        u2_noun
        u2_cn_nock_un(u2_noun bus, u2_noun fol);

      /* u2_cn_slam_un(): produce (gat sam), as ++toon.
      */
        u2_noun
        u2_cn_slam_un(u2_noun gat, u2_noun sam);

      /* u2_cn_nock_in(): produce .*(bus fol), as ++toon, in namespace.
      */
        u2_noun
        u2_cn_nock_in(u2_noun fly, u2_noun bus, u2_noun fol);

      /* u2_cn_slam_in(): produce (gat sam), as ++toon, in namespace.
      */
        u2_noun
        u2_cn_slam_in(u2_noun fly, u2_noun gat, u2_noun sam);

      /* u2_cn_nock_an(): as slam_in(), but with empty fly.
      */
        u2_noun
        u2_cn_nock_an(u2_noun bus, u2_noun fol);



