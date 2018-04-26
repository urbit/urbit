/* i/n/n.h
**
** This file is in the public domain.
*/
  /** Data structures.
  ***
  **/

  typedef struct {
    c3_l    sip_l;
    u3_noun key;
  } u3n_memo;

  typedef struct _u3n_prog {
    struct {
      c3_o      own_o;
      c3_w      len_w;
      c3_y*     ops_y;
    } byc_u;
    struct {
      c3_w      len_w;
      u3_noun*  non;
    } lit_u;
    struct {
      c3_w      len_w;
      u3n_memo* sot_u;
    } mem_u;
    struct {
      c3_w      len_w;
      u3j_site* sit_u;
    } cal_u;
    struct {
      c3_w      len_w;
      u3j_rite* rit_u;
    } reg_u;
  } u3n_prog;

  /**  Functions.
  **/
    /* u3n_nock_on(): produce .*(bus fol).
    */
      u3_noun
      u3n_nock_on(u3_noun bus, u3_noun fol);

    /* u3n_find(): return prog for given formula. fol is RETAINED.
     */
      u3n_prog*
      u3n_find(u3_noun fol);

    /* u3n_burn(): execute u3n_prog with bus as subject.
     */
      u3_noun
      u3n_burn(u3_noun bus, u3n_prog* pog_u);

    /* u3n_slam_on(): produce (gat sam).
    */
      u3_noun
      u3n_slam_on(u3_noun gat, u3_noun sam);

    /* u3n_kick_on(): fire `gat` without changing the sample.
    */
      u3_noun
      u3n_kick_on(u3_noun gat);

    /* u3n_nock_in(): produce .*(bus fol), as ++toon, in namespace.
    */
      u3_noun
      u3n_nock_in(u3_noun fly, u3_noun bus, u3_noun fol);

    /* u3n_nock_it(): produce .*(bus fol), as ++toon, in namespace.
    */
      u3_noun
      u3n_nock_it(u3_noun sea, u3_noun bus, u3_noun fol);

    /* u3n_nock_et(): produce .*(bus fol), as ++toon, in namespace.
    */
      u3_noun
      u3n_nock_et(u3_noun gul, u3_noun bus, u3_noun fol);

    /* u3n_slam_in(): produce (gat sam), as ++toon, in namespace.
    */
      u3_noun
      u3n_slam_in(u3_noun fly, u3_noun gat, u3_noun sam);

    /* u3n_slam_it(): produce (gat sam), as ++toon, in namespace.
    */
      u3_noun
      u3n_slam_it(u3_noun sea, u3_noun gat, u3_noun sam);

    /* u3n_slam_et(): produce (gat sam), as ++toon, in namespace.
    */
      u3_noun
      u3n_slam_it(u3_noun gul, u3_noun gat, u3_noun sam);

    /* u3n_nock_an(): as slam_in(), but with empty fly.
    */
      u3_noun
      u3n_nock_an(u3_noun bus, u3_noun fol);

    /* u3n_beep(): promote bytecode state.
     */
      void
      u3n_beep(u3p(u3h_root) har_p);

    /* u3n_bark(): mark bytecode cache.
     */
      c3_w
      u3n_bark(void);

    /* u3n_bree(): free bytecode cache.
     */
      void
      u3n_bree(void);

    /* u3n_ream(): refresh after restoring from checkpoint.
    */
      void
      u3n_ream(void);
