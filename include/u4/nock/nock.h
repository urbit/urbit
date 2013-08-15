/* include/nock/nock.h
**
** This file is in the public domain.
*/
  /** Data structures.
  **/
    /* u4_og: thread execution context.
    */
      typedef struct u4_og_struct {
        /* Allocation road.
        */
        u4_road road;

        /* Jet table - [formula [jato bole]].
        */
        u4_noun daz;
      } *u4_og;


  /** Functions.
  **/
    /* u4_nock_init(): 
    **
    **   Create thread.
    */
      u4_og
      u4_nock_init(void);

    /* u4_nock_pure():
    **
    **   As simply as possible, produce nock(subject formula) on (lane).
    */
      u4_noun
      u4_nock_pure(u4_lane lane,
                   u4_noun subject,
                   u4_noun formula);

    /* u4_nock_jet():
    **
    **   Accelerate (nur gom), a chip, with (vig), a jet code.
    **
    **     nur: bole.
    **     gom: formula.
    */
      void
      u4_nock_jet(u4_og    g,
                  u4_atom vig,
                  u4_noun nur,
                  u4_noun gom);
      
    /* u4_nock_fast():
    **
    **   Accelerating but still using the C stack, produce 
    **   nock(lan sef) on the box of (g->road).
    */
      u4_noun
      u4_nock_fast(u4_og    g,
                   u4_noun lan,
                   u4_noun sef);
