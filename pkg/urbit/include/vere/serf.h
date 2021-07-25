/* include/vere/serf.h
*/

  /** Data types.
  **/
    /* u3_cal: reclaim cache status
    */
    typedef enum {
      u3_no_reclaim = 0,
      u3_reclaim_partial = 1,
      u3_reclaim_all = 2
    } u3_cal;

    /* u3_serf: worker-process state
    */
      typedef struct _u3_serf {
        c3_d    key_d[4];          //  disk key
        c3_c*   dir_c;             //  execution directory (pier)
        c3_d    sen_d;             //  last event requested
        c3_d    dun_d;             //  last event processed
        c3_l    mug_l;             //  hash of state
        c3_o    pac_o;             //  pack kernel
        u3_cal  rec_o;             //  reclaim cache
        c3_o    mut_o;             //  mutated kerne
        u3_noun sac;               //  space measurementl
        void  (*xit_f)(void);      //  exit callback
      } u3_serf;

  /** Functions.
  **/
    /* u3_serf_init(): init or restore, producing status.
    */
      u3_noun
      u3_serf_init(u3_serf* sef_u);

    /* u3_serf_writ(): apply writ [wit], producing plea [*pel] on c3y.
    */
      c3_o
      u3_serf_writ(u3_serf* sef_u, u3_noun wit, u3_noun* pel);

    /* u3_serf_live(): apply %live command [com], producing *ret on c3y.
    */
      c3_o
      u3_serf_live(u3_serf* sef_u, u3_noun com, u3_noun* ret);

    /* u3_serf_peek(): read namespace.
    */
      u3_noun
      u3_serf_peek(u3_serf* sef_u, c3_w mil_w, u3_noun sam);

    /* u3_serf_play(): apply event list, producing status.
    */
      u3_noun
      u3_serf_play(u3_serf* sef_u, c3_d eve_d, u3_noun lit);

    /* u3_serf_work(): apply event, producing effects.
    */
      u3_noun
      u3_serf_work(u3_serf* sef_u, c3_w mil_w, u3_noun job);

    /* u3_serf_post(): update serf state post-writ.
    */
      void
      u3_serf_post(u3_serf* sef_u);

    /* u3_serf_grab(): garbage collect.
    */
      void
      u3_serf_grab(void);
