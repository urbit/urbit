#ifndef U3_VERE_MARS_H
#define U3_VERE_MARS_H

  /** Data types.
  **/
    /* u3_gift: mars pending reponse.
    */
      typedef struct _u3_gift {
        struct _u3_gift* nex_u;           //  next response
        c3_d             len_d;           //  length
        c3_y*            hun_y;           //  bytes
        enum {                            //  type
          u3_gift_fact_e = 0,             //    effects
          u3_gift_rest_e = 1              //    any
        } sat_e;                          //
        union {                           //  data
          c3_d           eve_d;           //    event number
          void*          ptr_v;           //    any
        };
      } u3_gift;

    /* u3_mars: the urbit state machine.
    */
      typedef struct _u3_mars {
        c3_d    key_d[4];                 //  disk key
        c3_c*   dir_c;                    //  execution directory (pier)
        c3_d    sen_d;                    //  last event requested
        c3_d    dun_d;                    //  last event processed
        c3_l    mug_l;                    //  hash of state
        c3_o    pac_o;                    //  pack kernel
        c3_o    rec_o;                    //  reclaim cache
        c3_o    mut_o;                    //  mutated kerne
        u3_noun sac;                      //  space measurement
        u3_disk* log_u;                   //  event log
        u3_meta met_u;                    //  metadata
        struct {                          //  snapshot
          uv_timer_t tim_u;               //    timer
          c3_d       eve_d;               //    last saved
        } sav_u;                          //
        u3_moat*     inn_u;               //  input stream
        u3_mojo*     out_u;               //  output stream
        u3_cue_xeno* sil_u;               //  cue handle
        enum {                            //  state
          u3_mars_work_e = 0,             //    working
          u3_mars_save_e = 1,             //    snapshotting
          u3_mars_exit_e = 2              //    exiting
        } sat_e;                          //
        struct {                          //  response queue
          u3_gift* ent_u;                 //    entry
          u3_gift* ext_u;                 //    exit
        } gif_u;                          //
        void  (*xit_f)(void);             //  exit callback
      } u3_mars;

  /** Functions.
  **/
    /* u3_mars_boot(): boot a new ship.
    */
      c3_o
      u3_mars_boot(c3_c* dir_c, u3_noun com);

    /* u3_mars_grab(): garbage collect.
    */
      void
      u3_mars_grab(void);

    /* u3_mars_init(): restart an existing ship.
    */
      u3_mars*
      u3_mars_init(c3_c*    dir_c,
                   u3_moat* inn_u,
                   u3_mojo* out_u,
                   c3_d     eve_d);

    /* u3_mars_kick(): try to send a task into mars.
    */
      c3_o
      u3_mars_kick(u3_mars* mar_u, c3_d len_d, c3_y* hun_y);

    /* u3_mars_grab(): garbage collect.
    */
      void
      u3_mars_grab(void);

#endif /* ifndef U3_VERE_MARS_H */
