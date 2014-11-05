/* include/g/v.h
**
** This file is in the public domain.
*/
  /**  Data structures.
  **/
    /* u3_cart: ovum carton.
    */
      struct _u3_cs_arvo;

      typedef struct _u3_cs_cart {
        u3_noun                 vir;      //  effects of ovum
        u3_bean                 did;      //  cart considered for commit?
        u3_bean                 cit;      //  cart committed?
        c3_d                    ent_d;    //  entry in raft queue?
        u3p(struct _u3_cs_cart) nex_p;
      } u3_cs_cart;

    /* u3_cs_arvo: modern arvo structure.
    */
      typedef struct _u3_cs_arvo {
        c3_d    ent_d;                    //  event number
        u3_noun yot;                      //  cached gates
        u3_noun now;                      //  current time, as noun
        u3_noun wen;                      //  current time, as text
        u3_noun sev_l;                    //  instance number
        u3_noun sen;                      //  instance string
        u3_noun own;                      //  owner list

        u3_noun roe;                      //  temporary unsaved events
        u3_noun key;                      //  log key, or 0

        u3_noun ken;                      //  kernel formula
        u3_noun roc;                      //  kernel core

        struct {                          //  ova waiting to process
          u3p(u3_cs_cart) egg_p;          //  exit of ovum queue
          u3p(u3_cs_cart) geg_p;          //  entry of ovum queue
        } ova;
      } u3_cs_arvo;

    /* u3_cs_home: all internal (within image) state. 
    */
      typedef struct _u3_cs_home {
        u3_cs_road rod_u;                   //  storage state
        u3_cs_arvo arv_u;                   //  arvo state
      } u3_cs_home;


  /**  Globals.
  **/
    /* u3_Home / u3H: root of thread.
    */
      c3_global u3_cs_home* u3_Home;
#       define u3H  u3_Home
#       define u3A  (&(u3_Home->arv_u))

  /**  Functions.
  **/
    /* u3_cv_do(): use a kernel function.
    */
      u3_noun
      u3_cv_do(const c3_c* txt_c, u3_noun arg);

    /* u3_cv_make(): make a new pier by loading a pill.
    */
      void
      u3_cv_make(c3_c* pas_c);

    /* u3_cv_jack(): execute kernel formula to bind jets.
    */
      void
      u3_cv_jack(void);

    /* u3_cv_start(): start time.
    */
      void
      u3_cv_start(u3_noun now);

    /* u3_cv_arm(): load a kernel arm.
    */
      u3_noun
      u3_cv_arm(const c3_c* txt_c);

    /* u3_cv_pike(): poke with floating core.
    */
      u3_noun
      u3_cv_pike(u3_noun ovo, u3_noun cor);

    /* u3_cv_nick(): transform enveloped packets, [vir cor].
    */
      u3_noun
      u3_cv_nick(u3_noun vir, u3_noun cor);

    /* u3_cv_do(): use a kernel function.
    */
      u3_noun
      u3_cv_do(const c3_c* txt_c, u3_noun arg);

    /* u3_cv_wish(): text expression with cache.
    */
      u3_noun
      u3_cv_wish(const c3_c* str_c);

    /* u3_cv_numb(): set the instance number.
    */
      void
      u3_cv_numb(void);

    /* u3_cv_time(): set the reck time.
    */
      void
      u3_cv_time(u3_noun now);

    /* u3_cv_peek(): query the reck namespace.
    */
      u3_noun
      u3_cv_peek(u3_noun hap);

    /* u3_cv_keep(): measure timer.
    */
      u3_noun
      u3_cv_keep(u3_noun hap);

    /* u3_cv_poke(): insert and apply an input ovum (protected).
    */
      u3_noun
      u3_cv_poke(u3_noun ovo);

    /* u3_cv_http_request(): hear http request on channel (unprotected).
    */
      void
      u3_cv_http_request(u3_bean sec, u3_noun pox, u3_noun req);

    /* u3_cv_tank(): dump single tank.
    */
      void
      u3_cv_tank(u3_noun blu, c3_l tab_l, u3_noun tac);

    /* u3_cv_punt(): dump tank list.
    */
      void
      u3_cv_punt(u3_noun blu, c3_l tab_l, u3_noun tac);

    /* u3_cv_sway(): print trace.
    */
      void
      u3_cv_sway(u3_noun blu, c3_l tab_l, u3_noun tax);

    /* u3_cv_plan(): queue ovum (external).
    */
      void
      u3_cv_plan(u3_noun pax, u3_noun fav);

    /* u3_cv_plow(): queue multiple ova (external).
    */
      void
      u3_cv_plow(u3_noun ova);

    /* u3_cv_hose(): clear initial ovum queue.
    */
      void
      u3_cv_hose(void);

    /* u3_cv_louse(): last-minute deviltry upon a bail.
    */
      void
      u3_cv_louse(c3_m how_m);

    /* u3_cv_mark(): mark arvo kernel.
    */
      void
      u3_cv_mark(void);
