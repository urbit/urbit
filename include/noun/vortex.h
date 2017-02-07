/* include/g/v.h
**
** This file is in the public domain.
*/
  /**  Data structures.
  **/
    /* u3_cart: ovum carton.
    */
      struct _u3v_arvo;

      typedef struct _u3v_cart {
        u3_noun               vir;      //  effects of ovum
        c3_o                  did;      //  cart considered for commit?
        c3_o                  cit;      //  cart committed?
        c3_d                  ent_d;    //  event number
        u3p(struct _u3v_cart) nex_p;
      } u3v_cart;

    /* u3v_arvo: modern arvo structure.
    */
      typedef struct _u3v_arvo {
        c3_d    ent_d;                    //  event number
        u3_noun yot;                      //  cached gates
        u3_noun now;                      //  current time, as noun
        u3_noun wen;                      //  current time, as text
        u3_noun sev_l;                    //  instance number
        u3_noun sen;                      //  instance string
        u3_noun own;                      //  owner list

        u3_noun sac;                      //  space profiling

        u3_noun roe;                      //  temporary unsaved events
        u3_noun key;                      //  log key, or 0
        u3_noun sys;                      //  system pill

        u3_noun ken;                      //  kernel formula
        u3_noun roc;                      //  kernel core

        struct {                          //  ova waiting to process
          u3p(u3v_cart) egg_p;            //  exit of ovum queue
          u3p(u3v_cart) geg_p;            //  entry of ovum queue
        } ova;
      } u3v_arvo;

    /* u3v_home: all internal (within image) state. 
    */
      typedef struct _u3v_home {
        u3a_road rod_u;                   //  storage state
        u3v_arvo arv_u;                   //  arvo state
      } u3v_home;


  /**  Globals.
  **/
    /* u3_Home / u3H: root of thread.
    */
      c3_global u3v_home* u3v_Home;
#       define u3H  u3v_Home
#       define u3A  (&(u3v_Home->arv_u))

  /**  Functions.
  **/
    /* u3v_do(): use a kernel function.
    */
      u3_noun
      u3v_do(const c3_c* txt_c, u3_noun arg);

    /* u3v_boot(): make a new pier by loading a pill.
    */
      void
      u3v_boot(c3_c* pas_c);

    /* u3v_boot_lite(): light bootstrap sequence, just making a kernel.
    */
      void
      u3v_boot_lite(u3_noun lit);

    /* u3v_make(): make a new pier by loading a pill.
    */
      void
      u3v_make(c3_c* pas_c);

    /* u3v_jack(): execute kernel formula to bind jets.
    */
      void
      u3v_jack(void);

    /* u3v_start(): start time.
    */
      void
      u3v_start(u3_noun now);

    /* u3v_arm(): load a kernel arm.
    */
      u3_noun
      u3v_arm(const c3_c* txt_c);

    /* u3v_pike(): poke with floating core.
    */
      u3_noun
      u3v_pike(u3_noun ovo, u3_noun cor);

    /* u3v_nick(): transform enveloped packets, [vir cor].
    */
      u3_noun
      u3v_nick(u3_noun vir, u3_noun cor);

    /* u3v_do(): use a kernel function.
    */
      u3_noun
      u3v_do(const c3_c* txt_c, u3_noun arg);

    /* u3v_wish(): text expression with cache.
    */
      u3_noun
      u3v_wish(const c3_c* str_c);

    /* u3v_numb(): set the instance number.
    */
      void
      u3v_numb(void);

    /* u3v_time(): set the reck time.
    */
      void
      u3v_time(u3_noun now);

    /* u3v_peek(): query the reck namespace.
    */
      u3_noun
      u3v_peek(u3_noun hap);

    /* u3v_keep(): measure timer.
    */
      u3_noun
      u3v_keep(u3_noun hap);

    /* u3v_poke(): insert and apply an input ovum (protected).
    */
      u3_noun
      u3v_poke(u3_noun ovo);

    /* u3v_http_request(): hear http request on channel (unprotected).
    */
      void
      u3v_http_request(c3_o sec, u3_noun pox, u3_noun req);

    /* u3v_tank(): dump single tank.
    */
      void
      u3v_tank(u3_noun blu, c3_l tab_l, u3_noun tac);

    /* u3v_punt(): dump tank list.
    */
      void
      u3v_punt(u3_noun blu, c3_l tab_l, u3_noun tac);

    /* u3v_sway(): print trace.
    */
      void
      u3v_sway(u3_noun blu, c3_l tab_l, u3_noun tax);

    /* u3v_plan(): queue ovum (external).
    */
      void
      u3v_plan(u3_noun pax, u3_noun fav);

    /* u3v_plow(): queue multiple ova (external).
    */
      void
      u3v_plow(u3_noun ova);

    /* u3v_hose(): clear initial ovum queue.
    */
      void
      u3v_hose(void);

    /* u3v_louse(): last-minute deviltry upon a bail.
    */
      void
      u3v_louse(c3_m how_m);

    /* u3v_mark(): mark arvo kernel.
    */
      c3_w
      u3v_mark(void);
