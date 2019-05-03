/* include/g/v.h
**
** This file is in the public domain.
*/
  /**  Data structures.
  **/
    /* u3v_arvo: modern arvo structure.
    */
      typedef struct _u3v_arvo {
        c3_d    ent_d;                    //  event number
        u3_noun yot;                      //  cached gates
        u3_noun now;                      //  current time, as noun
        u3_noun wen;                      //  current time, as text
        u3_noun sev_l;                    //  instance number
        u3_noun sen;                      //  instance string
        u3_noun our;                      //  identity
        u3_noun fak;                      //  c3y is fake
        u3_noun roc;                      //  kernel core
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
    /* u3v_boot(): evaluate boot sequence, making a kernel
    */
      c3_o
      u3v_boot(u3_noun eve);

    /* u3v_boot_lite(): light bootstrap sequence, just making a kernel.
    */
      c3_o
      u3v_boot_lite(u3_noun lit);

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

    /* u3v_poke(): insert and apply an input ovum (protected).
    */
      u3_noun
      u3v_poke(u3_noun ovo);

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

    /* u3v_mark(): mark arvo kernel.
    */
      c3_w
      u3v_mark(FILE* fil_u);
