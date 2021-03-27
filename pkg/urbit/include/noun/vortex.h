/* include/g/v.h
**
** This file is in the public domain.
*/
  /**  Data structures.
  **/
    /* u3v_arvo: modern arvo structure.
    **       NB: packed to perserve word alignment given [eve_d]
    */
      typedef struct __attribute__((__packed__)) _u3v_arvo {
        c3_d  eve_d;                      //  event number
        u3_noun yot;                      //  cached gates
        u3_noun now;                      //  current time
        u3_noun roc;                      //  kernel core
      } u3v_arvo;

    /* u3v_home: all internal (within image) state.
    **       NB: version must be last for discriminability in north road
    */
      typedef struct _u3v_home {
        u3a_road rod_u;                   //  storage state
        u3v_arvo arv_u;                   //  arvo state
        c3_w     ver_w;                   //  version number
      } u3v_home;


  /**  Globals.
  **/
    /* u3_Home / u3H: root of thread.
    */
      c3_global u3v_home* u3v_Home;
#       define u3H  u3v_Home
#       define u3A  (&(u3v_Home->arv_u))

  /** Constants.
  **/
#     define u3v_version 1

  /**  Functions.
  **/
    /* u3v_life(): execute initial lifecycle, producing Arvo core.
    */
      u3_noun
      u3v_life(u3_noun eve);

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

    /* u3v_time(): set the reck time.
    */
      void
      u3v_time(u3_noun now);

    /* u3v_peek(): query the reck namespace.
    */
      u3_noun
      u3v_peek(u3_noun hap);

    /* u3v_soft_peek(): softly query the reck namespace.
    */
      u3_noun
      u3v_soft_peek(c3_w mil_w, u3_noun sam);

    /* u3v_poke(): insert and apply an input ovum (protected).
    */
      u3_noun
      u3v_poke(u3_noun ovo);

    /* u3v_poke_sure(): inject an event, saving new state if successful.
    */
      u3_noun
      u3_poke_sure(c3_w mil_w, u3_noun eve);

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

    /* u3v_reclaim(): clear ad-hoc persistent caches to reclaim memory.
    */
      void
      u3v_reclaim(void);

    /* u3v_rewrite_compact(): rewrite arvo kernel for compaction.
    */
      void
      u3v_rewrite_compact();
