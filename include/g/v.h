/* include/g/v.h
**
** This file is in the public domain.
*/
  /** Arvo macros.
  **/
#   define  u3_do(txt_c, arg)         u3_cv_do(txt_c, arg)
#   define  u3_dc(txt_c, a, b)        u3_cv_do(txt_c, u3nc(a, b))
#   define  u3_dt(txt_c, a, b, c)     u3_cv_do(txt_c, u3nt(a, b, c))
#   define  u3_dq(txt_c, a, b, c, d)  u3_cv_do(txt_c, u3nt(a, b, c, d))

  /** Arvo functions.  Rather unstructured.
  **/
    /* u3_cv_do(): use a kernel function.
    */
      u3_noun
      u3_cv_do(const c3_c* txt_c, u3_noun arg);

    /* u3_cv_make(): make a new pier by loading a pill.
    */
      void
      u3_cv_make(c3_c* pas_c, u3_noun now);

    /* u3_cv_gate(): load a kernel function.
    */
      u3_noun
      u3_cv_gate(const c3_c* txt_c);

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

    /* u3_cv_wish(): noun from expression.
    */
      u3_noun
      u3_cv_wish(c3_c* str_c);

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

    /* u3_cv_louse(): last-minute deviltry upon a bail.
    */
      void
      u3_cv_louse(c3_m how_m);
