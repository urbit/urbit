/* include/host.h
**
** This file is in the public domain.
*/
  /** Macros.
  **/
    /* Symbol composition.  Horrid.
    */
#     define _j2_xd(x)        j2_##x##_d
#     define _j2_xm(x)        j2_##x##_m
#     define _j2_xmc(x)       j2_##x##_mc
#     define _j2_xmy(x)       j2_##x##_my
#     define _j2_xmx(x)       j2_##x##_mx
#     define _j2_xmk(x)       j2_##x##_mk
#     define _j2_xmi(x)       j2_##x##_mi
#     define _j2_xmj(x)       j2_##x##_jets
#     define _j2_xmd(x)       j2_##x##_drivers
#     define _j2_xp(p, x)     j2_##x##_p_##p
#     define _j2_xpc(p, x)    j2_##x##_pc_##p
#     define _j2_xss(x)       #x
#     define _j2_xs(x)        _j2_xss(x)

#     define _j2_qd(x)        _j2_xd(x)
#     define _j2_qm(x)        _j2_xm(x)
#     define _j2_qmc(x)       _j2_xmc(x)
#     define _j2_qmy(x)       _j2_xmy(x)
#     define _j2_qmx(x)       _j2_xmx(x)
#     define _j2_qmi(x)       _j2_xmi(x)
#     define _j2_qmk(x)       _j2_xmk(x)
#     define _j2_qmd(x)       _j2_xmd(x)
#     define _j2_qmj(x)       _j2_xmj(x)
#     define _j2_qp(p, x)     _j2_xp(p, x)
#     define _j2_qpc(p, x)    _j2_xpc(p, x)

#     define _j2_a(a)                   a
#     define _j2_ab(a, b)               a##__##b
#     define _j2_abc(a, b, c)           a##__##b##__##c
#     define _j2_abcd(a, b, c, d)       a##__##b##__##c##__##d
#     define _j2_abcde(a, b, c, d, e)   a##__##b##__##c##__##d##__##e

#     define j2_sa(a)                   _j2_xs(_j2_a(a))
#     define j2_sb(a, b)                _j2_xs(_j2_ab(a, b))
#     define j2_sc(a, b, c)             _j2_xs(_j2_abc(a, b, c))
#     define j2_sd(a, b, c, d)          _j2_xs(_j2_abcd(a, b, c, d))
#     define j2_se(a, b, c, d, e)       _j2_xs(_j2_abcde(a, b, c, d, e))

#     define j2_da(a)                   _j2_qd(_j2_a(a))
#     define j2_db(a, b)                _j2_qd(_j2_ab(a, b))
#     define j2_dc(a, b, c)             _j2_qd(_j2_abc(a, b, c))
#     define j2_dd(a, b, c, d)          _j2_qd(_j2_abcd(a, b, c, d))
#     define j2_de(a, b, c, d, e)       _j2_qd(_j2_abcde(a, b, c, d, e))

#     define j2_ma(a)                   _j2_qm(_j2_a(a))
#     define j2_mb(a, b)                _j2_qm(_j2_ab(a, b))
#     define j2_mc(a, b, c)             _j2_qm(_j2_abc(a, b, c))
#     define j2_md(a, b, c, d)          _j2_qm(_j2_abcd(a, b, c, d))
#     define j2_me(a, b, c, d, e)       _j2_qm(_j2_abcde(a, b, c, d, e))

#     define j2_mac(a)                  _j2_qmc(_j2_a(a))
#     define j2_mbc(a, b)               _j2_qmc(_j2_ab(a, b))
#     define j2_mcc(a, b, c)            _j2_qmc(_j2_abc(a, b, c))
#     define j2_mdc(a, b, c, d)         _j2_qmc(_j2_abcd(a, b, c, d))
#     define j2_mec(a, b, c, d, e)      _j2_qmc(_j2_abcde(a, b, c, d, e))

#     define j2_may(a)                  _j2_qmy(_j2_a(a))
#     define j2_mby(a, b)               _j2_qmy(_j2_ab(a, b))
#     define j2_mcy(a, b, c)            _j2_qmy(_j2_abc(a, b, c))
#     define j2_mdy(a, b, c, d)         _j2_qmy(_j2_abcd(a, b, c, d))
#     define j2_mey(a, b, c, d, e)      _j2_qmy(_j2_abcde(a, b, c, d, e))

#     define j2_max(a)                  _j2_qmx(_j2_a(a))
#     define j2_mbx(a, b)               _j2_qmx(_j2_ab(a, b))
#     define j2_mcx(a, b, c)            _j2_qmx(_j2_abc(a, b, c))
#     define j2_mdx(a, b, c, d)         _j2_qmx(_j2_abcd(a, b, c, d))
#     define j2_mex(a, b, c, d, e)      _j2_qmx(_j2_abcde(a, b, c, d, e))

#     define j2_mai(a)                  _j2_qmi(_j2_a(a))
#     define j2_mbi(a, b)               _j2_qmi(_j2_ab(a, b))
#     define j2_mci(a, b, c)            _j2_qmi(_j2_abc(a, b, c))
#     define j2_mdi(a, b, c, d)         _j2_qmi(_j2_abcd(a, b, c, d))
#     define j2_mei(a, b, c, d, e)      _j2_qmi(_j2_abcde(a, b, c, d, e))

#     define j2_mak(a)                  _j2_qmk(_j2_a(a))
#     define j2_mbk(a, b)               _j2_qmk(_j2_ab(a, b))
#     define j2_mck(a, b, c)            _j2_qmk(_j2_abc(a, b, c))
#     define j2_mdk(a, b, c, d)         _j2_qmk(_j2_abcd(a, b, c, d))
#     define j2_mek(a, b, c, d, e)      _j2_qmk(_j2_abcde(a, b, c, d, e))

#     define j2_maj(a)                  _j2_qmj(_j2_a(a))
#     define j2_mbj(a, b)               _j2_qmj(_j2_ab(a, b))
#     define j2_mcj(a, b, c)            _j2_qmj(_j2_abc(a, b, c))
#     define j2_mdj(a, b, c, d)         _j2_qmj(_j2_abcd(a, b, c, d))
#     define j2_mej(a, b, c, d, e)      _j2_qmj(_j2_abcde(a, b, c, d, e))

#     define j2_mad(a)                  _j2_qmd(_j2_a(a))
#     define j2_mbd(a, b)               _j2_qmd(_j2_ab(a, b))
#     define j2_mcd(a, b, c)            _j2_qmd(_j2_abc(a, b, c))
#     define j2_mdd(a, b, c, d)         _j2_qmd(_j2_abcd(a, b, c, d))
#     define j2_med(a, b, c, d, e)      _j2_qmd(_j2_abcde(a, b, c, d, e))

#     define j2_pa(a, p)                _j2_qp(p, _j2_a(a))
#     define j2_pb(a, b, p)             _j2_qp(p, _j2_ab(a, b))
#     define j2_pc(a, b, c, p)          _j2_qp(p, _j2_abc(a, b, c))
#     define j2_pd(a, b, c, d, p)       _j2_qp(p, _j2_abcd(a, b, c, d))
#     define j2_pe(a, b, c, d, e, p)    _j2_qp(p, _j2_abcde(a, b, c, d, e))

#     define j2_pac(a, p)               _j2_qpc(p, _j2_a(a))
#     define j2_pbc(a, b, p)            _j2_qpc(p, _j2_ab(a, b))
#     define j2_pcc(a, b, c, p)         _j2_qpc(p, _j2_abc(a, b, c))
#     define j2_pdc(a, b, c, d, p)      _j2_qpc(p, _j2_abcd(a, b, c, d))
#     define j2_pec(a, b, c, d, e, p)   _j2_qpc(p, _j2_abcde(a, b, c, d, e))

  /** Types.
  **/
    /* u2_ho_fun: actual function.
    */
      typedef u2_noun (*u2_ho_fun)(u2_ray wir_r, u2_noun cor);

    /* Jet to hash selector.
    */
#       define u2_jet_fun_m(jet_j) (0x7fffffff & (c3_w)(c3_p)(jet_j))

    /* u2_ho_state: jet state beans
    */
    typedef c3_w u2_ho_state;

#       define u2_jet_live    0x1   // live: C jet active
#       define u2_jet_test    0x2   // test: C jet must be tested
#       define u2_jet_memo    0x4   // memo: memoize, even if jet is dead
#       define u2_jet_leak    0x8   // leak: check for memory leaks
#       define u2_jet_dead    0

    /* u2_ho_pear: mug-to-pointer binding.
    */
      typedef struct {
        u2_noun tag;
        void*   ptr_v;
      } u2_ho_pear;

    /* u2_ho_cash: mug-to-pointer cache.  Semantics match sham.
    */
      typedef struct {
        u2_ho_pear dol_p[16];
      } u2_ho_cash;

    /* u2_ho_jet: a C function, per formula.
    */
      typedef struct {
        /* Function control string - finds formula in core.  Declared.
        **
        **    ".@" | "hook"
        */
        const c3_c* fcs_c;

        /* Invocation model.  Declared:
        **
        **    c3__lite      //  does not bail
        **    c3__hevy      //  may bail
        */
        c3_m   vok_m;

        /* C function, on core.  Declared.
        */
        u2_noun (*fun_f)(u2_ray wir_r, u2_noun cor);

        /* State - see above.  Declared, but may change dynamically.
        */
        u2_ho_state sat_s;

        /* chip: battery identifier.  Computed (declare u2_none).
        */
        u2_noun xip;

        /* Tool: Nock formula.  Computed (declare u2_none).
        */
        u2_tool fol;

        /* Custom memoization key from core.  Declared - should reorder.
        */
        u2_noun (*key_f)(u2_ray wir_r, u2_noun cor);

        /* Memo selector.  Declared, with key function.
        */
        c3_m fun_m;

        /* Axis in battery.  Computed (declare 0).
        */
        c3_l axe_l;
      } u2_ho_jet;

    /* u2_ho_driver: battery driver.
    */
      typedef struct _u2_ho_driver {
        /* Control string - computed from seals.
        */
        const c3_c* cos_c;

        /* Function/formula jet array.  Null `fcs` terminates.
        */
        u2_ho_jet *fan_j;

        /* Subdriver array, if any.
        */
        struct _u2_ho_driver* sub_d;

        /* Mug: short hash of chip, or 0.  Must match if set.
        */
        c3_w mug_w;

        /* chip: battery identifier, from shed.
        */
        u2_noun xip;

        /* Cache from axe to jet.
        */
        u2_ho_cash jax_s;
      } u2_ho_driver;

    /* u2_ho_hangar: driver system.
    */
      typedef struct _u2_ho_hangar {
        /* Cache from chip to driver.
        */
        u2_ho_cash bad_s;

        /* Next hangar in stack.
        */
        struct _u2_ho_hangar *nex_h;
      } u2_ho_hangar;

  /** Globals.
  **/
    extern u2_ho_driver *HostDriverBase[];

  /** Functions.
  **/
    /* u2_ho_push():
    **
    **   Push a driver hangar (corresponding to a jet shed).
    */
      void
      u2_ho_push(void);

    /* u2_ho_popp():
    **
    **   Pop a driver hangar.
    */
      void
      u2_ho_popp(void);

    /* u2_ho_klar():
    **
    **   Clear and release all hangars.
    */
      void
      u2_ho_klar(void);

    /* u2_ho_cstring(): return malloced control string for `xip`.
    */
      c3_c*                                                       //  transfer
      u2_ho_cstring(u2_noun xip);                                 //  retain

    /* u2_ho_warn():
    **
    **   Report a warning at file and line.  This is assumed
    **   to have no semantic effect and negligible cost.
    */
      void
      u2_ho_warn(const c3_c* fil_c,
                 c3_w        lyn_w);

#     define u2_ho_warn_here() u2_ho_warn(__FILE__, __LINE__)

    /* u2_ho_test():
    **
    **   Report result of jet test on `cor`.  `pro` is fast; `vet` is slow.
    */
      void
      u2_ho_test(u2_wire    wir_r,
                 u2_ho_jet* jet_j,
                 u2_weak    cor,                                   //  retain
                 u2_weak    sof,                                   //  retain
                 u2_weak    had);                                  //  retain

    /* u2_ho_dive():
    **
    **   Report compatibility failure in `xip`, with subject `bus`.
    */
      void
      u2_ho_dive(u2_wire wir_r,
                 u2_noun xip,       //  retain
                 u2_noun bus);      //  retain

    /* u2_ho_fine():
    **
    **   Report test execution in `xip`, with subject `bus`.
    */
      void
      u2_ho_fine(u2_wire wir_r,
                 u2_noun xip,       //  retain
                 u2_noun bus);      //  retain

    /* u2_ho_use():
    **
    **   Run a jet.  Must be previously verified with u2_ho_nice().
    */
      u2_weak                                                     //  transfer
      u2_ho_use(u2_ray     wir_r,
                u2_ho_jet* jet_j,                                 //  retain
                u2_noun    cor,                                   //  retain
                u2_noun    fol);                                  //  retain

    /* u2_ho_nice():
    **
    **   Verify quickly that a chip's jet supports this core.
    **
    **   Only the outer battery is assumed to match.
    */
      u2_bean
      u2_ho_nice(u2_ray     wir_r,
                 u2_ho_jet* jet_j,
                 u2_noun    cor);

    /* u2_ho_punt():
    **
    **   Apply host nock driver on `xip`, `cor`, `fol`.
    */
      u2_weak
      u2_ho_punt(u2_ray  wir_r,
                 u2_noun xip,                                     //  retain
                 u2_noun cor,                                     //  retain
                 u2_noun fol);                                    //  retain

    /* u2_ho_kick():
    **
    **   Apply host nock driver on `xip`, `cor`, `fol`.
    */
      u2_weak                                                     //  produce
      u2_ho_kick(u2_ray   wir_r,
                 u2_noun  xip,                                    //  retain
                 u2_noun  cor,                                    //  retain
                 u2_atom  axe);                                   //  retain

    /* u2_ho_kicq(): as u2_ho_kick(), but mocky.
    */
      u2_noun                                                     //  produce
      u2_ho_kicq(u2_ray   wir_r,
                 u2_noun  xip,                                    //  retain
                 u2_noun  cor,                                    //  retain
                 u2_atom  axe,                                    //  retain
                 u2_bean  *pon);                                  //  retain
