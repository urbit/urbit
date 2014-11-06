/* include/g/j.h
**
** This file is in the public domain.
*/
  /** Noun semantics.
  **/
#if 0
  ++  bane  ,@tas                                         ::  battery name
  ++  bash  ,@uvH                                         ::  ctx identity hash
  ++  bosh  ,@uvH                                         ::  local battery hash
  ++  batt  ,*                                            ::  battery
  ++  calx                                                ::  cached by battery
    $:  jax=,@ud                                          ::  jet index
        pax=,@ud                                          ::  parent axis or 0
        hap=(map ,@ud ,@ud)                               ::  axis/jet
        huc=(map term nock)                               ::  name/tool
    ==                                                    ::
  ++  clog  (pair cope (map batt (map term nock)))        ::  identity record
  ++  cope  (trel bane axis (each bash noun))             ::  core pattern
  ++  dash                                                ::  jet system
    $:  sys=(map batt bash)                               ::  battery/identity
        haw=(map bash clog)                               ::  identity/core
    ==                                                    ::
  ++  je   !:                                             ::  dashboard door
    |_  dash
    ++  fsck                                              ::  parse classic clue
      |=  clu=clue
      ^-  [p=term q=axis r=(map term nock)]
      :+  ?@  p.clu  `@tas`p.clu
          ?>  ?=([@ @] p.clu)
          (cat 3 -.p.clu (scot %ud +.p.clu))
        |-  ^-  axis
        ?:  ?=([10 *] q.clu)  $(q.clu +>.q.clu)
        ?:  ?=([1 0] q.clu)  0
        ?>  ?=([0 @] q.clu)  +.q.clu
      (~(gas by *(map term nock)) r.jlu)
    ::
    ++  fund                                              ::  register battery
      |=  [clu=clue cor=*]                                ::
      ^+  +>
      ?.  =(~ (find cor))  +>.$
      =+  cey=(fsck clu)
      =+  ^=  mop  ^-  (trel bane axis (each bash ,*))
          :-  p.cey
          ?:  =(0 q.cey)
            [3 %| -.cor]
          [q.cey %& (~(got by sys) -:.*([0 q.cey] cor))]
      =+  soh=(sham mop)
      =+  cag=(~(get by haw) soh)
      %=  +>.$
        sys  (~(put by sys) -.cor [soh r.cey])
        haw  %+  ~(put by haw)  soh 
             :-  mop
             ?~  cag 
               [[-.cor r.cey] ~ ~] 
             (~(put by q.u.cag) -.cor r.cey)
      ==
    -- 
#endif

  /** Data structures.
  ***
  *** All of these are transient structures allocated with malloc.
  **/
    /* u3e_harm: jet arm.
    */
      typedef struct _u3e_harm {
        c3_c*               fcs_c;             //  `.axe` or name
        u3_noun           (*fun_f)(u3_noun);   //  compute or 0 / semitransfer
        // u3_bean           (*val_f)(u3_noun);   //  validate or 0 / retain
        c3_o                ice;               //  perfect (don't test)
        c3_o                tot;               //  total (never punts)
        c3_o                liv;               //  live (enabled)
        c3_l                axe_l;             //  computed/discovered axis
        struct _u3e_core* cop_u;             //  containing core
      } u3e_harm;

    /* u3e_core: driver definition.
    */
      typedef struct _u3e_core {
        c3_c*               cos_c;      //  control string
        struct _u3e_harm* arm_u;      //  blank-terminated static list
        struct _u3e_core* dev_u;      //  blank-terminated static list
        struct _u3e_core* par_u;      //  dynamic parent pointer 
        c3_l                axe_l;      //  axis to parent
        c3_l                jax_l;      //  index in global dashboard
      } u3e_core;

    /* u3e_dash, u3_Dash, u3D: jet dashboard singleton
    */
      typedef struct _u3e_dash {
        u3e_core* dev_u;              //  null-terminated static list
        c3_l        len_l;              //  dynamic array length
        c3_l        all_l;              //  allocated length
        u3e_core* ray_u;              //  dynamic array by axis
      } u3e_dash;

  /** Globals.
  **/
    /* u3_Dash: jet dashboard.
    */
      extern u3e_dash u3_Dash;
#     define u3D u3_Dash


    /**  Functions.
    **/
      /* u3j_boot(): initialize jet system.
      */
        void
        u3j_boot(void);

      /* u3j_clear(): clear jet table to re-register.
      */
        void
        u3j_clear(void);

      /* u3j_hook():
      **
      **   Execute hook from core. 
      */
        u3_noun
        u3j_hook(u3_noun     cor,
                   const c3_c* tam_c);

      /* u3j_soft():
      **
      **   Execute hook from core, without jet.
      */
        u3_noun
        u3j_soft(u3_noun     cor,
                   const c3_c* tam_c);

      /* u3j_find(): battery to driver number, or 0.
      **
      ** `bat` is RETAINED by the caller.
      */
        c3_l
        u3j_find(u3_noun bat);

      /* u3j_kick(): try to kick by jet.  If no kick, produce u3_none.
      **
      ** `axe` is RETAINED by the caller; `cor` is RETAINED iff there 
      ** is no kick, TRANSFERRED if one.
      */
        u3_weak
        u3j_kick(u3_noun cor,
                   u3_noun axe);

      /* u3j_kink(): kick either by jet or by nock.
      */
        u3_noun
        u3j_kink(u3_noun cor,
                   u3_noun axe);
 
      /* u3j_mine(): register core for jets.
      */
        void
        u3j_mine(u3_noun clu,
                   u3_noun cor);

      /* u3j_ream(): refresh after restoring from checkpoint.
      */
        void
        u3j_ream(void);

      /* u3j_reap(): promote jet state.  RETAINS.
      */
        void
        u3j_reap(u3_noun das, u3p(u3h_root) har_p);
