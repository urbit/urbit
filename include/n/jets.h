/* include/f/jets.h
**
** This file is in the public domain.
*/
  /** Noun specifications.
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
        huc=(map ,@tas nock)                              ::  name/tool
    ==                                                    ::
  ++  dash                                                ::  jet engine
    $:  sys=(map batt bash)                               ::  battery/identity
        haw=(map bash cope)                               ::  identity/core
    ==                                                    ::
  ++  cope                                                ::  core pattern
    $:  soh=bash                                          ::  identity
        hud=(map batt (map ,@tas nock))                   ::  instances
        mop=(trel bane axis (each bash ,*))               ::  ancestry
    ==                                                    ::  
  ++  je   !:                                             ::  dashboard door
    |_  dash
    ++  fill                                              ::  validated match
      |=  cor=* 
      ^-  (unit bash)
      %+  biff  (find cor)
      |=  soh=bash
      ^-  (unit bash)
      ?.((fine cor (~(got by haw) soh)) ~ `soh)
    ::
    ++  find                                              ::  simple match
      |=  cor=* 
      `(unit bash)`?@(cor ~ (~(get by sys) -.cor))
    ::
    ++  fine                                              ::  validate context
      |=  [cor=* coe=cope]
      ^-  ?
      =+  rah=.*(cor [0 q.mop.coe])
      ?-  -.r.mop.coe
        |  =(rah p.r.mop.coe)
        &  $(cor rah, coe (~(got by haw) p.r.mop.coe))
      ==
    ::
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
    ++  fuel                                              ::  attach battery
      |=  [bat=* coe=cope]
      %_  +>
        sys  (~(put by sys) bat soh.coe)
        haw  (~(put by haw) soh.coe coe)
      ==
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
      =+  cup=(~(get by haw) soh)
      ?^  cup
        %=  +>.$
          sys  (~(put by sys) -.cor soh))
          haw  (~(put by haw) soh u.cup(hud (~(put by hud.u.cup) -.cor r.cey)))
        ==
      (fuel -.cor `cope`[soh [[-.cor r.cey] ~ ~] mop])
    -- 
#endif
  /** Data structures.
  ***
  *** All of these are transient structures allocated with malloc.
  **/
    /* u3_cs_harm: jet arm.
    */
      typedef struct _u3_cs_harm {
        c3_c*               fcs_c;             //  `.axe` or name
        u3_noun           (*fun_f)(u3_noun);   //  compute or 0 / semitransfer
        // u3_bean           (*val_f)(u3_noun);   //  validate or 0 / retain
        c3_o                ice;               //  perfect (don't test)
        c3_o                tot;               //  total (never punts)
        c3_o                liv;               //  live (enabled)
        c3_l                axe_l;             //  computed/discovered axis
        struct _u3_cs_core* cop_u;             //  containing core
      } u3_cs_harm;

    /* u3_cs_core: driver definition.
    */
      typedef struct _u3_cs_core {
        c3_c*               cos_c;      //  control string
        struct _u3_cs_harm* arm_u;      //  blank-terminated static list
        struct _u3_cs_core* dev_u;      //  blank-terminated static list
        struct _u3_cs_core* par_u;      //  dynamic parent pointer 
        c3_l                axe_l;      //  axis to parent
        c3_l                jax_l;      //  index in global dashboard
      } u3_cs_core;

    /* u3_cs_dash, u3_Dash, u3D: jet dashboard singleton
    */
      typedef struct _u3_cs_dash {
        u3_cs_core* dev_u;              //  null-terminated static list
        c3_l        len_l;              //  dynamic array length
        c3_l        all_l;              //  allocated length
        u3_cs_core* ray_u;              //  dynamic array by axis
      } u3_cs_dash;
