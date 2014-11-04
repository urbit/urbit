/* include/f/jets.h
**
** This file is in the public domain.
*/
  /** Noun structures.
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
