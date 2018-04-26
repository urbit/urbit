/* include/n/j.h
**
** This file is in the public domain.
*/
  /** Noun semantics.
  **/
#if 0
  ++  bane  ,@tas                                         ::  battery name
  ++  bash  ,@uvH                                         ::  label hash
  ++  bosh  ,@uvH                                         ::  battery hash
  ++  batt  ,*                                            ::  battery
  ++  calf                                                ::  
    $:  jax=,@ud                                          ::  hot core index
        hap=(map ,@ud ,@ud)                               ::  axis/hot arm index
        lab=path                                          ::  label as path
        jit=*                                             ::  arbitrary data
    ==                                                    ::
  ++  calx  (trel calf (pair bash cope) club)             ::  cached by battery
  ++  clog  (pair cope (map batt club))                   ::  label record
  ++  club  (pair corp (map term nock))                   ::  battery pattern
  ++  cope  (trel bane axis (each bash noun))             ::  core pattern
  ++  core  ,*
  ++  corp  (each core batt)                              ::  parent or static
  ++  dash  (map bash clog)                               ::  jet system
#endif

  /** Data structures.
  ***
  *** All of these are transient structures allocated with malloc.
  **/
    /* u3j_harm: jet arm.
    */
      typedef struct _u3j_harm {
        c3_c*             fcs_c;               //  `.axe` or name
        u3_noun           (*fun_f)(u3_noun);   //  compute or 0 / semitransfer
        // c3_o           (*val_f)(u3_noun);   //  validate or 0 / retain
        c3_o              ice;                 //  perfect (don't test)
        c3_o              tot;                 //  total (never punts)
        c3_o              liv;                 //  live (enabled)
        c3_l              axe_l;               //  computed/discovered axis
        struct _u3j_core* cop_u;               //  containing core
      } u3j_harm;

    /* u3j_core: driver definition.
    */
      typedef struct _u3j_core {
        c3_c*             cos_c;        //  control string
        struct _u3j_harm* arm_u;        //  blank-terminated static list
        struct _u3j_core* dev_u;        //  blank-terminated static list
        struct _u3j_core* par_u;        //  dynamic parent pointer 
        c3_l              axe_l;        //  axis to parent
        c3_l              jax_l;        //  index in global dashboard
      } u3j_core;

    /* u3e_dash, u3_Dash, u3D: jet dashboard singleton
    */
      typedef struct _u3e_dash {
        u3j_core* dev_u;              //  null-terminated static list
        c3_l      len_l;              //  dynamic array length
        c3_l      all_l;              //  allocated length
        u3j_core* ray_u;              //  dynamic array by axis
      } u3j_dash;

      typedef struct {
        u3_noun bat;
        u3_noun pax;
      } u3j_fist;

      typedef struct {
        c3_w    len_w;
        u3_noun sat;
        u3j_fist fis_u[0];
      } u3j_fink;

      typedef struct {
        c3_o      own_o;
        u3_weak   clu;
        u3j_fink* fin_u;
      } u3j_rite;

      struct _u3n_prog;
      typedef struct {
        struct _u3n_prog *pog_u;
        u3_noun   axe;
        u3_weak   bat;
        u3_weak   loc;
        c3_o      jet_o;
        c3_o      fon_o;
        u3_weak   lab;
        u3j_core* cop_u;
        u3j_harm* ham_u;
        u3j_fink* fin_u;
      } u3j_site;

  /** Globals.
  **/
    /* u3_Dash: jet dashboard.
    */
      extern u3j_dash u3j_Dash;
#     define u3D u3j_Dash

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

      /* u3j_spot():
      **
      **   Identify `cor`s location. RETAIN.
      */
        u3_weak
        u3j_spot(u3_noun cor);

      /* u3j_fine(): check core against u3j_fink.
       */
        c3_o
        u3j_fine(u3_noun cor, u3j_fink* fin_u);

      /* u3j_cast(): create u3j_fink from core and location.
       */
        u3j_fink*
        u3j_cast(u3_noun cor, u3_noun loc);

      /* u3j_nail(): resolve hot state for location and axis. RETAIN.
      **             return value indicates presence of driver.
      **/
        c3_o
        u3j_nail(u3_noun loc, u3_noun axe,
                 u3_noun* lab, u3j_core** cop_u, u3j_harm** ham_u);

      /* u3j_kick_hot(): Try to kick by jet with resolved hot state.
      **                 If no kick, produce u3_none.
      **
      ** `cor` is RETAINED iff there is no kick, TRANSFERRED if one.
      ** `axe` is RETAINED.
      */
        u3_weak
        u3j_hock(u3_noun cor,
                 u3j_core* cop_u,
                 u3j_harm* ham_u,
                 u3_atom axe);

      /* u3j_kick(): try to kick by jet.  If no kick, produce u3_none.
      **
      ** `axe` is RETAINED by the caller; `cor` is RETAINED iff there 
      ** is no kick, TRANSFERRED if one.
      */
        u3_weak
        u3j_kick(u3_noun cor, u3_noun axe);

      /* u3j_kink(): kick either by jet or by nock.
      */
        u3_noun
        u3j_kink(u3_noun cor,
                   u3_noun axe);

      /* u3j_mile(): register core for jets, returning location.
      */
        u3_weak
        u3j_mile(u3_noun clu, u3_noun cor);

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
        u3j_reap(u3p(u3h_root) cod_p, u3p(u3h_root) war_p);

      /* u3j_rite_mark(): mark u3j_rite for gc.
      */
        c3_w
        u3j_rite_mark(u3j_rite* rit_u);

      /* u3j_site_mark(): mark u3j_site for gc.
      */
        c3_w
        u3j_site_mark(u3j_site* sit_u);
