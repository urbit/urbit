/* include/f/jets.h
**
** This file is in the public domain.
*/
  /** Data structures.
  **/
    /* u3_cs_hook: core map from hint.
    */
      typedef struct _u3_cs_hook {
        c3_c*               nam_c;
        c3_l                axe_l;
        struct _u3_cs_hook* nex_u;
      } u3_cs_hook;

    /* u3_cs_hood: battery as instance of core.
    */
      typedef struct _u3_cs_hood {
        c3_l                 mug_l;     //  battery mug
        c3_w                 len_w;     //  dynamic array length
        struct _u3_cs_harm** ray_u;     //  dynamic array by axis
        struct _u3_cs_hook*  huk_u;     //  hooks if any
        struct _u3_cs_hood*  nex_u;     //  next in this core
      } u3_cs_hood;

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
        struct _u3_cs_hood* hud_u;      //  dynamic instance list
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
