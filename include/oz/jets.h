/* include/z/jets.h
**
** This file is in the public domain.
**
** Prefixes:
**
**   u3_zj  (zeno jet)
**
** Description:
**
**   zeno: accelerated core.
*/
  /** Data types.
  **/
    /* zj_fun:
    **
    **   A jet function pointer.
    **
    **     cob: argument
    **     van: context
    **
    **   A gate being [[cob van] axe].
    */
      typedef u3_fox (*u3_zj_fun)(u3_z   z,
                                  u3_fox cob,
                                  u3_fox van);
    /* zj_spec: 
    **
    **   A gate specification.  Used at load time and not again.
    */
      struct u3_zj_gate {
        /* c_src: watt source for gate (eg, "add").
        ** y_priority: priority group, 0-16.
        */
        const u3_c_c *c_src;
        u3_c_y       y_priority;
      };

    /* zj_jet: 
    **
    **    jet definition.
    */
      struct u3_zj_jet {
        /* mab: C function
        */
        u3_zj_fun 
      
    /** Miscellaneous internal data.
    **/
      /* z_pair, z_trep, z_quap:
      **
      **   Short tuples at the zeno level.
      */
        struct u3_z_pair { u3_fox p, q; };
        struct u3_z_trep { u3_fox p, q, r; };
        struct u3_z_quap { u3_fox p, q, r, s; };

      /* z_set:
      **
      **   A noun set, as a binary tree mug-ordered on (p).
      */
        struct u3_z_set {
          /* p: set element
          ** q: null, or element < p
          ** r: null, or element > p
          */
          u3_fox          p;
          struct u3_z_set *zs_q;
          struct u3_z_set *zs_r;
        };

      /* z_tab:
      **
      **   A noun table, as a binary tree mug-ordered on (zp_p.p).
      */
        struct u3_z_tab {
          /* p: set element
          ** q: null, or element < p
          ** r: null, or element > p
          */
          struct u3_z_pair zp_p;
          struct u3_z_tab  *zt_q;
          struct u3_z_tab  *zt_r;
        };

    /** Jet optimization.
    **/
      /* z_plug:
      **
      **   A jet table, as a binary tree mug_ordered on (axe).
      */
        struct u3_z_plug {
          /* axe: formula
          ** van: context
          ** jet: function pointer
          ** dur: null, or plug < axe
          ** hol: null, or plug > axe
          */
          u3_fox           axe;
          u3_fox           van;
          u3_z_jet         jet;
          struct u3_z_plug *dur;
          struct u3_z_plug *hol;
        };

    /* z_core, z: 
    **
    **   The zeno core.
    */
      struct u3_z_core {
        struct u3_q_core q;

        /* lum: jet exception target
        ** pol: jet table [trusted]
        ** fad: jet table [practice]
        */
        jmp_buf          jmp_lum;
        struct u3_z_plug *zs_pol;
        struct u3_z_plug *zs_fad;
      };
      typedef struct u3_z_core *u3_z;


  /** Functions.
  **/
    /** External.
    ***
    *** zeno looks just like quat - but remember to use u3_z_step and
    *** u3_z_spin if you want actual performance.
    ***
    *** Otherwise, 
    **/
      /* u3_z_new():
      **
      **   Create a zeno engine, mallocing (1 << y_a) words of memory.
      **   Return 0 if malloc fails.  Free with free().
      **
      **   Rely only on jets of priority <= (y_b), testing all others.
      **   y_b is 0 for full nock, 15 for full speed.
      */
        u3_z
        u3_z_new(u3_y y_a,
                 u3_y y_b);

      /* u3_z_spin():
      **
      **   As u3_q_step(), until complete.
      */
        u3_fox
        u3_z_spin(u3_z z);

      /* u3_z_step():
      **
      **   Step (z).
      */ 
        u3_q_stat
        u3_z_step(u3_z z);


  /** Internal.
  **/
