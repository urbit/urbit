/* include/z/jato.h
**
** This file is in the public domain.
*/
  /** Data structures.
  **/
    /* u3_zj_code:
    **
    **   Jet codes.
    */
      enum u3_zj_code {
#       define _zj_wet(nam, mug) u3_zj_code_##nam,
#       define _zj_dry(nam, mug) u3_zj_code_##nam,
#         include "z/jets.h"
#       undef _zj_wet 
#       undef _zj_dry
        u3_zj_code_none
      };

    /* zj_def:
    **
    **   A jet definition.  Populated by installation.
    */
      struct u3_zj_def {
        /* Name of the jet.
        */
        const c3_c *nam;

        /* Battery mug - if 0, not known.
        */
        c3_w mug;

        /* Fun, or null.
        */
        u3_fox (*pas)(u3_z, u3_fox);

        /* Priority, as derived.
        */
        uint32_t pri;

        /* Battery, as installed.
        */
        u3_fox bat;

        /* Context, as installed.
        */
        u3_fox con;

        /* Next, in linear (XX) search list.
        */
        struct u3_zj_def *nex;
      };

  /** Functions.
  **/
    /** Called from main engine.
    **/
      /* u3_zj_load():
      **
      **   Load jet by prop and battery.
      */
        void
        u3_zj_load(u3_z   z,
                   u3_fox pup,
                   u3_fox bat);

      /* u3_zj_look():
      **
      **   Look for a jet match - gate, [[sam con] bat].
      */
        enum u3_zj_code 
        u3_zj_look(u3_z   z,
                   u3_fox bat);

      /* u3_zj_bat():
      **
      **   Return the bat formula for a jet.
      */
        u3_fox
        u3_zj_bat(u3_z            z,
                  enum u3_zj_code code_sax);

      /* u3_zj_fire():
      **
      **   Fire a jet - subject, [[sam con] bat].
      **
      **   Set *pod and/or return error condition:
      **
      **     0         : jet executed correctly
      **     u3_cm_exit: true exit detected
      **     u3_cm_fail: failure to compute
      **     u3_cm_punt: return to soft code
      */
        u3_mote
        u3_zj_fire(u3_z            z,
                   u3_fox          *pod,
                   enum u3_zj_code sax,
                   u3_fox          sub);


    /** C environment for jet programmers.  Ideally pleasurable.
    ***
    *** Jet programmers may use lr functions, but not lm or ln.
    *** Replace with appropriate zc.
    **/
      /** Macros.
      **/
#       define u3_zh(z, n)      u3_zc_use(z, u3_lr_h(z, n))
#       define u3_zt(z, n)      u3_zc_use(z, u3_lr_t(z, n))
#       define u3_zf(z, a, n)   u3_zc_use(z, u3_lr_twig(z, a, n))

      /** Administrative and miscellaneous.
      **/
        /* u3_zc_malloc():
        **
        **   Drop-in malloc() replacement.  Never returns 0.  Always
        **   allocates on the hat.
        */
          void *
          u3_zc_malloc(u3_z z,
                       c3_w w_led);

        /* u3_zc_depart():
        **
        **   Depart, moving allocation to the can, saving the mat.
        */
          u3_ray
          u3_zc_depart(u3_z z);

        /* u3_zc_retreat():
        **
        **   Retreat, restoring the mat.
        */
          void
          u3_zc_retreat(u3_z   z,
                        u3_ray ray_mat);
        
        /* u3_zc_zap():
        **
        **   Zap the can.
        */
          void
          u3_zc_zap(u3_z z);

        /* u3_zc_save():
        **
        **   Save a noun to the hat.
        */
          u3_fox
          u3_zc_save(u3_z   z,
                     u3_fox zab);

        /* u3_zc_tank():
        **
        **   Raise an unrecoverable exception.  Call with
        **
        **     u3_cm_exit: true exit detected
        **     u3_cm_fail: failure to compute
        **     u3_cm_punt: confused, return to soft code
        */
          u3_fox 
          u3_zc_tank(u3_z    z,
                     u3_mote gaz);

        /* u3_zc_use():
        **
        **   Exit iff (rat) is none.
        */
          u3_fox
          u3_zc_use(u3_z   z,
                    u3_rat rat);

      /** Jet and interpreter activation.
      **/
        /* u3_zc_fire():
        **
        **   Cross-fire a jet.
        */
          u3_fox
          u3_zc_fire(u3_z            z,
                     enum u3_zj_code code_sax,
                     u3_fox          sam);

        /* u3_zc_nock():
        **
        **   Run the interpreter inside itself.
        */
          u3_fox
          u3_zc_nock(u3_z   z,
                     u3_fox lan,
                     u3_fox sef);

      /** Internal map facility.
      **/
        /* u3_zc_map_add():
        **
        **   Add (key toy) to (map).  For a new map, pass 0.
        */
          struct u3_zc_map *
          u3_zc_map_add(u3_z             z,
                        struct u3_zc_map *map,
                        u3_fox           key,
                        u3_fox           toy);

        /* u3_zc_map_find():
        **
        **   Return the toy for (key), or u3_none if there is none such.
        */
          u3_rat
          u3_zc_map_find(u3_z             z,
                         struct u3_zc_map *map,
                         u3_fox           key);


      /** Noun construction.
      **/
        /* u3_zc_bytes():
        **
        **   Copy (w_a) bytes from (y_b) into an atom on the hat of (l).
        */
          u3_fox
          u3_zc_bytes(u3_z       z,
                      c3_w       w_a,
                      const c3_y *y_b);

        /* u3_zc_string():
        **
        **   u3_zc_bytes(z, strlen(c_a), (u3_y *)c_a);
        */
          u3_fox
          u3_zc_string(u3_z       z,
                       const c3_c *c_a);

        /* u3_zc_cell(): 
        **
        **   Produce the cell [a b] on the hat of (z).
        */
          u3_fox
          u3_zc_cell(u3_z   z,
                     u3_fox a,
                     u3_fox b);

        /* u3_zc_mp():
        **
        **   Copy the GMP integer (mp_a) into an atom on the hat of (z).
        */
          u3_fox
          u3_zc_mp(u3_z  z,
                   mpz_t mp_a);

        /* u3_zc_trel(): 
        **
        **   Produce the trel [a b c] on the hat of (z).
        */
          u3_fox
          u3_zc_trel(u3_z   z,
                     u3_fox a,
                     u3_fox b,
                     u3_fox c);

        /* u3_zc_words():
        **
        **   Copy (w_a) words from (w_b) into an atom on the hat of (z).
        */
          u3_fox
          u3_zc_words(u3_z       z,
                      c3_w       w_a,
                      const c3_w *w_b);
