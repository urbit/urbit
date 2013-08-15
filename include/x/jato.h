/* include/x/jato.h
**
** This file is in the public domain.
*/
  /** Data structures.
  **/
    /* u3_xj_code:
    **
    **   Jet codes.
    */
      enum u3_xj_code {
#       define _xj_wet(nam, mug) u3_xj_code_##nam,
#       define _xj_dry(nam, mug) u3_xj_code_##nam,
#         include "x/jets.h"
#       undef _xj_wet 
#       undef _xj_dry
        u3_xj_code_none
      };

    /* xj_def:
    **
    **   A jet definition.  Populated by installation.
    */
      struct u3_xj_def {
        /* Name of the jet.
        */
        const c3_c *nam;

        /* Battery mug - if 0, not known.
        */
        c3_w mug;

        /* Fun, or null.
        */
        u3_fox (*pas)(u3_x, u3_fox);

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
        struct u3_xj_def *nex;
      };

  /** Functions.
  **/
    /** Called from main engine.
    **/
      /* u3_xj_load():
      **
      **   Load jet by prop and battery.
      */
        void
        u3_xj_load(u3_x   x,
                   u3_fox pup,
                   u3_fox bat);

      /* u3_xj_look():
      **
      **   Look for a jet match - gate, [[sam con] bat].
      */
        enum u3_xj_code 
        u3_xj_look(u3_x   x,
                   u3_fox bat);

      /* u3_xj_bat():
      **
      **   Return the bat formula for a jet.
      */
        u3_fox
        u3_xj_bat(u3_x            x,
                  enum u3_xj_code code_sax);

      /* u3_xj_fire():
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
        u3_xj_fire(u3_x            x,
                   u3_fox          *pod,
                   enum u3_xj_code sax,
                   u3_fox          sub);


    /** C environment for jet programmers.  Ideally pleasurable.
    ***
    *** Jet programmers may use lr functions, but not lm or ln.
    *** Replace with appropriate xc.
    **/
      /** Macros.
      **/
#       define u3_xh(x, n)      u3_xc_use(x, u3_lr_h(x, n))
#       define u3_xt(x, n)      u3_xc_use(x, u3_lr_t(x, n))
#       define u3_xf(x, a, n)   u3_xc_use(x, u3_lr_twig(x, a, n))

      /** Administrative and miscellaneous.
      **/
        /* u3_xc_malloc():
        **
        **   Drop-in malloc() replacement.  Never returns 0.  Always
        **   allocates on the hat.
        */
          void *
          u3_xc_malloc(u3_x x,
                       c3_w w_led);

        /* u3_xc_depart():
        **
        **   Depart, moving allocation to the can, saving the mat.
        */
          u3_ray
          u3_xc_depart(u3_x x);

        /* u3_xc_retreat():
        **
        **   Retreat, restoring the mat.
        */
          void
          u3_xc_retreat(u3_x   x,
                        u3_ray ray_mat);
        
        /* u3_xc_xap():
        **
        **   Zap the can.
        */
          void
          u3_xc_xap(u3_x x);

        /* u3_xc_save():
        **
        **   Save a noun to the hat.
        */
          u3_fox
          u3_xc_save(u3_x   x,
                     u3_fox xab);

        /* u3_xc_tank():
        **
        **   Raise an unrecoverable exception.  Call with
        **
        **     u3_cm_exit: true exit detected
        **     u3_cm_fail: failure to compute
        **     u3_cm_punt: confused, return to soft code
        */
          u3_fox 
          u3_xc_tank(u3_x    x,
                     u3_mote gax);

        /* u3_xc_use():
        **
        **   Exit iff (rat) is none.
        */
          u3_fox
          u3_xc_use(u3_x   x,
                    u3_rat rat);

      /** Jet and interpreter activation.
      **/
        /* u3_xc_fire():
        **
        **   Cross-fire a jet.
        */
          u3_fox
          u3_xc_fire(u3_x            x,
                     enum u3_xj_code code_sax,
                     u3_fox          sam);

        /* u3_xc_nock():
        **
        **   Run the interpreter inside itself.
        */
          u3_fox
          u3_xc_nock(u3_x   x,
                     u3_fox lan,
                     u3_fox sef);

      /** Internal map facility.
      **/
        /* u3_xc_map_add():
        **
        **   Add (key toy) to (map).  For a new map, pass 0.
        */
          struct u3_xc_map *
          u3_xc_map_add(u3_x             x,
                        struct u3_xc_map *map,
                        u3_fox           key,
                        u3_fox           toy);

        /* u3_xc_map_find():
        **
        **   Return the toy for (key), or u3_none if there is none such.
        */
          u3_rat
          u3_xc_map_find(u3_x             x,
                         struct u3_xc_map *map,
                         u3_fox           key);


      /** Noun construction.
      **/
        /* u3_xc_bytes():
        **
        **   Copy (w_a) bytes from (y_b) into an atom on the hat of (l).
        */
          u3_fox
          u3_xc_bytes(u3_x       x,
                      c3_w       w_a,
                      const c3_y *y_b);

        /* u3_xc_string():
        **
        **   u3_xc_bytes(x, strlen(c_a), (u3_y *)c_a);
        */
          u3_fox
          u3_xc_string(u3_x       x,
                       const c3_c *c_a);

        /* u3_xc_cell(): 
        **
        **   Produce the cell [a b] on the hat of (x).
        */
          u3_fox
          u3_xc_cell(u3_x   x,
                     u3_fox a,
                     u3_fox b);

        /* u3_xc_mp():
        **
        **   Copy the GMP integer (mp_a) into an atom on the hat of (x).
        */
          u3_fox
          u3_xc_mp(u3_x  x,
                   mpz_t mp_a);

        /* u3_xc_trel(): 
        **
        **   Produce the trel [a b c] on the hat of (x).
        */
          u3_fox
          u3_xc_trel(u3_x   x,
                     u3_fox a,
                     u3_fox b,
                     u3_fox c);

        /* u3_xc_words():
        **
        **   Copy (w_a) words from (w_b) into an atom on the hat of (x).
        */
          u3_fox
          u3_xc_words(u3_x       x,
                      c3_w       w_a,
                      const c3_w *w_b);
