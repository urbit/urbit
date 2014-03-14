/* include/trac.h
**
** This file is in the public domain.
*/
  /** Data structures.
  **/
    /* u2_loom_marx: profile watermark.
    */
      struct u2_loom_marx {
        /* Measure.
        */
        c3_w med_w;

        /* Maximum.
        */
        c3_w max_w;
      };

    /* u2_loom_knot: profile node.
    */
      typedef struct _u2_loom_knot {
        /* Task name - 31-byte prefix.
        */
        c3_c lic_c[32];

        /* Number of hits in this task.
        */
        c3_w fin_w;

        /* Subtask list, if any.
        */
        struct _u2_loom_knot *fam_k;
        struct _u2_loom_knot *nex_k;
      } u2_loom_knot;

    /* u2_loom_trac: tracing/profiling control structure.
    */
      typedef struct _u2_loom_trac {
        /* Control.
        */
          struct {
            /* u2_yes iff debugging is on.
            */
            u2_bean deb;

            /* u2_yes iff profiling is on.
            */
            u2_bean pro;
          } cor;

        /* Tracing.
        */
          struct {
            /* Position stack: *(list shoe)  [XX not used, still wire->tax]
            */
            u2_noun ryp;

            /* Mode bit - u2_yes == C/system, u2_no == interpreted
            */
            u2_bean sys;

            /* Function bit for sys - u2_yes == overhead, u2_no == function
            */
            u2_bean glu;

            /* Number of samples in random C code.
            */
            c3_d com_d;

            /* Number of samples in jet code.
            */
            c3_d jet_d;

            /* Number of samples in interpreted code.
            */
            c3_d erp_d;
          } wer;

        /* Profiling.
        */
          struct {
            /* Task stack: *(list term)
            */
            u2_noun don;

            /* Act count: *(map term num)
            */
            u2_noun cot;
          } duz;

        /* Built-in system acts and counters.
        */
          struct {
            /* Nock reductions.
            */
            c3_d hop_d;

            /* Jet activations.
            */
            c3_d jet_d;

            /* Jet tests.
            */
            c3_d tes_d;

            /* Matching comparisons.
            */
            c3_d nod_d;

            /* Memoization searches.
            */
            c3_d fin_d;

            /* Memoization productions.
            */
            c3_d pod_d;

            /* C stack record.
            */
            struct u2_loom_marx cas_x;

            /* Main memory usage record.
            */
            struct u2_loom_marx men_x;

            /* Basket memory usage record.
            */
            struct u2_loom_marx bek_x;

            /* Unix time in seconds at analysis instantiation.
            */
            c3_w sec_w;

            /* Unix time in microseconds at analysis instantiation.
            */
            c3_w usc_w;

            /* Original words in wire.
            */
            c3_w lif_w;

            /* Original words in basket.
            */
            c3_w bos_w;
          } sys;
      } u2_loom_trac;

#define   u2_trac_at(rac_r, wof)        (*u2_at(rac_r, u2_loom_trac, wof))
#define   u2_trac_be(rac_r, ite, wof)   (*u2_be(rac_r, u2_loom_trac, ite, wof))

#define   u2_wrac_at(wir_r, wof)      u2_trac_at(u2_wire_rac_r(wir_r), wof)
#define   u2_wrac_be(wir_r, ite, wof) u2_trac_be(u2_wire_rac_r(wir_r), ite, wof)

  /** Functions.
  **/
    /** Lifecycle.
    **/
      /* u2_tx_init(): initialize state.
      */
        u2_ray
        u2_tx_init(u2_ray wir_r);

      /* u2_tx_done(): produce a profile slab to render.  Close tracing.
      **
      ** type:
      */
        u2_noun                                                   //  produce
        u2_tx_done(u2_ray wir_r);

      /* u2_tx_open(): open tracing.
      */
        void
        u2_tx_open(u2_ray wir_r);

      /* u2_tx_do_*(): set debug/profile bean.  Return old value.
      */
        u2_bean u2_tx_do_debug(u2_ray wir_r, u2_bean lag);
        u2_bean u2_tx_do_profile(u2_ray wir_r, u2_bean lag);

      /* u2_tx_in_*(): get debug/profile bean.
      */
        u2_bean u2_tx_in_debug(u2_ray wir_r);
        u2_bean u2_tx_in_profile(u2_ray wir_r);

    /** Actions.
    **/
      /* u2_tx_did_*(): record system actions.
      */
#if 0
#       define u2_tx_did(wir_r, wof, det_ws)  \
            ( u2_wrac_be(wir_r, c3_d, wof) += det_ws )
#else
#       define u2_tx_did(wir_r, wof, det_ws)
#endif

#       define u2_tx_did_hop(wir_r, det_ws) u2_tx_did(wir_r, sys.hop_d, det_ws)
#       define u2_tx_did_jet(wir_r, det_ws) u2_tx_did(wir_r, sys.jet_d, det_ws)
#       define u2_tx_did_tes(wir_r, det_ws) u2_tx_did(wir_r, sys.tes_d, det_ws)
#       define u2_tx_did_nod(wir_r, det_ws) u2_tx_did(wir_r, sys.nod_d, det_ws)
#       define u2_tx_did_fin(wir_r, det_ws) u2_tx_did(wir_r, sys.fin_d, det_ws)
#       define u2_tx_did_pod(wir_r, det_ws) u2_tx_did(wir_r, sys.pod_d, det_ws)

      /* u2_tx_mex*(): record signed change in watermarks.
      */
#       define u2_tx_mex(wir_r, wof, det_ws) \
          ( ( u2_wrac_at(wir_r, wof.med_w) += det_ws), \
            ( u2_wrac_at(wir_r, wof.max_w) = \
                ( u2_wrac_at(wir_r, wof.med_w) > \
                  u2_wrac_at(wir_r, wof.max_w) ) \
                ? u2_wrac_at(wir_r, wof.med_w) \
                : u2_wrac_at(wir_r, wof.max_w) ) )

#       define u2_tx_add_cas(wir_r, det_ws) u2_tx_mex(wir_r, sys.cas_x, det_ws)
#       define u2_tx_add_men(wir_r, det_ws) u2_tx_mex(wir_r, sys.men_x, det_ws)
#       define u2_tx_add_bek(wir_r, det_ws) u2_tx_mex(wir_r, sys.bek_x, det_ws)

#       define u2_tx_sink_cas(wir_r) u2_tx_add_cas(wir_r, 1)
#       define u2_tx_rise_cas(wir_r) u2_tx_add_cas(wir_r, -1)

      /* u2_tx_add_mem(): add memory to rail.  A hack.  Not used.
      */
#       define u2_tx_add_mem(ral_r, det_ws) \
          ( (0 == ral_r) ? u2_tx_add_men(ral_r, det_ws) \
                         : u2_tx_add_bek(0, det_ws)

      /* u2_tx_did_act(): record user actions.
      */
        void
        u2_tx_did_act(u2_ray  wir_r,
                      u2_noun did);                               //  retain

      /* u2_tx_sys_bit(): set system bit, returning old value.
      */
        u2_bean
        u2_tx_sys_bit(u2_ray  wir_r,
                      u2_bean val);

      /* u2_tx_glu_bit(): set glue bit within system bit.
      */
        u2_bean
        u2_tx_glu_bit(u2_ray wir_r,
                      u2_bean val);
    /** Tasks.
    **/
      /* u2_tx_task_in(): enter a task for profiling purposes.
      **
      ** u2_yes iff the task is not already in the stack.
      */
        u2_bean
        u2_tx_task_in(u2_ray  wir_r,
                      u2_noun tak);                               //  retain

      /* u2_tx_task_out(): leave a task for profiling purposes.
      */
        void
        u2_tx_task_out(u2_ray  wir_r);

    /** Direct logging.
    **/
      /* u2_tx_slog(): print debug syslog [0-3 tank] 0=debug 3=alarm
      */
        void
        u2_tx_slog(u2_ray  wir_r,
                   u2_noun luf);                                   //  retain

      /* u2_tx_warn(): report a warning by internal file and line.
      */
        void
        u2_tx_warn(u2_ray      wir_r,
                   const c3_c* fil_c,
                   c3_w        lyn_w);
#       define u2_tx_warn_here(wir_r) u2_bx_warn(wir_r, __FILE__, __LINE__)

