/* include/mill/mill.h
**
** This file is in the public domain.
*/
  /** Data molds.
  **/
    /* Some kinds of nouns.
    */
      typedef u4_noun u4_lump;  //  *
      typedef u4_noun u4_gene;  //  see _mill_gene()
      typedef u4_atom u4_axis;  //  @
      typedef u4_atom u4_term;  //  @ 
      typedef u4_atom u4_mark;  //  @ 
      typedef u4_atom u4_bead;  //  @ 
      typedef u4_atom u4_tick;  //  @
      typedef u4_noun u4_herb;  //  [mold gene]
      typedef u4_noun u4_pike;  //  see _mill_gene_pike()
      typedef u4_noun u4_spec;  //  lambda spec
      typedef u4_noun u4_form;  //  pattern skeleton
      typedef u4_noun u4_bolo;  //  {list pike}
      typedef u4_noun u4_plox;  //  {list glep}
      typedef u4_noun u4_glep;  //  [tape mold]
      typedef u4_noun u4_curb;  //  [axis term]
      typedef u4_noun u4_loaf;  //  [mold nock]
      typedef u4_noun u4_wire;  //  [mold nock]
      typedef u4_noun u4_rail;  //  {list mold}
      typedef u4_noun u4_knot;  //  |(  (term) 
                                //      [%zarb @] 
                                //      [%lect (tick) (term)]
                                //   )
      typedef u4_noun u4_bait;  //  |(  (%zarb @)
                                //      [%lect @ (tick) (term)]
                                //  )
      typedef u4_noun u4_lure;  //  {list bait}
      typedef u4_noun u4_rope;  //  {list knot}
      typedef u4_noun u4_nail;  //  [rope gene] 
      typedef u4_noun u4_tack;  //  |([%axis axis] [%term term])
      typedef u4_noun u4_weld;  //  [axis nock]
      typedef u4_noun u4_bolt;  //  {list nail}
      typedef u4_noun u4_tape;  //  {list tack}
      typedef u4_noun u4_belt;  //  {list weld}
      typedef u4_noun u4_carb;  //  [axis gene]
      typedef u4_noun u4_hair;  //  [term mold]
      typedef u4_noun u4_cope;  //  {list hair}
      typedef u4_noun u4_nock;  //  [nockula]
      typedef u4_noun u4_book;  //  {bush gene}

      typedef u4_noun u4_mold;

    /* The main mill structure.
    */
      typedef struct _u4_mill {
        /* For all internal allocation.
        */
        u4_lane lane;

        /* Internal counter for perf testing.
        */
        uint32_t prf;

        /* Set (mold gene) currently in replay (aka repo).
        */
        u4_bag fan;

        /* Set (mold mold gene) currently in fab (_edit_cone_fab_gene).
        */
        u4_bag pox;

        /* Debug depth.
        */
        u4_atom rux;

        /* Site (source package being compiled, in top-level safe)
        */
        u4_noun zud;

        /* Spot (position in source).
        */
        u4_noun nix;

        /* 0 iff we are producing mold.
        */
        u4_atom ply;

        /* 0 iff we are producing nockula.
        */
        u4_atom bak;

        /* Trap - ((list {[p=(text) q=(site) r=(spot)]}))
        */
        u4_noun meb;

        /* Tab to memoize _mill_bake().
        */
        u4_tab niq;

        /* Tab to memoize _mill_play().
        */
        u4_tab zor;

        /* Tab [mold flag] to memoize _mill_null().
        */
        u4_tab dam;

        /* Tab [[rail mold mold] flag] to memoize _mill_nest().
        */
        u4_tab vus;

        /* Tab [[mold gene] mold] to memoize _mill_repo().
        */
        u4_tab rep;

        /* Tab [gene gene] to memoize _mill_open().
        */
        u4_tab pon;

        /* Tab [[mark rail typ] loaf] to memoize _mill_find().
        */
        u4_tab fin;
      } 
        *u4_milr;


  /** Functions.
  **/
    /** Public interfaces.
    **/
      /* u4_mill(): 
      **
      **   Convert (mold gene) to (mold nock).
      */
        u4_wire
        u4_mill(u4_lane lane,
                u4_mold nes,
                u4_gene zyl);

      /* u4_mill_init(): 
      **
      **   Initialize (m) with (lane).
      */
        void
        u4_mill_init(u4_milr m,
                     u4_lane lane);


    /** Private functions.
    **/
      /* _mill_and(): make and.
      */
        u4_nock
        _mill_and(u4_milr m,
                  u4_nock zeg,
                  u4_nock dac);

      /* _mill_or(): make or.
      */
        u4_nock
        _mill_or(u4_milr m,
                 u4_nock bos,
                 u4_nock nif);

      /* _mill_bake(): mill and discard mold.
      */
        u4_nock
        _mill_bake(u4_milr m, 
                   u4_mold tip,
                   u4_gene gen);

      /* _mill_both(): fuse pair.
      */
        u4_nock
        _mill_both(u4_milr m,
                   u4_mold nem,
                   u4_mold dif);

      /* _mill_comp(): compose a pipe.
      */
        u4_nock
        _mill_comp(u4_milr m,
                   u4_nock mal,
                   u4_nock buz);

      /* _mill_cong(): true if every tip is a gan.
      */
        u4_t
        _mill_cong(u4_milr m,
                   u4_mold gan,
                   u4_mold tip);

      /* _mill_cons(): construct cell.
      */
        u4_nock
        _mill_cons(u4_milr m,
                   u4_nock vor,
                   u4_nock sed);

      /* _mill_cook(): cook a gene, producing nockula and mold.
      */
        u4_mold
        _mill_cook(u4_milr m, 
                   u4_gene gen,
                   u4_mold tip);

      /* _mill_cull(): prune for computation.
      */
        u4_t
        _mill_cull(u4_milr m,
                   u4_rail bar,
                   u4_mold tip);

      /* _mill_dump(): prepare mold for printing.
      */
        u4_prep
        _mill_dump(u4_milr m,
                   u4_mold tip);

      /* _mill_durb(): prepare rail for printing.
      */
        u4_prep
        _mill_durb(u4_milr m,
                   u4_rail bar);

      /* _mill_edit(): edit a mold to reflect a list of writes.
      */
        u4_mold
        _mill_edit(u4_milr m,
                   u4_plox zel,
                   u4_rail bar,
                   u4_mold gom);

      /* _mill_eith(): fork pair.
      */
        u4_mold
        _mill_eith(u4_milr m,
                   u4_mold lef,
                   u4_mold gap);

      /* _mill_fail(): failage.
      */
        u4_noun
        _mill_fail(u4_milr m,
                   const u4_cl *cl_msg);

      /* _mill_find(): look by name.
      */
        u4_wire
        _mill_find(u4_milr m,
                   u4_term cox,
                   u4_rail bar,
                   u4_mold tip);

      /* _mill_fire(): fire [tel hum] as (gate arg).
      */
        u4_nock
        _mill_fire(u4_milr m,
                   u4_nock tel,
                   u4_nock hum);

      /* _mill_fish(): test nock.
      */
        u4_nock
        _mill_fish(u4_milr m,
                   u4_axis axe,
                   u4_mold tip);

      /* _mill_gate(): gate from form.
      */
        u4_gene
        _mill_gate(u4_milr m,
                   u4_form kel);

      /* _mill_grip(): generate match gene. 
      */
        u4_gene
        _mill_grip(u4_milr m,
                   u4_form kel);

      /* _mill_hike(): mutate nockula.
      */
        u4_nock
        _mill_hike(u4_milr m,
                   u4_axis axe,
                   u4_belt vix);

      /* _mill_hunt(): rope to tape.
      */
        u4_tape
        _mill_hunt(u4_milr m,
                   u4_rope dap,
                   u4_mold fim,
                   u4_axis *axe,
                   u4_mold *buv);

      /* _mill_hunt(): analyze branch test.
      */
        u4_noun
        _mill_test(u4_milr m,
                   u4_mold tip,
                   u4_gene gen);

      /* _mill_look(): read on a rope.
      */
        u4_wire
        _mill_look(u4_milr m,
                   u4_rope fes,
                   u4_mold tip);

      /* _mill_lump(): generate direct form.
      */
        u4_gene
        _mill_lump(u4_milr m,
                   u4_form kel);

      /* _mill_make(): mold inference, top level.
      */
        u4_wire
        _mill_make(u4_milr m,
                   u4_gene gen,
                   u4_mold tip);

      /* _mill_nest(): test geometric congruence.
      **
      **    [gan] is geometrically congruent with [tip] iff 
      **    every noun in [gan] is also in [tip].
      */
        u4_t
        _mill_nest(u4_milr m,
                   u4_mold gan,
                   u4_mold tip);

      /* _mill_not(): invert boolean.
      */
        u4_mold
        _mill_not(u4_milr m,
                  u4_nock zet);

      /* _mill_null(): true if mold is empty.
      */
        u4_t
        _mill_null(u4_milr m,
                   u4_mold tip);

      /* _mill_orth(): orthogonality.
      */
        u4_t
        _mill_orth(u4_milr m,
                   u4_mold tip,
                   u4_mold gan);

      /* _mill_open(): open macro gene.
      */
        u4_gene
        _mill_open(u4_milr m,
                   u4_noun nim);

      /* _mill_peek(): cut a railed mold.
      */
        u4_mold
        _mill_peek(u4_milr m,
                   u4_axis axe,
                   u4_rail bar,
                   u4_mold tip);

      /* _mill_pike: open pike.  *tes is the test, *bif is yes.
      */
        void
        _mill_pike(u4_milr m,
                   u4_rope mox, 
                   u4_pike pik,
                   u4_gene *tes,
                   u4_gene *bif);

      /* _mill_play(): mill and discard nockula.
      */
        u4_mold
        _mill_play(u4_milr m, 
                   u4_mold tip,
                   u4_gene gen);

      /* _mill_reap(): expand mold.
      */
        u4_mold
        _mill_reap(u4_milr m,
                   u4_mold tip);

      /* _mill_repo(): replay post.
      */
        u4_mold
        _mill_repo(u4_milr m,
                   u4_mold tip,
                   u4_gene gen);

      /* _mill_salt(): modify a change target.
      **
      **    suc: changes:  bolt
      **    pex: subject:  mold
      **    fuz: target:   mold
      **    ped: fragment: axis
      */
        u4_loaf
        _mill_salt(u4_milr m,
                   u4_bolt suc,
                   u4_mold pex,
                   u4_mold fuz,
                   u4_axis ped);

      /* _mill_seal(): produce set of holds sealing [typ].
      */
        u4_bag
        _mill_seal(u4_milr m,
                   u4_mold typ);

      /* _mill_slip(): hack entire bar.
      */
        u4_mold
        _mill_slip(u4_milr m,
                   u4_axis axe,
                   u4_rail bar);

      /* _mill_snap(): adjust [ger] for assignment into [lom].
      */
        u4_mold
        _mill_snap(u4_milr m,
                   u4_mold lom,
                   u4_mold ger);

      /* _mill_trap(): save trap.
      */
        void
        _mill_trap(u4_milr m,
                   const u4_cl *cl_msg);
